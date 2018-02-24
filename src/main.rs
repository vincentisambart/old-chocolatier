#![allow(dead_code)]

extern crate clang;
extern crate tempfile;

// TODO:
// - Do not forget the namespace support.
// - Maybe do something for 32/64-bit differences.
// - Maybe handle lightweight generics.
// - For args, look for const/inout/out, and nullable/nonnull...
// - Maybe convert ObjC errors (and/or exceptions) to Rust errors

use clang::{Clang, Entity, EntityKind, EntityVisitResult, Index};
use clang::diagnostic::Severity;
use std::process::Command;

#[derive(Copy, Clone)]
enum AppleSdk {
    MacOs,
    IOs,
    IOsSimulator,
    TvOs,
    TvOsSimulator,
    WatchOs,
    WatchOsSimulator,
}

impl AppleSdk {
    fn sdk_name(&self) -> &str {
        match *self {
            AppleSdk::MacOs => "macosx",
            AppleSdk::IOs => "iphoneos",
            AppleSdk::IOsSimulator => "iphonesimulator",
            AppleSdk::TvOs => "appletvos",
            AppleSdk::TvOsSimulator => "appletvsimulator",
            AppleSdk::WatchOs => "watchos",
            AppleSdk::WatchOsSimulator => "watchsimulator",
        }
    }
}

fn sdk_path(sdk: AppleSdk) -> String {
    let output = Command::new("xcrun")
        .args(&["--sdk", sdk.sdk_name(), "--show-sdk-path"])
        .output()
        .expect("xcrun command failed to start");
    assert!(output.status.success());
    String::from_utf8_lossy(&output.stdout).trim().into()
}

#[derive(Debug)]
enum ParseError {
    SourceError(clang::SourceError),
    CompilationError(String),
}

impl From<clang::SourceError> for ParseError {
    fn from(err: clang::SourceError) -> ParseError {
        ParseError::SourceError(err)
    }
}

#[derive(Debug, PartialEq)]
enum ObjCType {
    Void,
}

impl ObjCType {
    fn from(clang_type: &clang::Type) -> ObjCType {
        let kind = clang_type.get_kind();
        match kind {
            clang::TypeKind::Typedef => ObjCType::from(&clang_type.get_canonical_type()),
            _ => {
                println!("Unimplement type {:?}", kind);
                ObjCType::Void
            }
        }
    }
}

#[derive(Debug, PartialEq)]
struct ObjCMethodArg {
    name: String,
    objc_type: ObjCType,
}

impl ObjCMethodArg {
    fn from(entity: &Entity) -> ObjCMethodArg {
        assert!(entity.get_kind() == EntityKind::ParmDecl);
        if entity.get_type().unwrap().get_kind() == clang::TypeKind::Unexposed {
            println!("unexposed entity: {:?}", entity);
        }
        ObjCMethodArg {
            name: entity.get_name().unwrap(),
            objc_type: ObjCType::from(&entity.get_type().unwrap()),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ObjCMethodKind {
    InstanceMethod,
    ClassMethod,
}

#[derive(Debug, PartialEq)]
struct ObjCMethod {
    kind: ObjCMethodKind,
    sel: String,
    args: Vec<ObjCMethodArg>,
    ret_type: ObjCType,
}

impl ObjCMethod {
    fn from(entity: &Entity) -> ObjCMethod {
        assert!(
            entity.get_kind() == EntityKind::ObjCInstanceMethodDecl
                || entity.get_kind() == EntityKind::ObjCClassMethodDecl
        );
        let arg_entities = entity.get_arguments().unwrap();
        let args = arg_entities
            .iter()
            .map(|arg_entity| ObjCMethodArg::from(arg_entity));
        let kind = match entity.get_kind() {
            EntityKind::ObjCInstanceMethodDecl => ObjCMethodKind::InstanceMethod,
            EntityKind::ObjCClassMethodDecl => ObjCMethodKind::ClassMethod,
            _ => unreachable!(),
        };
        ObjCMethod {
            kind: kind,
            sel: entity.get_name().unwrap(),
            args: args.collect(),
            ret_type: ObjCType::Void,
        }
    }

    fn new(
        kind: ObjCMethodKind,
        sel: String,
        args: Vec<ObjCMethodArg>,
        ret_type: ObjCType,
    ) -> ObjCMethod {
        ObjCMethod {
            kind: kind,
            sel: sel,
            args: args,
            ret_type: ret_type,
        }
    }

    fn sel(&self) -> &str {
        &self.sel
    }
}

#[derive(Debug, PartialEq)]
struct ObjCClass {
    name: String,
    methods: Vec<ObjCMethod>,
}

impl ObjCClass {
    fn new(name: String, methods: Vec<ObjCMethod>) -> ObjCClass {
        ObjCClass {
            name: name,
            methods: methods,
        }
    }
    fn from(entity: &Entity) -> ObjCClass {
        assert!(entity.get_kind() == EntityKind::ObjCInterfaceDecl);
        let children = entity.get_children();
        let methods = children
            .iter()
            .filter(|child| {
                child.get_kind() == EntityKind::ObjCInstanceMethodDecl
                    || child.get_kind() == EntityKind::ObjCClassMethodDecl
            })
            .map(|decl| ObjCMethod::from(decl));
        ObjCClass {
            name: entity.get_name().unwrap(),
            methods: methods.collect(),
        }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn methods(&self) -> &[ObjCMethod] {
        &self.methods
    }
}

fn parse_objc(clang: &Clang, source: &str) -> Result<Vec<ObjCClass>, ParseError> {
    // The documentation says that files specified as unsaved must exist so create a dummy temporary empty file
    let file = tempfile::NamedTempFile::new().unwrap();
    let index = Index::new(clang, false, true);
    let mut parser = index.parser(file.path());

    parser.arguments(&[
        "-x",
        "objective-c", // The file doesn't have an Objective-C extension so set the language explicitely
        "-isysroot",
        &sdk_path(AppleSdk::MacOs),
    ]);
    parser.skip_function_bodies(true);
    parser.unsaved(&[clang::Unsaved::new(file.path(), source)]);
    let tu = parser.parse()?;
    // The parser will try to parse as much as possible, even with errors.
    // In that case, we still want fail because some information will be missing anyway.
    let diagnostics = tu.get_diagnostics();
    let mut errors = diagnostics.iter().filter(|diagnostic| {
        let severity = diagnostic.get_severity();
        severity == Severity::Error || severity == Severity::Fatal
    });
    if let Some(error) = errors.next() {
        return Err(ParseError::CompilationError(error.get_text()));
    }

    let mut objc_classes: Vec<ObjCClass> = Vec::new();
    tu.get_entity().visit_children(|entity, _| {
        if entity.get_kind() == EntityKind::ObjCInterfaceDecl {
            objc_classes.push(ObjCClass::from(&entity));
        }
        EntityVisitResult::Continue
    });
    Ok(objc_classes)
}

fn main() {
    let source = "
    #import <Foundation/Foundation.h>
    ";
    let clang = Clang::new().expect("Could not load libclang");
    let objc_classes = parse_objc(&clang, source).unwrap();
    println!("{:?}", objc_classes);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_same_classes(parsed_classes: &Vec<ObjCClass>, expected_classes: &Vec<ObjCClass>) {
        assert_eq!(parsed_classes.len(), expected_classes.len());
        for (parsed_class, expected_class) in parsed_classes.iter().zip(expected_classes) {
            assert_eq!(parsed_class.name(), expected_class.name());

            let parsed_methods = parsed_class.methods();
            let expected_methods = expected_class.methods();
            assert_eq!(parsed_methods.len(), expected_methods.len());

            for (parsed_method, expected_method) in parsed_methods.iter().zip(expected_methods) {
                assert_eq!(parsed_method.sel(), expected_method.sel());
            }
        }
    }

    #[test]
    fn simple_class() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @interface A
            - (void)foo;
            + (void)bar;
            - (void)hoge;
            @end
        ";

        let expected_classes: Vec<ObjCClass> = vec![
            ObjCClass::new(
                "A".into(),
                vec![
                    ObjCMethod::new(
                        ObjCMethodKind::InstanceMethod,
                        "foo".into(),
                        vec![],
                        ObjCType::Void,
                    ),
                    ObjCMethod::new(
                        ObjCMethodKind::ClassMethod,
                        "bar".into(),
                        vec![],
                        ObjCType::Void,
                    ),
                    ObjCMethod::new(
                        ObjCMethodKind::InstanceMethod,
                        "hoge".into(),
                        vec![],
                        ObjCType::Void,
                    ),
                ],
            ),
        ];

        let parsed_classes = parse_objc(&clang, source).unwrap();
        assert_same_classes(&parsed_classes, &expected_classes);
    }
}
