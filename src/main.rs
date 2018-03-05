#![allow(dead_code)]

extern crate clang;
#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate tempfile;

// TODO:
// - Do not forget the namespace support.
// - Maybe do something for 32/64-bit differences.
// - Maybe handle lightweight generics.
// - For args, look for const/inout/out, and nullable/nonnull...
// - Maybe convert ObjC errors (and/or exceptions) to Rust errors.
// - Do not forget about categories.
// - instancetype

use clang::{Clang, Entity, EntityKind, EntityVisitResult, Index};
use clang::diagnostic::Severity;
use std::process::Command;
use regex::Regex;

#[derive(Debug, PartialEq)]
enum Origin {
    ObjCCore,
    Framework(String),
    Library(String),
    Unknown,
}

fn guess_origin(path: &str) -> Origin {
    lazy_static! {
        static ref FRAMEWORK_PATH_RE: Regex = Regex::new(r"/([^./]+)\.framework/Headers/[^./]+.h\z").unwrap();
    }
    if let Some(caps) = FRAMEWORK_PATH_RE.captures(path) {
        return Origin::Framework(caps.get(1).unwrap().as_str().into());
    }

    lazy_static! {
        static ref LIBRARY_PATH_RE: Regex = Regex::new(r"/usr/include/([^./]+)/[^./]+.h\z").unwrap();
    }
    if let Some(caps) = LIBRARY_PATH_RE.captures(path) {
        let library = caps.get(1).unwrap().as_str();
        if library == "objc" {
            return Origin::ObjCCore;
        } else {
            return Origin::Library(library.into());
        }
    }

    Origin::Unknown
}

fn get_entity_file_path(entity: &Entity) -> Option<String> {
    let path = entity.get_location()?.get_file_location().file?.get_path();
    match path.into_os_string().into_string() {
        Ok(string) => Some(string),
        _ => None,
    }
}

fn guess_entity_origin(entity: &Entity) -> Origin {
    if let Some(file_path) = get_entity_file_path(entity) {
        guess_origin(&file_path)
    } else {
        Origin::Unknown
    }
}

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
    ObjCObjPtr(String, Vec<String>),
    Id(Vec<String>),
}

impl ObjCType {
    fn from(clang_type: &clang::Type) -> ObjCType {
        let kind = clang_type.get_kind();
        match kind {
            clang::TypeKind::Typedef => ObjCType::from(&clang_type.get_canonical_type()),
            clang::TypeKind::ObjCObjectPointer => {
                let pointee = clang_type.get_pointee_type().unwrap();
                match pointee.get_kind() {
                    clang::TypeKind::ObjCInterface => {
                        ObjCType::ObjCObjPtr(pointee.get_display_name(), Vec::new())
                    }
                    clang::TypeKind::Unexposed => {
                        println!(
                            "---------- Unexposed display name: {:?}",
                            clang_type.get_display_name()
                        );
                        println!(
                            "---------- template: {:?}",
                            pointee.get_template_argument_types()
                        );
                        if clang_type.get_display_name() == "id" {
                            ObjCType::Id(Vec::new())
                        } else if let Some(pointee_decl) = pointee.get_declaration() {
                            assert!(pointee_decl.get_kind() == EntityKind::ObjCInterfaceDecl);
                            println!("---: pointee: {:?}", pointee_decl);
                            ObjCType::ObjCObjPtr(pointee_decl.get_name().unwrap(), Vec::new())
                        } else {
                            panic!("{:?} -> {:?}", clang_type, pointee);
                        }
                    }
                    _ => {
                        // pointee.get_declaration()
                        println!("{:?} -> {:?}", clang_type, pointee);
                        println!(
                            "+++: {:?} -> {:?}",
                            clang_type.get_declaration(),
                            pointee.get_declaration()
                        );
                        println!(
                            "+++: {:?} -> {:?}",
                            clang_type.get_fields(),
                            pointee.get_fields()
                        );
                        println!(
                            "+++: {:?} -> {:?}",
                            clang_type.get_class_type(),
                            pointee.get_class_type()
                        );
                        panic!("Unhandled objc obj pointer {:?}", pointee);
                    }
                }
            }
            _ => {
                println!(
                    "Unimplement type {:?} - {:?}",
                    kind,
                    clang_type.get_display_name()
                );
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
        println!("ParmDecl children: {:?}", entity.get_children());

        if entity.get_type().unwrap().get_kind() == clang::TypeKind::Unexposed {
            println!("unexposed entity: {:?}", entity);
        }
        ObjCMethodArg {
            name: entity.get_name().unwrap(),
            objc_type: ObjCType::from(&entity.get_type().unwrap()),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum ObjCMethodKind {
    InstanceMethod,
    ClassMethod,
}

#[derive(Debug)]
struct ObjCMethod {
    kind: ObjCMethodKind,
    is_optional: bool,
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
        println!("method children: {:?}", entity.get_children());
        ObjCMethod {
            kind: kind,
            is_optional: entity.is_objc_optional(),
            sel: entity.get_name().unwrap(),
            args: args.collect(),
            ret_type: ObjCType::from(&entity.get_result_type().unwrap()),
        }
    }

    fn kind(&self) -> ObjCMethodKind {
        self.kind
    }

    fn is_optional(&self) -> bool {
        self.is_optional
    }

    fn sel(&self) -> &str {
        &self.sel
    }

    fn args(&self) -> &[ObjCMethodArg] {
        &self.args
    }

    fn ret_type(&self) -> &ObjCType {
        &self.ret_type
    }
}

#[derive(Debug)]
struct ObjCClass {
    name: String,
    template_arguments: Vec<String>,
    superclass_name: Option<String>,
    adopted_protocol_names: Vec<String>,
    methods: Vec<ObjCMethod>,
    guessed_origin: Origin,
}

impl ObjCClass {
    fn from(entity: &Entity) -> ObjCClass {
        assert!(entity.get_kind() == EntityKind::ObjCInterfaceDecl);
        let children = entity.get_children();

        let mut superclass_names = children
            .iter()
            .filter(|child| child.get_kind() == EntityKind::ObjCSuperClassRef)
            .map(|super_class| super_class.get_name());

        let superclass_name: Option<String> = match superclass_names.next() {
            Some(Some(ref name)) => Some(name.clone()),
            Some(None) => panic!("A superclass is expected to have a name"),
            None => None,
        };
        // There should only be one superclass.
        assert!(superclass_names.next() == None);

        let adopted_protocol_names = children
            .iter()
            .filter(|child| child.get_kind() == EntityKind::ObjCProtocolRef)
            .map(|protocol| protocol.get_name().unwrap());

        let methods = children
            .iter()
            .filter(|child| {
                child.get_kind() == EntityKind::ObjCInstanceMethodDecl
                    || child.get_kind() == EntityKind::ObjCClassMethodDecl
            })
            .map(ObjCMethod::from);

        ObjCClass {
            name: entity.get_name().unwrap(),
            template_arguments: Vec::new(),
            superclass_name: superclass_name,
            adopted_protocol_names: adopted_protocol_names.collect(),
            methods: methods.collect(),
            guessed_origin: guess_entity_origin(entity),
        }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn superclass_name(&self) -> &Option<String> {
        &self.superclass_name
    }

    fn adopted_protocol_names(&self) -> &[String] {
        &self.adopted_protocol_names
    }

    fn methods(&self) -> &[ObjCMethod] {
        &self.methods
    }
}

#[derive(Debug)]
struct ObjCProtocol {
    name: String,
    adopted_protocol_names: Vec<String>,
    methods: Vec<ObjCMethod>,
    guessed_origin: Origin,
}

impl ObjCProtocol {
    fn from(entity: &Entity) -> ObjCProtocol {
        assert!(entity.get_kind() == EntityKind::ObjCProtocolDecl);
        let children = entity.get_children();

        let adopted_protocol_names = children
            .iter()
            .filter(|child| child.get_kind() == EntityKind::ObjCProtocolRef)
            .map(|protocol| protocol.get_name().unwrap());

        let methods = children
            .iter()
            .filter(|child| {
                child.get_kind() == EntityKind::ObjCInstanceMethodDecl
                    || child.get_kind() == EntityKind::ObjCClassMethodDecl
            })
            .map(ObjCMethod::from);

        ObjCProtocol {
            name: entity.get_name().unwrap(),
            adopted_protocol_names: adopted_protocol_names.collect(),
            methods: methods.collect(),
            guessed_origin: guess_entity_origin(entity),
        }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn adopted_protocol_names(&self) -> &[String] {
        &self.adopted_protocol_names
    }

    fn methods(&self) -> &[ObjCMethod] {
        &self.methods
    }
}

#[derive(Debug)]
struct ObjCDecls {
    classes: Vec<ObjCClass>,
    protocols: Vec<ObjCProtocol>,
}

impl ObjCDecls {
    fn classes(&self) -> &[ObjCClass] {
        &self.classes
    }

    fn protocols(&self) -> &[ObjCProtocol] {
        &self.protocols
    }
}

fn show_tree(entity: &Entity, indent_level: usize) {
    let indent = {
        let mut indent = String::new();
        for _ in 0..indent_level {
            indent.push_str("   ");
        }
        indent
    };

    if let Some(name) = entity.get_name() {
        println!("{}*** kind: {:?} - {} ***", indent, entity.get_kind(), name);
    } else {
        println!("{}*** kind: {:?} ***", indent, entity.get_kind());
    }
    if entity.get_display_name() != entity.get_name() {
        if let Some(display_name) = entity.get_display_name() {
            println!("{}display name: {:?}", indent, display_name);
        }
    }

    if let Some(arguments) = entity.get_arguments() {
        println!("{}arguments:", indent);
        for arg in arguments {
            show_tree(&arg, indent_level + 1);
        }
    }

    let availability = entity.get_availability();
    if availability != clang::Availability::Available {
        println!("{}availability: {:?}", indent, availability);
    }

    let canonical_entity = entity.get_canonical_entity();
    if &canonical_entity != entity {
        println!("{}canonical_entity: {:?}", indent, canonical_entity);
    }

    if let Some(definition) = entity.get_definition() {
        println!("{}definition: {:?}", indent, definition);
    }

    if let Some(external_symbol) = entity.get_external_symbol() {
        println!("{}external symbol: {:?}", indent, external_symbol);
    }

    if let Some(module) = entity.get_module() {
        println!("{}module: {:?}", indent, module);
    }

    if let Some(template) = entity.get_template() {
        println!("{}template: {:?}", indent, template);
    }

    if let Some(template_kind) = entity.get_template_kind() {
        println!("{}template kind: {:?}", indent, template_kind);
    }

    if let Some(template_arguments) = entity.get_template_arguments() {
        println!("{}template_arguments: {:?}", indent, template_arguments);
    }

    if let Some(clang_type) = entity.get_type() {
        println!("{}type: {:?}", indent, clang_type);
    }

    if let Some(visibility) = entity.get_visibility() {
        println!("{}visibility: {:?}", indent, visibility);
    }

    if let Some(result_type) = entity.get_result_type() {
        println!("{}result_type: {:?}", indent, result_type);
    }

    if let Some(mangled_name) = entity.get_mangled_name() {
        println!("{}mangled_name: {:?}", indent, mangled_name);
    }

    if let Some(objc_ib_outlet_collection_type) = entity.get_objc_ib_outlet_collection_type() {
        println!(
            "{}objc_ib_outlet_collection_type: {:?}",
            indent, objc_ib_outlet_collection_type
        );
    }

    if let Some(objc_type_encoding) = entity.get_objc_type_encoding() {
        println!("{}objc type encoding: {:?}", indent, objc_type_encoding);
    }

    if let Some(objc_selector_index) = entity.get_objc_selector_index() {
        println!("{}objc selector index: {:?}", indent, objc_selector_index);
    }

    if let Some(objc_qualifiers) = entity.get_objc_qualifiers() {
        println!("{}objc qualifiers: {:?}", indent, objc_qualifiers);
    }

    let children = entity.get_children();
    if !children.is_empty() {
        println!("{}children:", indent);
        for child in children {
            show_tree(&child, indent_level + 1);
        }
    }
}

fn parse_objc(clang: &Clang, source: &str) -> Result<ObjCDecls, ParseError> {
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

    println!("--------------------------------");
    show_tree(&tu.get_entity(), 0);
    println!("--------------------------------");

    let mut objc_classes: Vec<ObjCClass> = Vec::new();
    let mut objc_protocols: Vec<ObjCProtocol> = Vec::new();
    tu.get_entity().visit_children(|entity, _| {
        match entity.get_kind() {
            EntityKind::ObjCInterfaceDecl => {
                objc_classes.push(ObjCClass::from(&entity));
            }
            EntityKind::ObjCProtocolDecl => {
                objc_protocols.push(ObjCProtocol::from(&entity));
            }
            _ => {}
        }
        EntityVisitResult::Continue
    });
    Ok(ObjCDecls {
        classes: objc_classes,
        protocols: objc_protocols,
    })
}

fn main() {
    let source = "
    // #import <Foundation/Foundation.h>
        // @protocol X;
        // @protocol Y;
        // @interface B<__covariant T>
        // - (T)machin;
        // @end
        // @class C;
        // typedef B<C *> *S;
        // @interface A
        // - (C<    /*  _____ ----- - */ X> *)foo:(B<   /*aaa*/ C<  X  , Y> *> *)xxx :(S *)yyy;
        // @end

        @interface A
        @end
        @interface A ()
        - (void)foo;
        @end
    ";
    let clang = Clang::new().expect("Could not load libclang");
    let decls = parse_objc(&clang, source).unwrap();
    println!("{:?}", decls);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_same_decls(parsed_decls: &ObjCDecls, expected_decls: &ObjCDecls) {
        let parsed_classes = parsed_decls.classes();
        let expected_classes = expected_decls.classes();
        assert_eq!(parsed_classes.len(), expected_classes.len());
        for (parsed_class, expected_class) in parsed_classes.iter().zip(expected_classes) {
            assert_eq!(parsed_class.name(), expected_class.name());
            assert_eq!(
                parsed_class.superclass_name(),
                expected_class.superclass_name()
            );
            assert_eq!(
                parsed_class.adopted_protocol_names(),
                expected_class.adopted_protocol_names()
            );

            let parsed_methods = parsed_class.methods();
            let expected_methods = expected_class.methods();
            assert_eq!(parsed_methods.len(), expected_methods.len());

            for (parsed_method, expected_method) in parsed_methods.iter().zip(expected_methods) {
                assert_eq!(parsed_method.sel(), expected_method.sel());
                assert_eq!(parsed_method.kind(), expected_method.kind());
                // For classes no method should be optional.
                assert_eq!(parsed_method.is_optional(), expected_method.is_optional());
                assert_eq!(parsed_method.args(), expected_method.args());
                assert_eq!(parsed_method.ret_type(), expected_method.ret_type());
            }
        }

        let parsed_protocols = parsed_decls.protocols();
        let expected_protocols = expected_decls.protocols();
        assert_eq!(parsed_protocols.len(), expected_protocols.len());
        for (parsed_protocol, expected_protocol) in parsed_protocols.iter().zip(expected_protocols)
        {
            assert_eq!(parsed_protocol.name(), expected_protocol.name());
            assert_eq!(
                parsed_protocol.adopted_protocol_names(),
                parsed_protocol.adopted_protocol_names()
            );

            let parsed_methods = parsed_protocol.methods();
            let expected_methods = expected_protocol.methods();
            assert_eq!(parsed_methods.len(), expected_methods.len());
            for (parsed_method, expected_method) in parsed_methods.iter().zip(expected_methods) {
                assert_eq!(parsed_method.sel(), expected_method.sel());
                assert_eq!(parsed_method.kind(), expected_method.kind());
                assert_eq!(parsed_method.is_optional(), expected_method.is_optional());
                assert_eq!(parsed_method.args(), expected_method.args());
                assert_eq!(parsed_method.ret_type(), expected_method.ret_type());
            }
        }
    }

    #[test]
    fn test_very_simple_class() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @interface A
            - (void)foo;
            + (void)bar;
            - (void)hoge;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "A".into(),
                    template_arguments: Vec::new(),
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "bar".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "hoge".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
            ],
            protocols: vec![],
        };

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_same_decls(&parsed_decls, &expected_decls);
    }

    #[test]
    fn test_very_simple_protocol() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @protocol A
            - (void)foo;
            + (void)bar;
            @optional
            - (void)hoge;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![],
            protocols: vec![
                ObjCProtocol {
                    name: "A".into(),
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "bar".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: true,
                            sel: "hoge".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
            ],
        };

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_same_decls(&parsed_decls, &expected_decls);
    }

    #[test]
    fn test_very_simple_inheritance() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @interface B
            - (void)foo;
            @end

            @interface A: B
            + (void)bar;
            - (void)hoge;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "B".into(),
                    template_arguments: Vec::new(),
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
                ObjCClass {
                    name: "A".into(),
                    template_arguments: Vec::new(),
                    superclass_name: Some("B".into()),
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "bar".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "hoge".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
            ],
            protocols: vec![],
        };

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_same_decls(&parsed_decls, &expected_decls);
    }

    #[test]
    fn test_simple_protocol_conformance() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @protocol C
            - (void)foo;
            @end

            @protocol B <C>
            - (void)bar;
            @end

            @interface A <B>
            - (void)hoge;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "A".into(),
                    template_arguments: Vec::new(),
                    superclass_name: None,
                    adopted_protocol_names: vec!["B".into()],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "hoge".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
            ],
            protocols: vec![
                ObjCProtocol {
                    name: "C".into(),
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
                ObjCProtocol {
                    name: "B".into(),
                    adopted_protocol_names: vec!["C".into()],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "bar".into(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
            ],
        };

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_same_decls(&parsed_decls, &expected_decls);
    }

    #[test]
    fn test_simple_objc_class_pointer() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @interface A
            - (A *)foo;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "A".into(),
                    template_arguments: Vec::new(),
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".into(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr("A".into(), Vec::new()),
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
            ],
            protocols: vec![],
        };

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_same_decls(&parsed_decls, &expected_decls);
    }

    #[test]
    fn test_guess_origin() {
        assert_eq!(guess_origin(""), Origin::Unknown);
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/System/Library/Frameworks/Foundation.framework/Headers/NSValue.h"),
            Origin::Framework("Foundation".into()),
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/System/Library/Frameworks/Metal.framework/Headers/MTLCaptureManager.h"),
            Origin::Framework("Metal".into()),
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/objc/NSObject.h"),
            Origin::ObjCCore,
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/dispatch/object.h"),
            Origin::Library("dispatch".into()),
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/dispatch/queue.h"),
            Origin::Library("dispatch".into()),
        );
    }
}
