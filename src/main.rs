extern crate clang;
extern crate tempfile;

// TODO:
// - Do not forget the namespace support.

use clang::{Clang, Entity, Index, TranslationUnit};
use clang::diagnostic::Severity;
use std::process::Command;

fn print_entity(entity: Entity, depth: u32) {
    let mut indent = String::new();
    for _ in 0..depth {
        indent.push_str("--");
    }

    println!("{}name: {:?}", indent, entity.get_name());
    println!("{}display name: {:?}", indent, entity.get_display_name());
    println!("{}kind: {:?}", indent, entity.get_kind());
    println!("{}range: {:?}", indent, entity.get_range());
    // println!("{}definition: {:?}", indent, entity.get_definition());
    if let Some(attr) = entity.get_objc_attributes() {
        println!("{}objc attributes: {:?}", indent, attr);
    }
    if let Some(t) = entity.get_type() {
        println!("{}type: {:?}", indent, t);
        println!("{}type result type: {:?}", indent, t.get_result_type());
    }
    if let Some(t) = entity.get_result_type() {
        println!("{}return type: {:?}", indent, t);
        println!(
            "{}return canonical type: {:?}",
            indent,
            t.get_canonical_type()
        );
        println!("{}return type+: {:?}", indent, t.get_pointee_type());
    }

    if let Some(arguments) = entity.get_arguments() {
        if !arguments.is_empty() {
            println!("{}arguments:", indent);
            for argument in arguments {
                print_entity(argument, depth + 1);
            }
        }
    }

    entity.get_children().into_iter().for_each(|child| {
        print_entity(child, depth + 1);
    });
}

#[allow(dead_code)]
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

fn parse_objc<'a>(index: &'a Index, source: &str) -> Result<TranslationUnit<'a>, ParseError> {
    // The documentation says that files specified as unsaved must exist so create a dummy temporary empty file
    let file = tempfile::NamedTempFile::new().unwrap();
    let mut parser = index.parser(file.path());

    parser.arguments(&[
        "-x",
        "objective-c", // The file doesn't have an Objective-C extension so set the language explicitely
        "-isysroot",
        &sdk_path(AppleSdk::MacOs),
    ]);
    parser.skip_function_bodies(true);
    parser.unsaved(&[clang::Unsaved::new(file.path(), source)]);
    let tu = parser.parse().map_err(|e| ParseError::SourceError(e))?;
    // The parser will try to parse as much as possible, even with errors.
    // In that case, we still want fail because some information will be missing anyway.
    for diagnostic in tu.get_diagnostics() {
        let severity = diagnostic.get_severity();
        if severity == Severity::Error || severity == Severity::Fatal {
            return Err(ParseError::CompilationError(diagnostic.get_text()));
        }
    }
    Ok(tu)
}

fn main() {
    let source = "
    #import <Foundation/Foundation.h>
    ";
    let clang = Clang::new().expect("Could not load libclang");
    let index = Index::new(&clang, false, true);
    let tu = parse_objc(&index, source).unwrap();

    for entity in tu.get_entity().get_children().into_iter() {
        print_entity(entity, 0);
        println!();
    }
}
