#![allow(dead_code)]

extern crate clang;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate nom;
extern crate regex;
extern crate tempfile;

mod parser;

// TODO:
// - Do not forget the namespace support.
// - Maybe do something for 32/64-bit differences.
// - For args, look for const/inout/out, and nullable/nonnull...
// - Maybe convert ObjC errors (and/or exceptions) to Rust errors.
// - Do not forget about categories.
// - use .to_owned() instead of .into()
// - rename project to chocolatier? alchemist?
// - BOOL should be mapped to a real boolean

use clang::{Clang, Entity, EntityKind, EntityVisitResult, Index, TypeKind};
use clang::diagnostic::Severity;
use std::process::Command;
use regex::Regex;
use std::iter::Peekable;

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
    ObjCObjPtr(String, Vec<ObjCType>, Vec<String>),
    ObjCId(Vec<String>),
    ObjCInstancetype,
    ObjCClass,
    ObjCSel,
    TemplateArgument(String),
}

fn is_type_objc_id(clang_type: &clang::Type) -> bool {
    if clang_type.get_kind() == TypeKind::ObjCId {
        return true;
    }
    // In some cases `id`'s type seems to not be TypeKind::ObjCId so check in a more ugly way.
    if clang_type.get_display_name() != "id" {
        return false;
    }
    clang_type.get_kind() == TypeKind::ObjCObjectPointer
        && clang_type.get_pointee_type().unwrap().get_kind() == TypeKind::Unexposed
}

fn is_entity_objc_id(entity: &Entity) -> bool {
    [EntityKind::TypeRef, EntityKind::TypedefDecl].contains(&entity.get_kind())
        && is_type_objc_id(&entity.get_type().unwrap())
}

fn is_entity_objc_class(entity: &Entity) -> bool {
    entity.get_kind() == EntityKind::TypeRef
        && entity.get_type().unwrap().get_kind() == TypeKind::ObjCClass
}

fn is_entity_objc_sel(entity: &Entity) -> bool {
    entity.get_kind() == EntityKind::TypeRef
        && entity.get_type().unwrap().get_kind() == TypeKind::ObjCSel
}

fn is_entity_objc_instancetype(entity: &Entity) -> bool {
    // There doesn't seem to be a real clean way to check for instancetype...
    if entity.get_kind() != EntityKind::TypeRef {
        return false;
    }
    if entity.get_name().unwrap() != "instancetype" {
        return false;
    }
    // In fact the canonical type of `instancetype` is `id` but we want to make the distinction.
    is_type_objc_id(&entity.get_type().unwrap().get_canonical_type())
}

impl ObjCType {
    fn from(
        clang_type: &clang::Type,
        children: &mut Peekable<std::slice::Iter<Entity>>,
    ) -> ObjCType {
        let kind = clang_type.get_kind();
        match kind {
            TypeKind::Typedef => {
                let child = children.next().unwrap();
                if is_entity_objc_instancetype(child) {
                    return ObjCType::ObjCInstancetype;
                }

                let declaration = clang_type.get_declaration().unwrap();
                let decl_children = declaration.get_children();
                let mut peekable_children = decl_children.iter().peekable();
                let objc_type =
                    ObjCType::from(&clang_type.get_canonical_type(), &mut peekable_children);
                assert!(peekable_children.peek() == None);
                objc_type
            }
            TypeKind::ObjCObjectPointer => {
                let pointee = clang_type.get_pointee_type().unwrap();
                match pointee.get_kind() {
                    TypeKind::ObjCInterface => {
                        let type_name = pointee.get_display_name();
                        let child = children.next().unwrap();
                        assert_eq!(child.get_kind(), EntityKind::ObjCClassRef);
                        assert_eq!(child.get_name().unwrap(), type_name);
                        ObjCType::ObjCObjPtr(type_name, Vec::new(), Vec::new())
                    }
                    // libclang is not being very helpful.
                    // It has no direct way to represent template instanciation or adoption of protocols.
                    TypeKind::Unexposed => {
                        let base_entity = children.next().unwrap();

                        let mut template_arguments: Vec<ObjCType> = Vec::new();
                        let mut protocol_names: Vec<String> = Vec::new();

                        println!("base entity: {:?}", base_entity);
                        let second_child_kind = children.peek().unwrap().get_kind();
                        match second_child_kind {
                            // TODO: Need refactoring. Maybe can use recursion in some cases.
                            EntityKind::TypeRef | EntityKind::ObjCClassRef => {
                                'template_args_outer: loop {
                                    let next_child_kind = match children.peek() {
                                        None => break 'template_args_outer,
                                        Some(child) => child.get_kind(),
                                    };
                                    match next_child_kind {
                                        EntityKind::TypeRef => {
                                            let next_child = children.next().unwrap();
                                            let definition = next_child.get_definition().unwrap();
                                            if is_entity_objc_id(&definition) {
                                                template_arguments
                                                    .push(ObjCType::ObjCId(Vec::new()));
                                            } else if definition.get_kind()
                                                == EntityKind::TemplateTypeParameter
                                            {
                                                template_arguments.push(
                                                    ObjCType::TemplateArgument(
                                                        next_child.get_name().unwrap(),
                                                    ),
                                                );
                                            } else {
                                                panic!("Unknown definition {:?}", definition);
                                            }
                                        }
                                        EntityKind::ObjCClassRef => {
                                            let next_child = children.next().unwrap();
                                            template_arguments.push(ObjCType::ObjCObjPtr(
                                                next_child.get_name().unwrap(),
                                                Vec::new(), // TODO: Might be non-empty?
                                                Vec::new(), // TODO: Might be non-empty?
                                            ));
                                        }
                                        _ => {
                                            break 'template_args_outer;
                                        }
                                    }
                                }
                            }
                            EntityKind::ObjCProtocolRef => 'protocols_outer: loop {
                                let next_child_kind = match children.peek() {
                                    None => break 'protocols_outer,
                                    Some(child) => child.get_kind(),
                                };
                                if next_child_kind == EntityKind::ObjCProtocolRef {
                                    let next_child = children.next().unwrap();
                                    protocol_names.push(next_child.get_name().unwrap());
                                } else {
                                    break;
                                }
                            },
                            _ => {
                                panic!("Unexpected second child {:?}", second_child_kind);
                            }
                        }

                        if base_entity.get_kind() == EntityKind::ObjCClassRef {
                            ObjCType::ObjCObjPtr(
                                base_entity.get_name().unwrap(),
                                template_arguments,
                                protocol_names,
                            )
                        } else if is_entity_objc_id(base_entity) {
                            assert!(template_arguments.is_empty());
                            ObjCType::ObjCId(protocol_names)
                        } else {
                            panic!("Indecipherable TypeKind::Unexposed {:?}", pointee);
                        }
                    }
                    _ => {
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
            TypeKind::Unexposed => {
                let child = children
                    .next()
                    .expect("No child that could help find what the Unexposed hides");
                if child.get_kind() != EntityKind::TypeRef {
                    panic!(
                        "Unexpected child {:?} when trying to find what an Unexposed hides",
                        child
                    )
                }
                let definition = child.get_definition().unwrap();
                if definition.get_kind() != EntityKind::TemplateTypeParameter {
                    panic!(
                        "Unexpected child definition {:?} when trying to find what an Unexposed hides",
                        definition
                    )
                }

                ObjCType::TemplateArgument(definition.get_name().unwrap())
            }
            TypeKind::Void => ObjCType::Void,
            TypeKind::ObjCId => {
                let child = children.next().unwrap();
                assert!(is_entity_objc_id(child));
                ObjCType::ObjCId(Vec::new())
            }
            TypeKind::ObjCClass => {
                let child = children.next().unwrap();
                assert!(is_entity_objc_class(child));
                ObjCType::ObjCClass
            }
            TypeKind::ObjCSel => {
                let child = children.next().unwrap();
                assert!(is_entity_objc_sel(child));
                ObjCType::ObjCSel
            }
            _ => {
                println!(
                    "Unimplemented type {:?} - {:?}",
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

        let children = entity.get_children();
        let mut peekable_children = children.iter().peekable();
        let arg = ObjCMethodArg {
            name: entity.get_name().unwrap(),
            objc_type: ObjCType::from(&entity.get_type().unwrap(), &mut peekable_children),
        };
        assert!(peekable_children.peek() == None);
        arg
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
            [
                EntityKind::ObjCInstanceMethodDecl,
                EntityKind::ObjCClassMethodDecl,
            ].contains(&entity.get_kind())
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
        let children = entity.get_children();
        let mut peekable_children = children.iter().peekable();
        let method = ObjCMethod {
            kind,
            is_optional: entity.is_objc_optional(),
            sel: entity.get_name().unwrap(),
            args: args.collect(),
            ret_type: ObjCType::from(&entity.get_result_type().unwrap(), &mut peekable_children),
        };
        assert!(
            peekable_children.peek() == None
                || peekable_children.peek().unwrap().get_kind() == EntityKind::ParmDecl
        );
        method
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
                [
                    EntityKind::ObjCInstanceMethodDecl,
                    EntityKind::ObjCClassMethodDecl,
                ].contains(&child.get_kind())
            })
            .map(ObjCMethod::from);

        let template_arguments = children
            .iter()
            .filter(|child| child.get_kind() == EntityKind::TemplateTypeParameter)
            .map(|child| child.get_name().unwrap());

        ObjCClass {
            name: entity.get_name().unwrap(),
            template_arguments: template_arguments.collect(),
            superclass_name,
            adopted_protocol_names: adopted_protocol_names.collect(),
            methods: methods.collect(),
            guessed_origin: guess_entity_origin(entity),
        }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn template_arguments(&self) -> &[String] {
        &self.template_arguments
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
                [
                    EntityKind::ObjCInstanceMethodDecl,
                    EntityKind::ObjCClassMethodDecl,
                ].contains(&child.get_kind())
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
        if !arguments.is_empty() {
            println!("{}arguments:", indent);
            for arg in arguments {
                show_tree(&arg, indent_level + 1);
            }
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
        [Severity::Error, Severity::Fatal].contains(&severity)
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
        @interface B<__covariant T>
        - (B<B<B*>*>*)m5;
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
                parsed_class.template_arguments(),
                expected_class.template_arguments()
            );
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
    fn test_protocol_conformance() {
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
    fn test_objc_class_pointer() {
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
                            ret_type: ObjCType::ObjCObjPtr("A".into(), Vec::new(), Vec::new()),
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

    // #[test]
    // TODO: Mix lightweight generics and protocols.
    fn test_lightweight_generic() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @class A;
            @interface B<__covariant T>
            - (T)m1;
            - (B*)m2;
            - (B<T>*)m3;
            - (B<A*>*)m4;
            - (B<B<A*>*>*)m5;
            @end
            // @interface C<__covariant T, __covariant U>
            // - (C<C<B<T>*, U>*, B<id>*> *)foo;
            // @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "B".into(),
                    template_arguments: vec!["T".into()],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "m1".into(),
                            args: vec![],
                            ret_type: ObjCType::TemplateArgument("T".into()),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "m2".into(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr("B".into(), vec![], vec![]),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "m3".into(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr(
                                "B".into(),
                                vec![ObjCType::TemplateArgument("T".into())],
                                vec![],
                            ),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "m4".into(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr(
                                "B".into(),
                                vec![ObjCType::ObjCObjPtr("A".into(), vec![], vec![])],
                                vec![],
                            ),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "m5".into(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr(
                                "B".into(),
                                vec![
                                    ObjCType::ObjCObjPtr(
                                        "B".into(),
                                        vec![ObjCType::ObjCObjPtr("A".into(), vec![], vec![])],
                                        vec![],
                                    ),
                                ],
                                vec![],
                            ),
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
                // ObjCClass {
                //     name: "C".into(),
                //     template_arguments: vec!["T".into(), "U".into()],
                //     superclass_name: None,
                //     adopted_protocol_names: vec![],
                //     methods: vec![
                //         ObjCMethod {
                //             kind: ObjCMethodKind::InstanceMethod,
                //             is_optional: false,
                //             sel: "foo".into(),
                //             args: vec![],
                //             ret_type: ObjCType::ObjCObjPtr(
                //                 "C".into(),
                //                 vec![
                //                     ObjCType::ObjCObjPtr(
                //                         "C".into(),
                //                         vec![
                //                             ObjCType::ObjCObjPtr(
                //                                 "B".into(),
                //                                 vec![ObjCType::TemplateArgument("T".into())],
                //                                 vec![],
                //                             ),
                //                             ObjCType::TemplateArgument("U".into()),
                //                         ],
                //                         vec![],
                //                     ),
                //                     ObjCType::ObjCObjPtr(
                //                         "B".into(),
                //                         vec![ObjCType::ObjCId(vec![])],
                //                         vec![],
                //                     ),
                //                 ],
                //                 vec![],
                //             ),
                //         },
                //     ],
                //     guessed_origin: Origin::Unknown,
                // },
            ],
            protocols: vec![],
        };

        let parsed_decls = parse_objc(&clang, source).unwrap();
        assert_same_decls(&parsed_decls, &expected_decls);
    }

    #[test]
    fn test_parameter_adopting_protocols() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @protocol P1, P2;
            @class B;
            @interface A
            - (void)foo:(id<P1, P2>)x;
            + (void)bar:(B<P2>*)y;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "A".into(),
                    template_arguments: vec![],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo:".into(),
                            args: vec![
                                ObjCMethodArg {
                                    name: "x".into(),
                                    objc_type: ObjCType::ObjCId(vec!["P1".into(), "P2".into()]),
                                },
                            ],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "bar:".into(),
                            args: vec![
                                ObjCMethodArg {
                                    name: "y".into(),
                                    objc_type: ObjCType::ObjCObjPtr(
                                        "B".into(),
                                        vec![],
                                        vec!["P2".into()],
                                    ),
                                },
                            ],
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
    fn test_custom_objc_types_return_value() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @interface A
            - (instancetype)anInstancetype;
            - (id)anId;
            - (Class)aClass;
            - (SEL)aSel;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "A".into(),
                    template_arguments: vec![],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "anInstancetype".into(),
                            args: vec![],
                            ret_type: ObjCType::ObjCInstancetype,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "anId".into(),
                            args: vec![],
                            ret_type: ObjCType::ObjCId(vec![]),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "aClass".into(),
                            args: vec![],
                            ret_type: ObjCType::ObjCClass,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "aSel".into(),
                            args: vec![],
                            ret_type: ObjCType::ObjCSel,
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
