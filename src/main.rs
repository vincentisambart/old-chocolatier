#![allow(dead_code)]

extern crate clang;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate nom;
extern crate regex;
extern crate tempfile;

mod parser;
use parser::ParsedEntity;

// TODO:
// - Do not forget the namespace support.
// - Maybe do something for 32/64-bit differences.
// - For args, look for const/inout/out, and nullable/nonnull...
// - Maybe convert ObjC errors (and/or exceptions) to Rust errors.
// - Do not forget about categories.
// - BOOL should be mapped to a real boolean
// - structs, enums
// - maybe stop using children to get types as it's too brittle (but only once there are tests for all data structures)
// - add way to check if optional method can be called to protocols
// - only be able to call method of a certain OS version if running on that OS version (something like if #available, probably doable properly using the type system)

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
        return Origin::Framework(caps.get(1).unwrap().as_str().to_owned());
    }

    lazy_static! {
        static ref LIBRARY_PATH_RE: Regex = Regex::new(r"/usr/include/([^./]+)/[^./]+.h\z").unwrap();
    }
    if let Some(caps) = LIBRARY_PATH_RE.captures(path) {
        let library = caps.get(1).unwrap().as_str();
        if library == "objc" {
            return Origin::ObjCCore;
        } else {
            return Origin::Library(library.to_owned());
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
    String::from_utf8_lossy(&output.stdout).trim().to_owned()
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
struct CallableDecl {
    args: Vec<(Option<String>, ObjCType)>,
    ret_type: ObjCType,
    is_variadic: bool,
}

impl CallableDecl {
    fn from_type<'a, 'tu: 'a, I: Iterator<Item = &'a Entity<'tu>>>(
        clang_type: &clang::Type,
        children: &mut Peekable<I>,
    ) -> CallableDecl {
        assert!(
            [
                TypeKind::Unexposed,
                TypeKind::FunctionPrototype,
                TypeKind::BlockPointer,
            ].contains(&clang_type.get_kind())
        );
        let result_type = clang_type.get_result_type().unwrap();
        let ret_type = ObjCType::from(&result_type, children);
        let mut args: Vec<(Option<String>, ObjCType)> = Vec::new();
        let arg_count = clang_type.get_argument_types().unwrap().len();
        if arg_count > 0 {
            // Very ugly hack, due to if a FunctionPrototype returns a typedef for some reason result_type is the expanded type...?
            let mut should_skip_next = false;
            if let Some(next_child) = children.peek() {
                if next_child.get_kind() == EntityKind::TypeRef {
                    should_skip_next = true;
                }
            }
            if should_skip_next {
                children.next();
            }
        }
        for _ in 0..arg_count {
            let arg_entity = children.next().unwrap();
            assert_eq!(arg_entity.get_kind(), EntityKind::ParmDecl);
            let arg_entity_type = arg_entity.get_type().unwrap();
            let arg_children = arg_entity.get_children();
            let mut arg_children_peekable = arg_children.iter().peekable();
            let arg_objc_type = ObjCType::from(&arg_entity_type, &mut arg_children_peekable);
            assert_eq!(arg_children_peekable.peek(), None);
            let arg = (arg_entity.get_name(), arg_objc_type);
            args.push(arg);
        }
        CallableDecl {
            args,
            ret_type,
            is_variadic: clang_type.is_variadic(),
        }
    }
}

#[derive(Debug, PartialEq)]
struct StructDecl {
    fields: Vec<(String, ObjCType)>,
}

#[derive(Debug, PartialEq)]
enum ObjCType {
    Void,
    ObjCObjPtr(String, Vec<ObjCType>, Vec<String>),
    ObjCId(Vec<String>),
    ObjCInstancetype,
    ObjCClass,
    ObjCSel,
    FnPtr(Box<CallableDecl>),
    BlockPtr(Box<CallableDecl>),
    Struct(StructDecl),
    Ptr(Box<ObjCType>),
    TemplateArgument(String),
    ULong,
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

enum MatchedTypePart {
    Protocol(String),
    TemplateArg(String),
    ObjPtr(String, Vec<MatchedTypePart>),
}

impl ObjCType {
    fn from_parsed_entity<'a, 'tu: 'a, I: Iterator<Item = &'a Entity<'tu>>>(
        base_entity: &Entity,
        parsed_entity: &ParsedEntity,
        children: &mut Peekable<I>,
    ) -> ObjCType {
        assert_eq!(parsed_entity.name(), &base_entity.get_name().unwrap());
        assert!([EntityKind::ObjCClassRef, EntityKind::TypeRef].contains(&base_entity.get_kind()));

        let mut protocol_names: Vec<String> = Vec::new();
        let mut child_types: Vec<ObjCType> = Vec::new();

        for condition in parsed_entity.conditions() {
            let child = children.next().unwrap();
            assert!(
                [
                    EntityKind::ObjCClassRef,
                    EntityKind::TypeRef,
                    EntityKind::ObjCProtocolRef
                ].contains(&child.get_kind())
            );
            assert_eq!(condition.name(), &child.get_name().unwrap());
            if child.get_kind() == EntityKind::ObjCProtocolRef {
                protocol_names.push(condition.name().to_owned());
            } else {
                child_types.push(Self::from_parsed_entity(child, condition, children));
            }
        }

        match base_entity.get_kind() {
            EntityKind::ObjCClassRef => {
                assert!(parsed_entity.is_ptr());
                ObjCType::ObjCObjPtr(base_entity.get_name().unwrap(), child_types, protocol_names)
            }
            EntityKind::TypeRef => {
                assert!(child_types.is_empty());
                if is_entity_objc_id(base_entity) {
                    ObjCType::ObjCId(protocol_names)
                } else {
                    // TODO: Check if it's really a template arg.
                    assert!(protocol_names.is_empty());
                    ObjCType::TemplateArgument(base_entity.get_name().unwrap())
                }
            }
            _ => panic!("Unexpected entity {:?}", base_entity),
        }
    }

    fn from_unexposed_type<'a, 'tu: 'a, I: Iterator<Item = &'a Entity<'tu>>>(
        clang_type: &clang::Type,
        children: &mut Peekable<I>,
    ) -> ObjCType {
        // libclang is not being very helpful...
        // It has no direct way to represent for example template instanciation or adoption of protocols.
        assert!(
            clang_type.get_kind() == TypeKind::Unexposed
                || (clang_type.get_kind() == TypeKind::ObjCObjectPointer
                    && clang_type.get_pointee_type().unwrap().get_kind() == TypeKind::Unexposed)
        );
        match parser::parse_unexposed_type(&clang_type.get_display_name()) {
            Some(parsed_entity) => {
                Self::from_parsed_entity(children.next().unwrap(), &parsed_entity, children)
            }
            None => panic!("Could not decipher {:?}", clang_type.get_display_name()),
        }
    }

    fn from<'a, 'tu: 'a, I: Iterator<Item = &'a Entity<'tu>>>(
        clang_type: &clang::Type,
        children: &mut Peekable<I>,
    ) -> ObjCType {
        let kind = clang_type.get_kind();
        println!("type: {:?}", clang_type);
        match kind {
            TypeKind::Typedef => {
                let child = children.next().unwrap();
                if is_entity_objc_instancetype(child) {
                    return ObjCType::ObjCInstancetype;
                }

                let declaration = clang_type.get_declaration().unwrap();
                let decl_children = declaration.get_children();
                let mut decl_children_iter = decl_children.iter().peekable();
                let objc_type =
                    ObjCType::from(&clang_type.get_canonical_type(), &mut decl_children_iter);
                assert_eq!(decl_children_iter.peek(), None);
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
                    TypeKind::Unexposed => Self::from_unexposed_type(clang_type, children),
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
            TypeKind::Unexposed => Self::from_unexposed_type(clang_type, children),
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
            TypeKind::Pointer => {
                let pointee_type = clang_type.get_pointee_type().unwrap();
                if pointee_type.get_result_type() != None
                    && [TypeKind::Unexposed, TypeKind::FunctionPrototype]
                        .contains(&pointee_type.get_kind())
                {
                    let callable_decl = CallableDecl::from_type(&pointee_type, children);
                    ObjCType::FnPtr(Box::new(callable_decl))
                } else {
                    ObjCType::Ptr(Box::new(Self::from(&pointee_type, children)))
                }
            }
            // TypeKind::Elaborated => {
            //     let child = children.next().unwrap();
            //     assert_eq!(child.get_type().unwrap().get_kind(), TypeKind::Record);
            //     ObjCType::Void // TODO
            // }
            TypeKind::ULong => ObjCType::ULong,
            TypeKind::BlockPointer => {
                let callable_decl =
                    CallableDecl::from_type(&clang_type.get_pointee_type().unwrap(), children);
                ObjCType::BlockPtr(Box::new(callable_decl))
            }
            _ => {
                panic!(
                    "Unimplemented type {:?} - {:?}",
                    kind,
                    clang_type.get_display_name()
                );
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
    fn from<'a, 'tu: 'a, I: Iterator<Item = &'a Entity<'tu>>>(
        entity: &Entity,
        children: &mut Peekable<I>,
    ) -> ObjCMethodArg {
        assert_eq!(entity.get_kind(), EntityKind::ParmDecl);
        let entity_type = entity.get_type().unwrap();
        let objc_type = ObjCType::from(&entity_type, children);
        assert_eq!(children.peek(), None);
        ObjCMethodArg {
            name: entity.get_name().unwrap(),
            objc_type,
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

fn find_property_at_same_location<'a>(
    type_entity: &Entity<'a>,
    searched_for: &Entity,
) -> Option<Entity<'a>> {
    assert!(
        [EntityKind::ObjCInterfaceDecl, EntityKind::ObjCProtocolDecl]
            .contains(&type_entity.get_kind())
    );
    let location = searched_for.get_location().unwrap();
    let children = type_entity.get_children();
    children
        .into_iter()
        .filter(|sibling| sibling.get_kind() == EntityKind::ObjCPropertyDecl)
        .find(|sibling| sibling.get_location().unwrap() == location)
}

fn method_source_property<'a>(entity: &Entity<'a>) -> Option<Entity<'a>> {
    assert!(
        [
            EntityKind::ObjCInstanceMethodDecl,
            EntityKind::ObjCClassMethodDecl,
        ].contains(&entity.get_kind())
    );
    let arg_count = entity.get_arguments().unwrap().len();
    if arg_count > 1 {
        return None;
    }
    let children = entity.get_children();
    // TODO: Might be able to simplify this condition
    if children.iter().any(|child| {
        [
            EntityKind::ObjCClassRef,
            EntityKind::TypeRef,
            EntityKind::ObjCProtocolRef,
        ].contains(&child.get_kind())
    }) {
        None
    } else {
        // No type children on the method means that either the method doesn't use any complex type, or it's generated from a property.
        // If it's generated from a property we have to get the children of that property instead.
        let parent = entity.get_lexical_parent().unwrap();
        find_property_at_same_location(&parent, entity)
    }
}

fn skip_entities<'a, 'tu: 'a, I: Iterator<Item = &'a Entity<'tu>>>(
    children: &mut Peekable<I>,
    kind: EntityKind,
) {
    loop {
        if let Some(child) = children.peek() {
            if child.get_kind() != kind {
                break;
            }
        } else {
            break;
        }
        let _ = children.next();
    }
}

impl ObjCMethod {
    fn from(entity: &Entity) -> ObjCMethod {
        assert!(
            [
                EntityKind::ObjCInstanceMethodDecl,
                EntityKind::ObjCClassMethodDecl,
            ].contains(&entity.get_kind())
        );
        let mut args: Vec<ObjCMethodArg> = Vec::new();
        let ret_type;
        if let Some(source_property) = method_source_property(entity) {
            let property_children = source_property.get_children();
            let mut property_children_peekable = property_children.iter().peekable();
            skip_entities(&mut property_children_peekable, EntityKind::UnexposedAttr);

            ret_type = ObjCType::from(
                &entity.get_result_type().unwrap(),
                &mut property_children_peekable,
            );

            // When it's generated from a property, no real child on the method's entity.
            for source_arg in entity.get_arguments().unwrap() {
                let arg = ObjCMethodArg::from(&source_arg, &mut property_children_peekable);
                args.push(arg);
            }
            assert_eq!(property_children_peekable.peek(), None);
        } else {
            let arg_count = entity.get_arguments().unwrap().len();
            let children = entity.get_children();
            let mut children_peekable = children.iter().peekable();
            skip_entities(&mut children_peekable, EntityKind::UnexposedAttr);
            ret_type = ObjCType::from(&entity.get_result_type().unwrap(), &mut children_peekable);

            for _ in 0..arg_count {
                let child = children_peekable.next().unwrap();
                let grand_children = child.get_children();
                let mut grand_children_peekable = grand_children.iter().peekable();
                let arg = ObjCMethodArg::from(child, &mut grand_children_peekable);
                assert_eq!(grand_children_peekable.peek(), None);
                args.push(arg);
            }
            assert_eq!(children_peekable.peek(), None);
        }

        let kind = match entity.get_kind() {
            EntityKind::ObjCInstanceMethodDecl => ObjCMethodKind::InstanceMethod,
            EntityKind::ObjCClassMethodDecl => ObjCMethodKind::ClassMethod,
            _ => unreachable!(),
        };

        ObjCMethod {
            kind,
            is_optional: entity.is_objc_optional(),
            sel: entity.get_name().unwrap(),
            args,
            ret_type,
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

    if let Some(location) = entity.get_location() {
        println!("{}location: {:?}", indent, location);
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

    if let Some(typedef_underlying_type) = entity.get_typedef_underlying_type() {
        println!(
            "{}typedef underlying type: {:?}",
            indent, typedef_underlying_type
        );
    }

    if let Some(visibility) = entity.get_visibility() {
        println!("{}visibility: {:?}", indent, visibility);
    }

    if let Some(result_type) = entity.get_result_type() {
        println!("{}result type: {:?}", indent, result_type);
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

    if let Some(objc_attributes) = entity.get_objc_attributes() {
        println!("{}objc attributes: {:?}", indent, objc_attributes);
    }

    if let Some(reference) = entity.get_reference() {
        println!("{}reference: {:?}", indent, reference);
    }

    if let Some(storage_class) = entity.get_storage_class() {
        println!("{}storage class: {:?}", indent, storage_class);
    }

    if let Some(semantic_parent) = entity.get_semantic_parent() {
        println!("{}semantic parent: {:?}", indent, semantic_parent);
    }

    if let Some(lexical_parent) = entity.get_lexical_parent() {
        println!("{}lexical parent: {:?}", indent, lexical_parent);
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
        #import <Foundation/NSArray.h>
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
                    name: "A".to_owned(),
                    template_arguments: Vec::new(),
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "bar".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "hoge".to_owned(),
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
                    name: "A".to_owned(),
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "bar".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: true,
                            sel: "hoge".to_owned(),
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
                    name: "B".to_owned(),
                    template_arguments: Vec::new(),
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
                ObjCClass {
                    name: "A".to_owned(),
                    template_arguments: Vec::new(),
                    superclass_name: Some("B".to_owned()),
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "bar".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "hoge".to_owned(),
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
                    name: "A".to_owned(),
                    template_arguments: Vec::new(),
                    superclass_name: None,
                    adopted_protocol_names: vec!["B".to_owned()],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "hoge".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
            ],
            protocols: vec![
                ObjCProtocol {
                    name: "C".to_owned(),
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::Void,
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
                ObjCProtocol {
                    name: "B".to_owned(),
                    adopted_protocol_names: vec!["C".to_owned()],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "bar".to_owned(),
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
                    name: "A".to_owned(),
                    template_arguments: Vec::new(),
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr("A".to_owned(), Vec::new(), Vec::new()),
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
            @interface C<__covariant T, __covariant U>
            - (C<C<B<T>*, U>*, B<id>*> *)foo;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "B".to_owned(),
                    template_arguments: vec!["T".to_owned()],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "m1".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::TemplateArgument("T".to_owned()),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "m2".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr("B".to_owned(), vec![], vec![]),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "m3".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr(
                                "B".to_owned(),
                                vec![ObjCType::TemplateArgument("T".to_owned())],
                                vec![],
                            ),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "m4".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr(
                                "B".to_owned(),
                                vec![ObjCType::ObjCObjPtr("A".to_owned(), vec![], vec![])],
                                vec![],
                            ),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "m5".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr(
                                "B".to_owned(),
                                vec![
                                    ObjCType::ObjCObjPtr(
                                        "B".to_owned(),
                                        vec![ObjCType::ObjCObjPtr("A".to_owned(), vec![], vec![])],
                                        vec![],
                                    ),
                                ],
                                vec![],
                            ),
                        },
                    ],
                    guessed_origin: Origin::Unknown,
                },
                ObjCClass {
                    name: "C".to_owned(),
                    template_arguments: vec!["T".to_owned(), "U".to_owned()],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr(
                                "C".to_owned(),
                                vec![
                                    ObjCType::ObjCObjPtr(
                                        "C".to_owned(),
                                        vec![
                                            ObjCType::ObjCObjPtr(
                                                "B".to_owned(),
                                                vec![ObjCType::TemplateArgument("T".to_owned())],
                                                vec![],
                                            ),
                                            ObjCType::TemplateArgument("U".to_owned()),
                                        ],
                                        vec![],
                                    ),
                                    ObjCType::ObjCObjPtr(
                                        "B".to_owned(),
                                        vec![ObjCType::ObjCId(vec![])],
                                        vec![],
                                    ),
                                ],
                                vec![],
                            ),
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
    fn test_parameter_adopting_protocols() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @protocol P1, P2;
            @class B;
            @interface A
            - (void)foo:(id<P1, P2>)x;
            + (void)bar:(B<P2>*)y;
            - (id<P1, P2>)foobar:(B<P2>*)z;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "A".to_owned(),
                    template_arguments: vec![],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo:".to_owned(),
                            args: vec![
                                ObjCMethodArg {
                                    name: "x".to_owned(),
                                    objc_type: ObjCType::ObjCId(vec![
                                        "P1".to_owned(),
                                        "P2".to_owned(),
                                    ]),
                                },
                            ],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "bar:".to_owned(),
                            args: vec![
                                ObjCMethodArg {
                                    name: "y".to_owned(),
                                    objc_type: ObjCType::ObjCObjPtr(
                                        "B".to_owned(),
                                        vec![],
                                        vec!["P2".to_owned()],
                                    ),
                                },
                            ],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foobar:".to_owned(),
                            args: vec![
                                ObjCMethodArg {
                                    name: "z".to_owned(),
                                    objc_type: ObjCType::ObjCObjPtr(
                                        "B".to_owned(),
                                        vec![],
                                        vec!["P2".to_owned()],
                                    ),
                                },
                            ],
                            ret_type: ObjCType::ObjCId(vec!["P1".to_owned(), "P2".to_owned()]),
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

    // #[test] // TODO
    fn test_lightweight_generic_and_protocols() {
        let clang = Clang::new().expect("Could not load libclang");

        // The basic idea is probably that as lightweight generics have been added late in the language, something that was parsing correctly as protocol confirmance before should still be parsed as protocol conformance.
        let source = "
            @protocol Ambiguous1, P1, Ambiguous2, Ambiguous3;
            @class Ambiguous1;
            @class A;
            typedef A* Ambiguous2;
            typedef A* Ambiguous3;
            @interface B<__covariant T>
            @end
            @interface C<__covariant Ambiguous2, __covariant U>
            - (C<C<B<Ambiguous1*><Ambiguous1>*, U>*, B<id<P1>>*> *)foo;
            - (C<C<B<Ambiguous1*><Ambiguous1>*, U>*, B<A*><Ambiguous2>*> *)bar;
            - (B<Ambiguous3>*)hoge;
            - (C<A*, Ambiguous3>*)hogehoge;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "B".to_owned(),
                    template_arguments: vec!["T".to_owned()],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![],
                    guessed_origin: Origin::Unknown,
                },
                ObjCClass {
                    name: "C".to_owned(),
                    template_arguments: vec!["Ambiguous2".to_owned(), "U".to_owned()],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "foo".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr(
                                "C".to_owned(),
                                vec![
                                    ObjCType::ObjCObjPtr(
                                        "C".to_owned(),
                                        vec![
                                            ObjCType::ObjCObjPtr(
                                                "B".to_owned(),
                                                vec![
                                                    ObjCType::ObjCObjPtr(
                                                        "Ambiguous1".to_owned(),
                                                        vec![],
                                                        vec![],
                                                    ),
                                                ],
                                                vec!["Ambiguous1".to_owned()],
                                            ),
                                            ObjCType::TemplateArgument("U".to_owned()),
                                        ],
                                        vec![],
                                    ),
                                    ObjCType::ObjCObjPtr(
                                        "B".to_owned(),
                                        vec![ObjCType::ObjCId(vec!["P1".to_owned()])],
                                        vec![],
                                    ),
                                ],
                                vec![],
                            ),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "bar".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr(
                                "C".to_owned(),
                                vec![
                                    ObjCType::ObjCObjPtr(
                                        "C".to_owned(),
                                        vec![
                                            ObjCType::ObjCObjPtr(
                                                "B".to_owned(),
                                                vec![
                                                    ObjCType::ObjCObjPtr(
                                                        "Ambiguous1".to_owned(),
                                                        vec![],
                                                        vec![],
                                                    ),
                                                ],
                                                vec!["Ambiguous1".to_owned()],
                                            ),
                                            ObjCType::TemplateArgument("U".to_owned()),
                                        ],
                                        vec![],
                                    ),
                                    ObjCType::ObjCObjPtr(
                                        "B".to_owned(),
                                        vec![ObjCType::ObjCObjPtr("A".to_owned(), vec![], vec![])],
                                        vec!["Ambiguous2".to_owned()],
                                    ),
                                ],
                                vec![],
                            ),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "hoge".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr(
                                "B".to_owned(),
                                vec![],
                                vec!["Ambiguous3".to_owned()],
                            ),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "hogehoge".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCObjPtr(
                                "C".to_owned(),
                                vec![
                                    ObjCType::ObjCObjPtr("A".to_owned(), vec![], vec![]),
                                    ObjCType::ObjCObjPtr("A".to_owned(), vec![], vec![]),
                                ],
                                vec![],
                            ),
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
                    name: "A".to_owned(),
                    template_arguments: vec![],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "anInstancetype".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCInstancetype,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "anId".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCId(vec![]),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "aClass".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCClass,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "aSel".to_owned(),
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
    fn test_typedef_property() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            typedef unsigned long usize;
            @interface A
            @property (readwrite) usize anUnsignedInteger;
            @property (readonly) usize anUnsignedIntegerWithAvailability __attribute__((availability(macos,introduced=10.4)));
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "A".to_owned(),
                    template_arguments: vec![],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "anUnsignedInteger".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ULong,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "setAnUnsignedInteger:".to_owned(),
                            args: vec![
                                ObjCMethodArg {
                                    name: "anUnsignedInteger".to_owned(),
                                    objc_type: ObjCType::ULong,
                                },
                            ],
                            ret_type: ObjCType::Void,
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "anUnsignedIntegerWithAvailability".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ULong,
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
    fn test_method_with_attribute() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            @interface A
            - (instancetype)init __attribute__((objc_designated_initializer));
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "A".to_owned(),
                    template_arguments: vec![],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::InstanceMethod,
                            is_optional: false,
                            sel: "init".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::ObjCInstancetype,
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
    fn test_function_pointer_return_value() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            typedef unsigned long usize;
            typedef usize(*FN_PTR)(id, ...);
            @interface A
            + (usize(*)(id, ...))directFnPtr;
            + (FN_PTR)typedefedFnPtr;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "A".to_owned(),
                    template_arguments: vec![],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "directFnPtr".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::FnPtr(Box::new(CallableDecl {
                                args: vec![(None, ObjCType::ObjCId(vec![]))],
                                ret_type: ObjCType::ULong,
                                is_variadic: true,
                            })),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "typedefedFnPtr".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::FnPtr(Box::new(CallableDecl {
                                args: vec![(None, ObjCType::ObjCId(vec![]))],
                                ret_type: ObjCType::ULong,
                                is_variadic: true,
                            })),
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
    fn test_block_ptr_return_value() {
        let clang = Clang::new().expect("Could not load libclang");

        let source = "
            typedef unsigned long usize;
            typedef usize(^BLOCK_PTR)(id, ...);
            @interface A
            + (usize(^)(id, ...))directBlockPtr;
            + (BLOCK_PTR)typedefedBlockPtr;
            @end
        ";

        let expected_decls = ObjCDecls {
            classes: vec![
                ObjCClass {
                    name: "A".to_owned(),
                    template_arguments: vec![],
                    superclass_name: None,
                    adopted_protocol_names: vec![],
                    methods: vec![
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "directBlockPtr".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::BlockPtr(Box::new(CallableDecl {
                                args: vec![(None, ObjCType::ObjCId(vec![]))],
                                ret_type: ObjCType::ULong,
                                is_variadic: true,
                            })),
                        },
                        ObjCMethod {
                            kind: ObjCMethodKind::ClassMethod,
                            is_optional: false,
                            sel: "typedefedBlockPtr".to_owned(),
                            args: vec![],
                            ret_type: ObjCType::BlockPtr(Box::new(CallableDecl {
                                args: vec![(None, ObjCType::ObjCId(vec![]))],
                                ret_type: ObjCType::ULong,
                                is_variadic: true,
                            })),
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
            Origin::Framework("Foundation".to_owned()),
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/System/Library/Frameworks/Metal.framework/Headers/MTLCaptureManager.h"),
            Origin::Framework("Metal".to_owned()),
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/objc/NSObject.h"),
            Origin::ObjCCore,
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/dispatch/object.h"),
            Origin::Library("dispatch".to_owned()),
        );
        assert_eq!(
            guess_origin("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/dispatch/queue.h"),
            Origin::Library("dispatch".to_owned()),
        );
    }
}
