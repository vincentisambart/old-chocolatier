use nom;

#[derive(Debug, PartialEq)]
enum ObjCUnexposedType {
    Ptr(Box<ObjCUnexposedType>),
    Name(String, Vec<ObjCUnexposedType>),
}

named!(read_identifier<&str, &str>,
    is_not_s!(" \t\r\n<>,-+*")
);

named!(read_unqualified_type<&str, ObjCUnexposedType>,
    ws!(
        alt_complete!(
            do_parse!(
                identifier: read_identifier >>
                list: delimited!(
                    tag_s!("<"),
                    separated_list!(tag_s!(","), read_qualified_type),
                    tag_s!(">")
                ) >>
                (ObjCUnexposedType::Name(identifier.to_owned(), list))
            ) |
            do_parse!(
                identifier: read_identifier >>
                (ObjCUnexposedType::Name(identifier.to_owned(), Vec::new()))
            )
        )
    )
);

named!(read_qualified_type<&str, ObjCUnexposedType>,
    ws!(
        do_parse!(
            unqualified_type: read_unqualified_type >>
            res: fold_many0!(
                tag_s!("*"),
                unqualified_type,
                |acc, _| ObjCUnexposedType::Ptr(Box::new(acc))
            ) >>
            (res)
        )
    )
);

fn parse_unexposed_type(text: &str) -> Option<ObjCUnexposedType> {
    match read_qualified_type(text) {
        nom::IResult::Done("", qualified_type) => Some(qualified_type),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_unexposed_type() {
        assert_eq!(
            parse_unexposed_type("A"),
            Some(ObjCUnexposedType::Name("A".to_owned(), vec![]))
        );
        assert_eq!(
            parse_unexposed_type("A*"),
            Some(ObjCUnexposedType::Ptr(Box::new(ObjCUnexposedType::Name(
                "A".to_owned(),
                vec![]
            ))))
        );
        assert_eq!(parse_unexposed_type("A+"), None);
        assert_eq!(
            parse_unexposed_type("A<B*, C<X, Y> * >  "),
            Some(ObjCUnexposedType::Name(
                "A".to_owned(),
                vec![
                    ObjCUnexposedType::Ptr(Box::new(ObjCUnexposedType::Name(
                        "B".to_owned(),
                        vec![],
                    ))),
                    ObjCUnexposedType::Ptr(Box::new(ObjCUnexposedType::Name(
                        "C".to_owned(),
                        vec![
                            ObjCUnexposedType::Name("X".to_owned(), vec![]),
                            ObjCUnexposedType::Name("Y".to_owned(), vec![]),
                        ],
                    ))),
                ]
            ))
        );
    }
}
