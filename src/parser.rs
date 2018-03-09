use nom;

#[derive(Debug, PartialEq)]
pub struct ParsedEntity {
    name: String,
    conditions: Vec<ParsedEntity>,
    is_ptr: bool,
}

impl ParsedEntity {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn conditions(&self) -> &[ParsedEntity] {
        &self.conditions
    }

    pub fn is_ptr(&self) -> bool {
        self.is_ptr
    }
}

named!(read_identifier<&str, &str>,
    is_not_s!(" \t\r\n<>,-+*")
);

named!(read_type<&str, ParsedEntity>,
    ws!(
        alt_complete!(
            do_parse!(
                name: read_identifier >>
                conditions: delimited!(
                    tag_s!("<"),
                    separated_list!(tag_s!(","), read_type),
                    tag_s!(">")
                ) >>
                tag_s!("*") >>
                (ParsedEntity {
                    name: name.to_owned(),
                    conditions,
                    is_ptr: true
                })
            ) |
            // TODO: Might be able to use conditions (cond!()?) for simplifying things
            do_parse!(
                name: read_identifier >>
                conditions: delimited!(
                    tag_s!("<"),
                    separated_list!(tag_s!(","), read_type),
                    tag_s!(">")
                ) >>
                (ParsedEntity {
                    name: name.to_owned(),
                    conditions,
                    is_ptr: false
                })
            ) |
            do_parse!(
                name: read_identifier >>
                tag_s!("*") >>
                (ParsedEntity {
                    name: name.to_owned(),
                    conditions: Vec::new(),
                    is_ptr: true
                })
            ) |
            do_parse!(
                name: read_identifier >>
                (ParsedEntity {
                    name: name.to_owned(),
                    conditions: Vec::new(),
                    is_ptr: false
                })
            )
        )
    )
);

pub fn parse_unexposed_type(text: &str) -> Option<ParsedEntity> {
    match read_type(text) {
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
            Some(ParsedEntity {
                name: "A".to_owned(),
                conditions: vec![],
                is_ptr: false,
            })
        );
        assert_eq!(
            parse_unexposed_type("B *   "),
            Some(ParsedEntity {
                name: "B".to_owned(),
                conditions: vec![],
                is_ptr: true,
            })
        );
        assert_eq!(parse_unexposed_type("A+"), None);
        assert_eq!(
            parse_unexposed_type("id<A *, B>"),
            Some(ParsedEntity {
                name: "id".to_owned(),
                conditions: vec![
                    ParsedEntity {
                        name: "A".to_owned(),
                        conditions: vec![],
                        is_ptr: true,
                    },
                    ParsedEntity {
                        name: "B".to_owned(),
                        conditions: vec![],
                        is_ptr: false,
                    },
                ],
                is_ptr: false,
            })
        );
        assert_eq!(
            parse_unexposed_type("A<B*, C<X, Y> * > * "),
            Some(ParsedEntity {
                name: "A".to_owned(),
                conditions: vec![
                    ParsedEntity {
                        name: "B".to_owned(),
                        conditions: vec![],
                        is_ptr: true,
                    },
                    ParsedEntity {
                        name: "C".to_owned(),
                        conditions: vec![
                            ParsedEntity {
                                name: "X".to_owned(),
                                conditions: vec![],
                                is_ptr: false,
                            },
                            ParsedEntity {
                                name: "Y".to_owned(),
                                conditions: vec![],
                                is_ptr: false,
                            },
                        ],
                        is_ptr: true,
                    },
                ],
                is_ptr: true,
            })
        );
    }
}
