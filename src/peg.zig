pub fn Parser(comptime ParserGenerator: type) type {
    return struct {
        pub const Grammar = struct {
            pub usingnamespace ParserGenerator.Group(.{
                Spacing,
                ParserGenerator.OneOrMore(Definition),
                EndOfFile,
            });
        };

        pub const Definition = struct {
            pub usingnamespace ParserGenerator.Group(.{
                Identifier,
                LEFTARROW,
                Expression,
            });
        };

        pub const Expression = struct {
            pub usingnamespace ParserGenerator.Group(.{
                Sequence,
                ParserGenerator.ZeroOrMore(ParserGenerator.Group(.{
                    SLASH,
                    Sequence,
                })),
            });
        };

        pub const Sequence = struct {
            pub usingnamespace ParserGenerator.Select(.{
                ParserGenerator.Group(.{
                    Prefix,
                    ParserGenerator.ZeroOrMore(Prefix),
                }),
                ParserGenerator.Group(.{}),
            });
        };

        pub const Prefix = struct {
            pub usingnamespace ParserGenerator.Select(.{
                ParserGenerator.Group(.{
                    AND,
                    Suffix,
                }),
                ParserGenerator.Group(.{
                    NOT,
                    Suffix,
                }),
                Suffix,
            });
        };

        pub const Suffix = struct {
            pub usingnamespace ParserGenerator.Group(.{
                Primary,
                ParserGenerator.Optional(ParserGenerator.Select(.{
                    QUESTION,
                    STAR,
                    PLUS,
                })),
            });
        };

        pub const Primary = struct {
            pub usingnamespace ParserGenerator.Select(.{
                ParserGenerator.Group(.{
                    Identifier,
                    ParserGenerator.Negative(LEFTARROW),
                }),
                ParserGenerator.Group(.{
                    OPEN,
                    Expression,
                    CLOSE,
                }),
                Literal,
                Class,
                DOT,
            });
        };

        pub const Identifier = struct {
            pub usingnamespace ParserGenerator.Group(.{
                IdentStart,
                ParserGenerator.ZeroOrMore(IdentCont),
                Spacing,
            });
        };

        pub const IdentStart = struct {
            pub usingnamespace ParserGenerator.AnyOf(.{
                'a',
                'b',
                'c',
                'd',
                'e',
                'f',
                'g',
                'h',
                'i',
                'j',
                'k',
                'l',
                'm',
                'n',
                'o',
                'p',
                'q',
                'r',
                's',
                't',
                'u',
                'v',
                'w',
                'x',
                'y',
                'z',
                'A',
                'B',
                'C',
                'D',
                'E',
                'F',
                'G',
                'H',
                'I',
                'J',
                'K',
                'L',
                'M',
                'N',
                'O',
                'P',
                'Q',
                'R',
                'S',
                'T',
                'U',
                'V',
                'W',
                'X',
                'Y',
                'Z',
                '_',
            });
        };

        pub const IdentCont = struct {
            pub usingnamespace ParserGenerator.Select(.{
                IdentStart,
                ParserGenerator.AnyOf(.{
                    '0',
                    '1',
                    '2',
                    '3',
                    '4',
                    '5',
                    '6',
                    '7',
                    '8',
                    '9',
                }),
            });
        };

        pub const Literal = struct {
            pub usingnamespace ParserGenerator.Select(.{
                ParserGenerator.Group(.{
                    ParserGenerator.AnyOf(.{
                        '\'',
                    }),
                    ParserGenerator.ZeroOrMore(ParserGenerator.Group(.{
                        ParserGenerator.Negative(ParserGenerator.AnyOf(.{
                            '\'',
                        })),
                        Char,
                    })),
                    ParserGenerator.AnyOf(.{
                        '\'',
                    }),
                    Spacing,
                }),
                ParserGenerator.Group(.{
                    ParserGenerator.AnyOf(.{
                        '"',
                    }),
                    ParserGenerator.ZeroOrMore(ParserGenerator.Group(.{
                        ParserGenerator.Negative(ParserGenerator.AnyOf(.{
                            '"',
                        })),
                        Char,
                    })),
                    ParserGenerator.AnyOf(.{
                        '"',
                    }),
                    Spacing,
                }),
            });
        };

        pub const Class = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String("["),
                ParserGenerator.ZeroOrMore(ParserGenerator.Group(.{
                    ParserGenerator.Negative(ParserGenerator.String("]")),
                    Range,
                })),
                ParserGenerator.String("]"),
                Spacing,
            });
        };

        pub const Range = struct {
            pub usingnamespace ParserGenerator.Select(.{
                ParserGenerator.Group(.{
                    Char,
                    ParserGenerator.String("-"),
                    Char,
                }),
                Char,
            });
        };

        pub const Char = struct {
            pub usingnamespace ParserGenerator.Select(.{
                ParserGenerator.Group(.{
                    ParserGenerator.String("\\"),
                    ParserGenerator.AnyOf(.{
                        'a',
                        'b',
                        'e',
                        'f',
                        'n',
                        'r',
                        't',
                        'v',
                        '\'',
                        '"',
                        '[',
                        ']',
                        '\\',
                    }),
                }),
                ParserGenerator.Group(.{
                    ParserGenerator.String("\\"),
                    ParserGenerator.AnyOf(.{
                        '0',
                        '1',
                        '2',
                        '3',
                    }),
                    ParserGenerator.AnyOf(.{
                        '0',
                        '1',
                        '2',
                        '3',
                        '4',
                        '5',
                        '6',
                        '7',
                    }),
                    ParserGenerator.AnyOf(.{
                        '0',
                        '1',
                        '2',
                        '3',
                        '4',
                        '5',
                        '6',
                        '7',
                    }),
                }),
                ParserGenerator.Group(.{
                    ParserGenerator.String("\\"),
                    ParserGenerator.AnyOf(.{
                        '0',
                        '1',
                        '2',
                        '3',
                        '4',
                        '5',
                        '6',
                        '7',
                    }),
                    ParserGenerator.Optional(ParserGenerator.AnyOf(.{
                        '0',
                        '1',
                        '2',
                        '3',
                        '4',
                        '5',
                        '6',
                        '7',
                    })),
                }),
                ParserGenerator.Group(.{
                    ParserGenerator.String("\\"),
                    ParserGenerator.String("-"),
                }),
                ParserGenerator.Group(.{
                    ParserGenerator.Negative(ParserGenerator.String("\\")),
                    ParserGenerator.Any(),
                }),
            });
        };

        pub const LEFTARROW = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String("<-"),
                Spacing,
            });
        };

        pub const SLASH = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String("/"),
                Spacing,
            });
        };

        pub const AND = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String("&"),
                Spacing,
            });
        };

        pub const NOT = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String("!"),
                Spacing,
            });
        };

        pub const QUESTION = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String("?"),
                Spacing,
            });
        };

        pub const STAR = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String("*"),
                Spacing,
            });
        };

        pub const PLUS = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String("+"),
                Spacing,
            });
        };

        pub const OPEN = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String("("),
                Spacing,
            });
        };

        pub const CLOSE = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String(")"),
                Spacing,
            });
        };

        pub const DOT = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String("."),
                Spacing,
            });
        };

        pub const Spacing = struct {
            pub usingnamespace ParserGenerator.ZeroOrMore(ParserGenerator.Select(.{
                Space,
                Comment,
            }));
        };

        pub const Comment = struct {
            pub usingnamespace ParserGenerator.Group(.{
                ParserGenerator.String("#"),
                ParserGenerator.ZeroOrMore(ParserGenerator.Group(.{
                    ParserGenerator.Negative(EndOfLine),
                    ParserGenerator.Any(),
                })),
                EndOfLine,
            });
        };

        pub const Space = struct {
            pub usingnamespace ParserGenerator.Select(.{
                ParserGenerator.String(" "),
                ParserGenerator.String("\t"),
                EndOfLine,
            });
        };

        pub const EndOfLine = struct {
            pub usingnamespace ParserGenerator.Select(.{
                ParserGenerator.String("\r\n"),
                ParserGenerator.String("\n"),
                ParserGenerator.String("\r"),
            });
        };

        pub const EndOfFile = struct {
            pub usingnamespace ParserGenerator.Negative(ParserGenerator.Any());
        };
    };
}
