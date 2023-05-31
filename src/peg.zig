pub fn Parser(comptime ParserGenerator: type) type {
    return struct {
        pub const Grammar = struct {
            pub usingnamespace ParserGenerator.Group(.Grammar, .{
                Spacing,
                ParserGenerator.OneOrMore(.Grammar, Definition),
                EndOfFile,
            });
        };

        pub const Definition = struct {
            pub usingnamespace ParserGenerator.Group(.Definition, .{
                Identifier,
                LEFTARROW,
                Expression,
            });
        };

        pub const Expression = struct {
            pub usingnamespace ParserGenerator.Group(.Expression, .{
                Sequence,
                ParserGenerator.ZeroOrMore(.Expression, ParserGenerator.Group(.Expression, .{
                    SLASH,
                    Sequence,
                })),
            });
        };

        pub const Sequence = struct {
            pub usingnamespace ParserGenerator.Select(.Sequence, .{
                ParserGenerator.Group(.Sequence, .{
                    Prefix,
                    ParserGenerator.ZeroOrMore(.Sequence, Prefix),
                }),
                ParserGenerator.Group(.Sequence, .{}),
            });
        };

        pub const Prefix = struct {
            pub usingnamespace ParserGenerator.Select(.Prefix, .{
                ParserGenerator.Group(.Prefix, .{
                    AND,
                    Suffix,
                }),
                ParserGenerator.Group(.Prefix, .{
                    NOT,
                    Suffix,
                }),
                Suffix,
            });
        };

        pub const Suffix = struct {
            pub usingnamespace ParserGenerator.Group(.Suffix, .{
                Primary,
                ParserGenerator.Optional(.Suffix, ParserGenerator.Select(.Suffix, .{
                    QUESTION,
                    STAR,
                    PLUS,
                })),
            });
        };

        pub const Primary = struct {
            pub usingnamespace ParserGenerator.Select(.Primary, .{
                ParserGenerator.Group(.Primary, .{
                    Identifier,
                    ParserGenerator.Negative(.Primary, LEFTARROW),
                }),
                ParserGenerator.Group(.Primary, .{
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
            pub usingnamespace ParserGenerator.Group(.Identifier, .{
                IdentStart,
                ParserGenerator.ZeroOrMore(.Identifier, IdentCont),
                Spacing,
            });
        };

        pub const IdentStart = struct {
            pub usingnamespace ParserGenerator.AnyOf(.IdentStart, .{
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
            pub usingnamespace ParserGenerator.Select(.IdentCont, .{
                IdentStart,
                ParserGenerator.AnyOf(.IdentCont, .{
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
            pub usingnamespace ParserGenerator.Select(.Literal, .{
                ParserGenerator.Group(.Literal, .{
                    ParserGenerator.AnyOf(.Literal, .{
                        '\'',
                    }),
                    ParserGenerator.ZeroOrMore(.Literal, ParserGenerator.Group(.Literal, .{
                        ParserGenerator.Negative(.Literal, ParserGenerator.AnyOf(.Literal, .{
                            '\'',
                        })),
                        Char,
                    })),
                    ParserGenerator.AnyOf(.Literal, .{
                        '\'',
                    }),
                    Spacing,
                }),
                ParserGenerator.Group(.Literal, .{
                    ParserGenerator.AnyOf(.Literal, .{
                        '"',
                    }),
                    ParserGenerator.ZeroOrMore(.Literal, ParserGenerator.Group(.Literal, .{
                        ParserGenerator.Negative(.Literal, ParserGenerator.AnyOf(.Literal, .{
                            '"',
                        })),
                        Char,
                    })),
                    ParserGenerator.AnyOf(.Literal, .{
                        '"',
                    }),
                    Spacing,
                }),
            });
        };

        pub const Class = struct {
            pub usingnamespace ParserGenerator.Group(.Class, .{
                ParserGenerator.String(.Class, "["),
                ParserGenerator.ZeroOrMore(.Class, ParserGenerator.Group(.Class, .{
                    ParserGenerator.Negative(.Class, ParserGenerator.String(.Class, "]")),
                    Range,
                })),
                ParserGenerator.String(.Class, "]"),
                Spacing,
            });
        };

        pub const Range = struct {
            pub usingnamespace ParserGenerator.Select(.Range, .{
                ParserGenerator.Group(.Range, .{
                    Char,
                    ParserGenerator.String(.Range, "-"),
                    Char,
                }),
                Char,
            });
        };

        pub const Char = struct {
            pub usingnamespace ParserGenerator.Select(.Char, .{
                ParserGenerator.Group(.Char, .{
                    ParserGenerator.String(.Char, "\\"),
                    ParserGenerator.AnyOf(.Char, .{
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
                ParserGenerator.Group(.Char, .{
                    ParserGenerator.String(.Char, "\\"),
                    ParserGenerator.AnyOf(.Char, .{
                        '0',
                        '1',
                        '2',
                        '3',
                    }),
                    ParserGenerator.AnyOf(.Char, .{
                        '0',
                        '1',
                        '2',
                        '3',
                        '4',
                        '5',
                        '6',
                        '7',
                    }),
                    ParserGenerator.AnyOf(.Char, .{
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
                ParserGenerator.Group(.Char, .{
                    ParserGenerator.String(.Char, "\\"),
                    ParserGenerator.AnyOf(.Char, .{
                        '0',
                        '1',
                        '2',
                        '3',
                        '4',
                        '5',
                        '6',
                        '7',
                    }),
                    ParserGenerator.Optional(.Char, ParserGenerator.AnyOf(.Char, .{
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
                ParserGenerator.Group(.Char, .{
                    ParserGenerator.String(.Char, "\\"),
                    ParserGenerator.String(.Char, "-"),
                }),
                ParserGenerator.Group(.Char, .{
                    ParserGenerator.Negative(.Char, ParserGenerator.String(.Char, "\\")),
                    ParserGenerator.Any(.Char),
                }),
            });
        };

        pub const LEFTARROW = struct {
            pub usingnamespace ParserGenerator.Group(.LEFTARROW, .{
                ParserGenerator.String(.LEFTARROW, "<-"),
                Spacing,
            });
        };

        pub const SLASH = struct {
            pub usingnamespace ParserGenerator.Group(.SLASH, .{
                ParserGenerator.String(.SLASH, "/"),
                Spacing,
            });
        };

        pub const AND = struct {
            pub usingnamespace ParserGenerator.Group(.AND, .{
                ParserGenerator.String(.AND, "&"),
                Spacing,
            });
        };

        pub const NOT = struct {
            pub usingnamespace ParserGenerator.Group(.NOT, .{
                ParserGenerator.String(.NOT, "!"),
                Spacing,
            });
        };

        pub const QUESTION = struct {
            pub usingnamespace ParserGenerator.Group(.QUESTION, .{
                ParserGenerator.String(.QUESTION, "?"),
                Spacing,
            });
        };

        pub const STAR = struct {
            pub usingnamespace ParserGenerator.Group(.STAR, .{
                ParserGenerator.String(.STAR, "*"),
                Spacing,
            });
        };

        pub const PLUS = struct {
            pub usingnamespace ParserGenerator.Group(.PLUS, .{
                ParserGenerator.String(.PLUS, "+"),
                Spacing,
            });
        };

        pub const OPEN = struct {
            pub usingnamespace ParserGenerator.Group(.OPEN, .{
                ParserGenerator.String(.OPEN, "("),
                Spacing,
            });
        };

        pub const CLOSE = struct {
            pub usingnamespace ParserGenerator.Group(.CLOSE, .{
                ParserGenerator.String(.CLOSE, ")"),
                Spacing,
            });
        };

        pub const DOT = struct {
            pub usingnamespace ParserGenerator.Group(.DOT, .{
                ParserGenerator.String(.DOT, "."),
                Spacing,
            });
        };

        pub const Spacing = struct {
            pub usingnamespace ParserGenerator.ZeroOrMore(.Spacing, ParserGenerator.Select(.Spacing, .{
                Space,
                Comment,
            }));
        };

        pub const Comment = struct {
            pub usingnamespace ParserGenerator.Group(.Comment, .{
                ParserGenerator.String(.Comment, "#"),
                ParserGenerator.ZeroOrMore(.Comment, ParserGenerator.Group(.Comment, .{
                    ParserGenerator.Negative(.Comment, EndOfLine),
                    ParserGenerator.Any(.Comment),
                })),
                EndOfLine,
            });
        };

        pub const Space = struct {
            pub usingnamespace ParserGenerator.Select(.Space, .{
                ParserGenerator.String(.Space, " "),
                ParserGenerator.String(.Space, "\t"),
                EndOfLine,
            });
        };

        pub const EndOfLine = struct {
            pub usingnamespace ParserGenerator.Select(.EndOfLine, .{
                ParserGenerator.String(.EndOfLine, "\r\n"),
                ParserGenerator.String(.EndOfLine, "\n"),
                ParserGenerator.String(.EndOfLine, "\r"),
            });
        };

        pub const EndOfFile = struct {
            pub usingnamespace ParserGenerator.Negative(.EndOfFile, ParserGenerator.Any(.EndOfFile));
        };
    };
}
