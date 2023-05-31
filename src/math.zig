pub fn Parser(comptime ParserGenerator: type) type {
    return struct {
        pub const Expr = struct {
            pub usingnamespace ParserGenerator.Group(.Expr, .{
                Factor,
                ParserGenerator.ZeroOrMore(.Expr, ParserGenerator.Group(.Expr, .{
                    ParserGenerator.AnyOf(.Expr, .{
                        '+',
                        '-',
                    }),
                    Factor,
                })),
            });
        };

        pub const Factor = struct {
            pub usingnamespace ParserGenerator.Group(.Factor, .{
                Term,
                ParserGenerator.ZeroOrMore(.Factor, ParserGenerator.Group(.Factor, .{
                    ParserGenerator.AnyOf(.Factor, .{
                        '*',
                        '/',
                    }),
                    Term,
                })),
            });
        };

        pub const Term = struct {
            pub usingnamespace ParserGenerator.Select(.Term, .{
                Number,
                ParserGenerator.Group(.Term, .{
                    ParserGenerator.String(.Term, "("),
                    Expr,
                    ParserGenerator.String(.Term, ")"),
                }),
            });
        };

        pub const Number = struct {
            pub usingnamespace ParserGenerator.OneOrMore(.Number, ParserGenerator.AnyOf(.Number, .{
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
            }));
        };
    };
}
