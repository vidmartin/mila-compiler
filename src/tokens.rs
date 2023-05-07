use core::fmt;


#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Token {
    /// the "program" keyword
    /// used once on the first line of the program
    KwProgram, 
    /// the "var" keyword
    /// used to declare variables
    KwVar,
    /// the "array" keyword
    /// used to describe array types
    KwArray,
    /// the "of" keyword
    /// used as part of a for statement
    KwOf,
    /// the "begin" keyword
    /// used to introduce a new block of code
    KwBegin,
    /// the "end" keyword
    /// used to conclude a block of code
    KwEnd,
    /// the "for" keyword
    /// used to define a for statement
    KwFor,
    /// the "do" keyword
    /// used as part of other statements
    KwDo,
    /// the "if" keyword
    /// used to define an if statement
    KwIf,
    /// the "if" keyword
    /// used as part of an if statement
    KwThen,
    /// the "to" keyword
    /// used as part of a for statement
    KwTo,
    /// the "downto" keyword
    /// used as part of a for statement
    KwDownto,
    /// the "div" keyword
    /// used as division operator
    KwDiv,
    /// the "mod" keyword
    /// used as modulo operator
    KwMod,
    /// the "const" keyword
    /// used to declare constants
    KwConst,
    /// the "function" keyword
    /// used to declare functions
    KwFunction,
    /// the "while" keyword
    /// used to define while statements
    KwWhile,
    /// the "procedure" keyword
    /// used to define procedures
    KwProcedure,
    /// the "exit" keyword
    /// used to return control from functions / procedures
    KwExit,
    /// the "or" keyword
    /// used as logical OR operator
    KwOr,
    /// the "and" keyword
    /// used as logical AND operator
    KwAnd,
    /// the "not" keyword
    /// used as logical NOT operator
    KwNot,
    /// the "forward" keyword
    /// used for forward declarations
    KwForward, // TODO: update lexer

    /// the ":" token
    /// used to annotate data types
    TkColon,
    /// the ";" token
    /// used to separate statements
    TkSemicolon,
    /// the ":=" token
    /// used to assign values to variables
    TkAssign,
    /// the "[" token
    /// used to declare arrays, access arrays
    TkSqOpen,
    /// the "]" token
    /// used to declare arrays, access arrays
    TkSqClose,
    /// the "(" token
    /// used to indicate priority of operations, call functions, declare functions, ...
    TkParOpen,
    /// the ")" token
    /// used to indicate priority of operations, call functions, declare functions, ...
    TkParClose,
    /// the ".." token
    /// used to declare arrays
    TkDotDot,
    /// the "." token
    /// used to mark the end of file
    TkDot,
    /// the "<" token
    /// used as 'less than' operator
    TkLess,
    /// the ">" token
    /// used as 'more than' operator
    TkMore,
    /// the "=" token
    /// used as 'equals' operator, and to define constants
    TkEq,
    /// the "<>" token
    /// used as 'not equals' operator
    TkNotEq,
    /// the "<=" token
    /// used as 'less than or equal' operator
    TkLessOrEq,
    /// the ">=" token
    /// used as 'more than or equal' operator
    TkMoreOrEq,
    /// the "*" token
    /// used as multiplication operator
    TkMul,
    /// the "+" token
    /// used as addition operator
    TkAdd,
    /// the "-" token
    /// used as subtraction operator
    TkSub,
    /// the "," token
    /// used to separate variable names
    TkComma,

    /// any valid identifier that is not also a keyword
    /// might refer to a function, variable, constant, data type, ...
    Ident(String),

    /// integer literal
    LitInt(i64),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{:?}>", *self)
    }
}

pub fn parse_kw(s: &str) -> Option<Token> {
    Some(
        match s {
            "program"   => Token::KwProgram,
            "var"       => Token::KwVar,
            "array"     => Token::KwArray,
            "of"        => Token::KwOf,
            "begin"     => Token::KwBegin,
            "end"       => Token::KwEnd,
            "for"       => Token::KwFor,
            "do"        => Token::KwDo,
            "if"        => Token::KwIf,
            "then"      => Token::KwThen,
            "to"        => Token::KwTo,
            "downto"    => Token::KwDownto,
            "div"       => Token::KwDiv,
            "mod"       => Token::KwMod,
            "const"     => Token::KwConst,
            "function"  => Token::KwFunction,
            "while"     => Token::KwWhile,
            "procedure" => Token::KwProcedure,
            "exit"      => Token::KwExit,
            "or"        => Token::KwOr,
            "and"       => Token::KwAnd,
            "not"       => Token::KwNot,
            _           => None?
        }
    )
}
