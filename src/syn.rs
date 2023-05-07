
use crate::tokens::*;
use crate::ast::*;
use std::iter::Peekable;

#[derive(Debug)]
pub enum SyntaxError {
    Expected { expected: Token, gotten: Token },
    ExpectedIdentifier { gotten: Token },
    ExpectedIntLiteral { gotten: Token },
    Unexpected(Token),
    UnexpectedEnd,
}

pub type ParseResult<TNode> = Result<TNode, SyntaxError>;

struct Parser<'a, TLex : Iterator<Item = Token>> {
    lex: &'a mut Peekable<TLex>,
}

impl<'a, TLex : Iterator<Item = Token>> Parser<'a, TLex> {
    pub fn new(lex: &'a mut Peekable<TLex>) -> Self {
        Self { lex: lex }
    }

    fn expect_token(&mut self, token: &Token) -> ParseResult<()> {
        match self.lex.next() {
            Some(tok) => {
                if tok == *token {
                    Ok(())
                } else {
                    Err(SyntaxError::Expected {
                        expected: token.clone(),
                        gotten: tok,
                    })
                }
            },
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }   

    fn expect_identifier(&mut self) -> ParseResult<String> {
        match self.lex.next() {
            Some(Token::Ident(s)) => Ok(s),
            Some(tok) => Err(SyntaxError::ExpectedIdentifier {
                gotten: tok,
            }),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    fn expect_int_lit(&mut self) -> ParseResult<i64> {
        match self.lex.next() {
            Some(Token::LitInt(i)) => Ok(i),
            Some(tok) => Err(SyntaxError::ExpectedIntLiteral {
                gotten: tok,
            }),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_program(&mut self) -> ParseResult<ProgramNode> {
        self.expect_token(&Token::KwProgram)?;
        let program_name = self.expect_identifier()?;
        self.expect_token(&Token::TkSemicolon)?;

        let mut declarations = self.parse_declarations()?;
        let main_block = self.parse_block()?;
        let main_func = CallableDeclarationNode {
            name: "main".to_string(),
            param_types: Vec::new(),
            variables: Vec::new(),
            return_type: Some("integer".to_string()),
            implementation: Some(main_block),
        };
        declarations.callables.push(main_func);

        self.expect_token(&Token::TkDot)?;

        return Ok(ProgramNode {
            declarations: declarations,
            name: program_name,
        });
    }

    pub fn parse_declarations(&mut self) -> ParseResult<ProgramDeclarations> {
        match self.lex.peek() {
            Some(Token::KwConst | Token::KwVar | Token::KwFunction | Token::KwProcedure) => {
                let first_declaration = self.parse_declaration()?;
                let mut rest_of_declarations = self.parse_declarations()?;
                match first_declaration {
                    Declaration::Variables(mut storage_nodes) => rest_of_declarations.variables.append(&mut storage_nodes),
                    Declaration::Constants(mut storage_nodes) => rest_of_declarations.constants.append(&mut storage_nodes),
                    Declaration::Function(callable_node) | Declaration::Procedure(callable_node) => rest_of_declarations.callables.push(callable_node),
                };
                Ok(rest_of_declarations)
            },
            Some(Token::KwBegin) => {
                Ok(ProgramDeclarations {
                    variables: Vec::new(),
                    constants: Vec::new(),
                    callables: Vec::new()
                })
            }
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_declaration(&mut self) -> ParseResult<Declaration> {
        match self.lex.peek() {
            Some(Token::KwFunction) => todo!(),
            Some(Token::KwProcedure) => todo!(),
            Some(Token::KwConst) => todo!(),
            Some(Token::KwVar) => Ok(Declaration::Variables(self.parse_variables()?)),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_variables(&mut self) -> ParseResult<Vec<StorageDeclarationNode>> {
        self.expect_token(&Token::KwVar)?;
        let mut names_with_type = self.parse_names_with_type()?;
        self.expect_token(&Token::TkSemicolon)?;
        let mut more_names_with_types_2 = self.parse_more_names_with_types_2()?;
        names_with_type.append(&mut more_names_with_types_2);

        Ok(
            names_with_type.into_iter().map(
                |(name, typename)| StorageDeclarationNode {
                    dtype: typename,
                    name: name,
                }
            ).collect()
        )
    }

    pub fn parse_names_with_type(&mut self) -> ParseResult<Vec<(String, String)>> {
        let names = self.parse_names()?;
        self.expect_token(&Token::TkColon);
        let typename = self.parse_type()?;

        Ok(names.into_iter().map(|name| (name, typename.clone())).collect())
    }

    pub fn parse_names(&mut self) -> ParseResult<Vec<String>> {
        let first_name = self.expect_identifier()?;
        let mut more_names = self.parse_more_names()?;
        more_names.insert(0, first_name);

        Ok(more_names)
    }

    pub fn parse_more_names(&mut self) -> ParseResult<Vec<String>> {
        match self.lex.peek() {
            Some(Token::TkColon) => Ok(Vec::new()),
            Some(Token::TkComma) => {
                self.expect_token(&Token::TkComma)?;
                let first_name = self.expect_identifier()?;
                let mut more_names = self.parse_more_names()?;
                more_names.insert(0, first_name);
                Ok(more_names)
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_type(&mut self) -> ParseResult<String> {
        let typename = self.expect_identifier()?;
        return Ok(typename);

        // TODO: ARRAYS!
    }

    pub fn parse_more_names_with_types_2(&mut self) -> ParseResult<Vec<(String, String)>> {
        match self.lex.peek() {
            Some(Token::Ident(name)) => {
                let mut names_with_type = self.parse_names_with_type()?;
                self.expect_token(&Token::TkSemicolon)?;
                let mut more_names_with_types_2 = self.parse_more_names_with_types_2()?;
                names_with_type.append(&mut more_names_with_types_2);
                Ok(names_with_type)
            },
            Some(Token::KwConst | Token::KwVar | Token::KwFunction | Token::KwProcedure | Token::KwBegin) => Ok(Vec::new()),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_block(&mut self) -> ParseResult<StatementBlockNode> {
        self.expect_token(&Token::KwBegin)?;
        let statements = self.parse_statements()?;
        self.expect_token(&Token::KwEnd)?;

        Ok(StatementBlockNode { statements: statements })
    }

    pub fn parse_statements(&mut self) -> ParseResult<Vec<StatementNode>> {
        match self.lex.peek() {
            Some(Token::KwEnd) => Ok(Vec::new()),
            Some(Token::KwBegin | Token::KwIf | Token::KwWhile | Token::KwFor | Token::Ident(_)) => {
                let first_statement = self.parse_statement()?;
                self.expect_token(&Token::TkSemicolon)?;
                let mut more_statements = self.parse_statements()?;
                more_statements.insert(0, first_statement);
                Ok(more_statements)
            }
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_statement(&mut self) -> ParseResult<StatementNode> {
        todo!()
    }
}

enum Declaration {
    Variables(Vec<StorageDeclarationNode>),
    Constants(Vec<StorageDeclarationNode>),
    Function(CallableDeclarationNode),
    Procedure(CallableDeclarationNode)
}
