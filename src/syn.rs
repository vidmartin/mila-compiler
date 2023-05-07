
use crate::tokens::*;
use crate::ast::*;
use std::iter::Peekable;

pub enum SyntaxError {
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
                    Err(SyntaxError::Unexpected(tok))
                }
            },
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }   

    fn expect_identifier(&mut self) -> ParseResult<String> {
        match self.lex.next() {
            Some(Token::Ident(s)) => Ok(s),
            Some(tok) => Err(SyntaxError::Unexpected(tok)),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    fn expect_int_lit(&mut self) -> ParseResult<i64> {
        match self.lex.next() {
            Some(Token::LitInt(i)) => Ok(i),
            Some(tok) => Err(SyntaxError::Unexpected(tok)),
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
        todo!()
    }

    pub fn parse_block(&mut self) -> ParseResult<StatementBlockNode> {
        todo!()
    }
}
