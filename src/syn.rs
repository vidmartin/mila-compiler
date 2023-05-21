
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

pub struct Parser<'a, TLex : Iterator<Item = Token>> {
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
            Some(Token::KwConst) => Ok(Declaration::Constants(self.parse_constants()?)),
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
                    init: None,
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
        match self.lex.peek() {
            Some(Token::KwIf) => todo!(),
            Some(Token::KwWhile) => todo!(),
            Some(Token::KwFor) => todo!(),
            Some(Token::KwBegin) => todo!(),
            Some(Token::Ident(s)) => {
                Ok(self.parse_assign_or_call()?)
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_assign_or_call(&mut self) -> ParseResult<StatementNode> {
        let target = self.expect_identifier()?;
        match self.parse_assign_or_call_rest()? {
            AssignOrCall::Assign(lhs) => Ok(
                StatementNode::Assignment(
                    AssignmentNode { varname: target, value: lhs }
                )
            ),
            AssignOrCall::Call(params) => Ok(
                StatementNode::Expression(
                    ExpressionNode::Call(
                        CallNode { callable_name: target, params: params }
                    )
                )
            ),
        }
    }

    pub fn parse_assign_or_call_rest(&mut self) -> ParseResult<AssignOrCall> {
        match self.lex.peek() {
            Some(Token::TkParOpen) => {
                self.expect_token(&Token::TkParOpen)?;
                let params = self.parse_params()?;
                self.expect_token(&Token::TkParClose)?;
                Ok(AssignOrCall::Call(params))
            },
            Some(Token::TkAssign) => {
                self.expect_token(&Token::TkAssign)?;
                Ok(AssignOrCall::Assign(self.parse_expression()?))
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_read_or_call(&mut self) -> ParseResult<ExpressionNode> {
        let target = self.expect_identifier()?;
        match self.parse_read_or_call_rest()? {
            Some(params) => Ok(
                ExpressionNode::Call(
                    CallNode { callable_name: target, params: params }
                )
            ),
            None => Ok(
                ExpressionNode::Access(target)
            ),
        }
    }

    pub fn parse_read_or_call_rest(&mut self) -> ParseResult<Option<Vec<ExpressionNode>>> {
        match self.lex.peek() {
            Some(Token::TkParOpen) => {
                self.expect_token(&Token::TkParOpen)?;
                let params = self.parse_params()?;
                self.expect_token(&Token::TkParClose)?;
                Ok(Some(params))
            },
            Some(
                Token::TkMul |
                Token::KwDiv | 
                Token::KwMod | 
                Token::TkAdd | 
                Token::TkSub | 
                Token::TkEq | 
                Token::TkNotEq |
                Token::TkMore | 
                Token::TkLess | 
                Token::TkMoreOrEq | 
                Token::TkLessOrEq | 
                Token::KwAnd | 
                Token::KwOr | 
                Token::TkSemicolon | 
                Token::TkComma | 
                Token::TkParClose | 
                Token::KwThen | 
                Token::KwDo | 
                Token::KwWhile
            ) => Ok(None),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_params(&mut self) -> ParseResult<Vec<ExpressionNode>> {
        match self.lex.peek() {
            Some(Token::TkParClose) => Ok(Vec::new()),
            Some(Token::KwNot | Token::TkSub | Token::TkParOpen | Token::LitInt(_) | Token::Ident(_)) => {
                let first_param = self.parse_expression()?;
                let mut more_params = self.parse_more_params()?;
                more_params.insert(0, first_param);
                Ok(more_params)
            }
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_more_params(&mut self) -> ParseResult<Vec<ExpressionNode>> {
        match self.lex.peek() {
            Some(Token::TkParClose) => Ok(Vec::new()),
            Some(Token::TkComma) => {
                self.expect_token(&Token::TkComma)?;
                let first_param = self.parse_expression()?;
                let mut more_params = self.parse_more_params()?;
                more_params.insert(0, first_param);
                Ok(more_params)
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_constants(&mut self) -> ParseResult<Vec<StorageDeclarationNode>> {
        self.expect_token(&Token::KwConst)?;
        let first_constant = self.parse_constant()?;
        self.expect_token(&Token::TkSemicolon)?;
        let mut more_constants = self.parse_more_constants()?;
        more_constants.insert(0, first_constant);
        Ok(more_constants)
    }

    pub fn parse_more_constants(&mut self) -> ParseResult<Vec<StorageDeclarationNode>> {
        match self.lex.peek() {
            Some(Token::Ident(s)) => {
                let first_constant = self.parse_constant()?;
                self.expect_token(&Token::TkSemicolon)?;
                let mut constants = self.parse_more_constants()?;
                constants.insert(0, first_constant);
                Ok(constants)
            },
            Some(Token::KwBegin | Token::KwConst | Token::KwFunction | Token::KwProcedure | Token::KwVar) => Ok(Vec::new()),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_constant(&mut self) -> ParseResult<StorageDeclarationNode> {
        let ident = self.expect_identifier()?;
        self.expect_token(&Token::TkEq)?;
        let val = self.expect_int_lit()?;
        Ok(StorageDeclarationNode {
            dtype: "integer".to_string(), // TODO: have "integer" as a constant somewhere
            name: ident,
            init: Some(LiteralNode::Integer(val)),
        })
    }

    pub fn parse_expression(&mut self) -> ParseResult<ExpressionNode> {
        // TODO: this is not done properly yet! we can't jump straight to e6!
        self.parse_e6()
    }

    pub fn parse_e0(&mut self) -> ParseResult<ExpressionNode> {
        let lhs = self.parse_e1()?;
        Ok(self.parse_more_e0(lhs)?)
    }

    pub fn parse_more_e0(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        match self.lex.peek() {
            Some(Token::KwOr) => {
                self.expect_token(&Token::KwOr)?;
                Ok(ExpressionNode::BinOp {
                    op: Token::KwOr,
                    lhs: Some(lhs),
                    rhs: Some(self.parse_e0()?),
                })
            },
            Some(Token::TkParClose | Token::TkComma | Token::TkSemicolon | Token::KwDo | Token::KwElse | Token::KwThen) => Some(lhs),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_e1(&mut self) -> ParseResult<()> {

    }

    pub fn parse_e6(&mut self) -> ParseResult<ExpressionNode> {
        match self.lex.peek() {
            Some(Token::TkParOpen) => {
                self.expect_token(&Token::TkParOpen)?;
                let expr = self.parse_expression()?;
                self.expect_token(&Token::TkParClose)?;
                Ok(expr)
            },
            Some(Token::LitInt(i)) => Ok(ExpressionNode::Literal(LiteralNode::Integer(*i))),
            Some(Token::Ident(_)) => Ok(self.parse_read_or_call()?),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }    
}

enum Declaration {
    Variables(Vec<StorageDeclarationNode>),
    Constants(Vec<StorageDeclarationNode>),
    Function(CallableDeclarationNode),
    Procedure(CallableDeclarationNode)
}

enum AssignOrCall {
    Assign(ExpressionNode),
    Call(Vec<ExpressionNode>),
}
