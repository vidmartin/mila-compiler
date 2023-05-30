
use crate::tokens::*;
use crate::ast::*;
use std::iter::Peekable;

#[derive(Debug)]
pub enum SyntaxError {
    Expected { expected: Token, gotten: Token },
    ExpectedIdentifier { gotten: Token },
    ExpectedIntLiteral { gotten: Token },
    ExpectedStrLiteral { gotten: Token },
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

    fn debug_print(&mut self, s: &str) {
        if let Some(tok) = self.lex.peek() {
            println!("{}, {}", s, tok);
        } else {
            println!("{}, end of file", s);
        }
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

    fn expect_token_maybe(&mut self, token: &Token) -> ParseResult<bool> {
        if let Some(tok) = self.lex.peek() {
            if *tok == *token {
                self.expect_token(token)?;
                return Ok(true);
            }
        }
        
        return Ok(false);
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

    fn expect_str_lit(&mut self) -> ParseResult<String> {
        match self.lex.next() {
            Some(Token::LitStr(s)) => Ok(s),
            Some(tok) => Err(SyntaxError::ExpectedStrLiteral {
                gotten: tok,
            }),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    fn expect_int_lit_signed(&mut self) -> ParseResult<i64> {
        if self.expect_token_maybe(&Token::TkSub)? {
            Ok(-(self.expect_int_lit()?))
        } else {
            Ok(self.expect_int_lit()?)
        }
    }

    pub fn parse_program(&mut self) -> ParseResult<ProgramNode> {
        self.debug_print("Program");

        self.expect_token(&Token::KwProgram)?;
        let program_name = self.expect_identifier()?;
        self.expect_token(&Token::TkSemicolon)?;

        let mut declarations = self.parse_declarations()?;
        let main_block = self.parse_block()?;
        let main_func = CallableDeclarationNode {
            name: "main".to_string(),
            param_types: Vec::new(),
            variables: Vec::new(),
            return_type: Some(DataType::One("integer".to_string())),
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
        self.debug_print("Declarations");

        match self.lex.peek() {
            Some(Token::KwConst | Token::KwVar | Token::KwFunction | Token::KwProcedure) => {
                let first_declaration = self.parse_declaration()?;
                self.expect_token(&Token::TkSemicolon)?;
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
        self.debug_print("Declaration");

        match self.lex.peek() {
            Some(Token::KwFunction) => Ok(Declaration::Function(self.parse_function()?)),
            Some(Token::KwProcedure) => Ok(Declaration::Procedure(self.parse_procedure()?)),
            Some(Token::KwConst) => Ok(Declaration::Constants(self.parse_constants()?)),
            Some(Token::KwVar) => Ok(Declaration::Variables(self.parse_variables()?)),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_function(&mut self) -> ParseResult<CallableDeclarationNode> {
        self.debug_print("Function");

        let wip = self.parse_function_header()?;
        return Ok(self.parse_function_or_procedure_rest(wip)?);
    }

    pub fn parse_function_header(&mut self) -> ParseResult<CallableDeclarationNode> {
        self.debug_print("FunctionHeader");
        
        self.expect_token(&Token::KwFunction)?;
        let name = self.expect_identifier()?;
        self.expect_token(&Token::TkParOpen)?;
        let params = self.parse_names_with_types()?;
        self.expect_token(&Token::TkParClose)?;
        self.expect_token(&Token::TkColon)?;
        let rettype = self.expect_identifier()?;
        self.expect_token(&Token::TkSemicolon)?;

        Ok(CallableDeclarationNode {
            implementation: None,
            name: name,
            param_types: params.into_iter().map(|(_name, dtype)| dtype).collect(),
            return_type: Some(DataType::One(rettype)),
            variables: Vec::new()
        })
    }

    pub fn parse_procedure(&mut self) -> ParseResult<CallableDeclarationNode> {
        self.debug_print("Procedure");
        let wip = self.parse_procedure_header()?;
        return Ok(self.parse_function_or_procedure_rest(wip)?);
    }

    pub fn parse_procedure_header(&mut self) -> ParseResult<CallableDeclarationNode> {
        self.debug_print("ProcedureHeader");
        
        self.expect_token(&Token::KwProcedure)?;
        let name = self.expect_identifier()?;
        self.expect_token(&Token::TkParOpen)?;
        let params = self.parse_names_with_types()?;
        self.expect_token(&Token::TkParClose)?;
        self.expect_token(&Token::TkSemicolon)?;

        Ok(CallableDeclarationNode {
            implementation: None,
            name: name,
            param_types: params.into_iter().map(|(_name, dtype)| dtype).collect(),
            return_type: None,
            variables: Vec::new()
        })
    }

    pub fn parse_function_or_procedure_rest(&mut self, mut wip: CallableDeclarationNode) -> ParseResult<CallableDeclarationNode> {
        self.debug_print("FunctionOrProcedureRest");
        
        match self.lex.peek() {
            Some(Token::KwBegin | Token::KwVar) => {
                let vars = self.parse_maybe_variables()?;
                let implementation = self.parse_block()?;
                
                wip.variables = vars;
                wip.implementation = Some(implementation);

                Ok(wip)
            },
            Some(Token::KwForward) => {
                self.expect_token(&Token::KwForward)?;
                self.expect_token(&Token::TkSemicolon)?;

                Ok(wip) // leave implementation at None
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_maybe_variables(&mut self) -> ParseResult<Vec<StorageDeclarationNode>> {
        self.debug_print("MaybeVariables");

        match self.lex.peek() {
            Some(Token::KwVar) => Ok(self.parse_variables()?),
            Some(Token::KwBegin) => Ok(Vec::new()),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_variables(&mut self) -> ParseResult<Vec<StorageDeclarationNode>> {
        self.debug_print("Variables");

        self.expect_token(&Token::KwVar)?;
        let mut names_with_type = self.parse_names_with_type()?;
        self.expect_token(&Token::TkSemicolon)?;
        let mut more_names_with_types_2 = self.parse_more_names_with_types_2()?;
        names_with_type.append(&mut more_names_with_types_2);

        Ok(
            names_with_type.into_iter().map(
                |(name, dtype)| StorageDeclarationNode {
                    dtype: dtype,
                    name: name,
                    init: None,
                }
            ).collect()
        )
    }

    pub fn parse_names_with_types(&mut self) -> ParseResult<Vec<(String, DataType)>> {
        self.debug_print("NamesWithTypes");

        match self.lex.peek() {
            Some(Token::Ident(_)) => {
                let mut first = self.parse_names_with_type()?;
                let mut rest = self.parse_more_names_with_types()?;
                first.append(&mut rest);
                Ok(first)
            },
            Some(Token::TkParClose) => Ok(Vec::new()),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_more_names_with_types(&mut self) -> ParseResult<Vec<(String, DataType)>> {
        self.debug_print("MoreNamesWithTypes");

        match self.lex.peek() {
            Some(Token::TkSemicolon) => {
                self.expect_token(&Token::TkSemicolon)?;
                let mut first = self.parse_names_with_type()?;
                let mut rest = self.parse_more_names_with_types()?;
                first.append(&mut rest);
                Ok(first)
            },
            Some(Token::TkParClose) => Ok(Vec::new()),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_names_with_type(&mut self) -> ParseResult<Vec<(String, DataType)>> {
        self.debug_print("NamesWithType");

        let names = self.parse_names()?;
        self.expect_token(&Token::TkColon)?;
        let dtype = self.parse_type()?;

        Ok(names.into_iter().map(|name| (name, dtype.clone())).collect())
    }

    pub fn parse_names(&mut self) -> ParseResult<Vec<String>> {
        self.debug_print("Names");

        let first_name = self.expect_identifier()?;
        let mut more_names = self.parse_more_names()?;
        more_names.insert(0, first_name);

        Ok(more_names)
    }

    pub fn parse_more_names(&mut self) -> ParseResult<Vec<String>> {
        self.debug_print("MoreNames");

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

    pub fn parse_type(&mut self) -> ParseResult<DataType> {
        self.debug_print("Type");

        match self.lex.peek() {
            Some(Token::Ident(_)) => Ok(DataType::One(self.expect_identifier()?)),
            Some(Token::KwArray) => {
                self.expect_token(&Token::KwArray)?;
                self.expect_token(&Token::TkSqOpen)?;
                let from = self.expect_int_lit_signed()?;
                self.expect_token(&Token::TkDotDot)?;
                let to = self.expect_int_lit_signed()?;
                self.expect_token(&Token::TkSqClose)?;
                self.expect_token(&Token::KwOf)?;
                let item = self.parse_type()?;

                Ok(DataType::Array {
                    from: from,
                    to: to,
                    item: Box::new(item),
                })
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_more_names_with_types_2(&mut self) -> ParseResult<Vec<(String, DataType)>> {
        self.debug_print("MoreNamesWithTypes2");

        match self.lex.peek() {
            Some(Token::Ident(_)) => {
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
        self.debug_print("Block");

        self.expect_token(&Token::KwBegin)?;
        let statements = self.parse_statements()?;
        self.expect_token(&Token::KwEnd)?;

        Ok(StatementBlockNode { statements: statements })
    }

    // pub fn parse_statements(&mut self) -> ParseResult<Vec<StatementNode>> {
    //     self.debug_print("Statements");

    //     match self.lex.peek() {
    //         Some(Token::KwEnd) => Ok(Vec::new()),
    //         Some(Token::KwBegin | Token::KwIf | Token::KwWhile | Token::KwFor | Token::Ident(_)) => {
    //             let first_statement = self.parse_statement()?;
    //             self.expect_token(&Token::TkSemicolon)?;
    //             let mut more_statements = self.parse_statements()?;
    //             more_statements.insert(0, first_statement);
    //             Ok(more_statements)
    //         }
    //         Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
    //         None => Err(SyntaxError::UnexpectedEnd),
    //     }
    // }

    pub fn parse_statements(&mut self) -> ParseResult<Vec<StatementNode>> {
        match self.lex.peek() {
            Some(Token::KwEnd) => Ok(Vec::new()),
            Some(
                Token::TkParOpen |
                Token::TkComma |
                Token::TkDot |
                Token::Ident(_) |
                Token::KwBegin |
                Token::KwFor |
                Token::KwIf |
                Token::KwWhile |
                Token::LitInt(_) |
                Token::LitStr(_) |
                Token::KwNot
            ) => {
                let first_stmt = self.parse_statement()?;
                let mut more_stmts = self.parse_more_statements()?;
                more_stmts.insert(0, first_stmt);
                Ok(more_stmts)
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_more_statements(&mut self) -> ParseResult<Vec<StatementNode>> {
        match self.lex.peek() {
            Some(Token::KwEnd) => Ok(Vec::new()),
            Some(Token::TkSemicolon) => {
                self.expect_token(&Token::TkSemicolon)?;
                Ok(self.parse_statements()?)
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_statement(&mut self) -> ParseResult<StatementNode> {
        self.debug_print("Statement");

        match self.lex.peek() {
            Some(Token::KwIf) => Ok(StatementNode::IfStatement(self.parse_if()?)),
            Some(Token::KwWhile) => Ok(StatementNode::WhileLoop(self.parse_while()?)),
            Some(Token::KwFor) => Ok(StatementNode::ForLoop(self.parse_for()?)),
            Some(Token::KwBegin) => Ok(StatementNode::StatementBlock(self.parse_block()?)),
            Some(Token::Ident(_)) => Ok(self.parse_expression_or_assignment()?),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    // pub fn parse_assign_or_call(&mut self) -> ParseResult<StatementNode> {
    //     self.debug_print("AssignOrCall");

    //     let target = self.expect_identifier()?;
    //     match self.parse_assign_or_call_rest()? {
    //         AssignOrCall::Assign(lhs) => Ok(
    //             StatementNode::Assignment(
    //                 AssignmentNode { varname: target, value: lhs }
    //             )
    //         ),
    //         AssignOrCall::Call(params) => Ok(
    //             StatementNode::Expression(
    //                 ExpressionNode::Call(
    //                     CallNode { callable_name: target, params: params }
    //                 )
    //             )
    //         ),
    //     }
    // }

    // pub fn parse_assign_or_call_rest(&mut self) -> ParseResult<AssignOrCall> {
    //     self.debug_print("AssignOrCallRest");

    //     match self.lex.peek() {
    //         Some(Token::TkParOpen) => {
    //             self.expect_token(&Token::TkParOpen)?;
    //             let params = self.parse_params()?;
    //             self.expect_token(&Token::TkParClose)?;
    //             Ok(AssignOrCall::Call(params))
    //         },
    //         Some(Token::TkAssign) => {
    //             self.expect_token(&Token::TkAssign)?;
    //             Ok(AssignOrCall::Assign(self.parse_expression()?))
    //         },
    //         Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
    //         None => Err(SyntaxError::UnexpectedEnd),
    //     }
    // }

    pub fn parse_read_or_call(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("ReadOrCall");

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
        self.debug_print("ReadOrCallRest");

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
                Token::TkSqOpen |
                Token::TkSqClose |
                Token::KwAnd | 
                Token::KwOr | 
                Token::TkSemicolon | 
                Token::TkAssign |
                Token::TkComma | 
                Token::TkParClose | 
                Token::KwThen | 
                Token::KwDo | 
                Token::KwWhile |
                Token::KwTo |
                Token::KwDownto |
                Token::KwEnd
            ) => Ok(None),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_params(&mut self) -> ParseResult<Vec<ExpressionNode>> {
        self.debug_print("Params");

        match self.lex.peek() {
            Some(Token::TkParClose) => Ok(Vec::new()),
            Some(
                Token::KwNot |
                Token::TkSub |
                Token::TkParOpen |
                Token::LitInt(_) |
                Token::LitStr(_) |
                Token::Ident(_)
            ) => {
                let first_param = self.parse_e0()?;
                let mut more_params = self.parse_more_params()?;
                more_params.insert(0, first_param);
                Ok(more_params)
            }
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_more_params(&mut self) -> ParseResult<Vec<ExpressionNode>> {
        self.debug_print("MoreParams");

        match self.lex.peek() {
            Some(Token::TkParClose) => Ok(Vec::new()),
            Some(Token::TkComma) => {
                self.expect_token(&Token::TkComma)?;
                let first_param = self.parse_e0()?;
                let mut more_params = self.parse_more_params()?;
                more_params.insert(0, first_param);
                Ok(more_params)
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_constants(&mut self) -> ParseResult<Vec<StorageDeclarationNode>> {
        self.debug_print("Constants");

        self.expect_token(&Token::KwConst)?;
        let first_constant = self.parse_constant()?;
        self.expect_token(&Token::TkSemicolon)?;
        let mut more_constants = self.parse_more_constants()?;
        more_constants.insert(0, first_constant);
        Ok(more_constants)
    }

    pub fn parse_more_constants(&mut self) -> ParseResult<Vec<StorageDeclarationNode>> {
        self.debug_print("MoreConstants");

        match self.lex.peek() {
            Some(Token::Ident(_)) => {
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
        self.debug_print("Constant");

        let ident = self.expect_identifier()?;
        self.expect_token(&Token::TkEq)?;
        let val = self.expect_int_lit()?;
        Ok(StorageDeclarationNode {
            dtype: DataType::One("integer".to_string()), // TODO: have "integer" as a constant somewhere
            name: ident,
            init: Some(LiteralNode::Integer(val)),
        })
    }

    pub fn parse_expression_or_assignment(&mut self) -> ParseResult<StatementNode> {
        self.debug_print("ExpressionOrAssignment");

        let lhs = self.parse_e0()?;

        match self.parse_expression_or_assigment_rest()? {
            Some(rhs) => {
                Ok(StatementNode::Assignment(AssignmentNode {
                    target: lhs,
                    value: rhs,
                }))
            },
            None => Ok(StatementNode::Expression(lhs)),
        }
    }

    pub fn parse_expression_or_assigment_rest(&mut self) -> ParseResult<Option<ExpressionNode>> {
        self.debug_print("ExpressionOrAssignmentRest");

        match self.lex.peek() {
            Some(Token::TkAssign) => {
                self.expect_token(&Token::TkAssign)?;
                Ok(Some(self.parse_e0()?))
            },
            Some(Token::TkSemicolon | Token::KwElse | Token::KwEnd) => Ok(None),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_e0(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E0");

        let lhs = self.parse_e1()?;
        Ok(self.parse_more_e0(lhs)?)
    }

    pub fn parse_more_e0(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("MoreE0");

        match self.lex.peek() {
            Some(op @ Token::KwOr) => {
                let op = op.clone();
                self.expect_token(&op)?;
                let rhs = self.parse_e0()?;

                if let ExpressionNode::BinOp(BinaryOperator {
                    op: subop @ Token::KwOr,
                    lhs: sublhs,
                    rhs: subrhs,
                }) = rhs {
                    // convert to left associativity
                    Ok(ExpressionNode::BinOp(BinaryOperator {
                        op: subop,
                        lhs: Some(
                            Box::new(ExpressionNode::BinOp(BinaryOperator {
                                op: op,
                                lhs: Some(Box::new(lhs)),
                                rhs: sublhs,
                            }))
                        ),
                        rhs: subrhs,
                    }))
                } else {
                    Ok(ExpressionNode::BinOp(BinaryOperator {
                        op: op,
                        lhs: Some(Box::new(lhs)),
                        rhs: Some(Box::new(rhs)),
                    }))
                }
            },
            Some(
                Token::TkParClose |
                Token::TkSqClose |
                Token::TkComma |
                Token::TkSemicolon |
                Token::TkAssign |
                Token::KwDo |
                Token::KwElse |
                Token::KwThen |
                Token::KwTo |
                Token::KwDownto |
                Token::KwEnd
            ) => Ok(lhs),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_e1(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E1");

        let lhs = self.parse_e2()?;
        Ok(self.parse_more_e1(lhs)?)
    }

    pub fn parse_more_e1(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("MoreE1");

        match self.lex.peek() {
            Some(op @ Token::KwAnd) => {
                let op = op.clone();
                self.expect_token(&op)?;
                let rhs = self.parse_e1()?;

                if let ExpressionNode::BinOp(BinaryOperator {
                    op: subop @ Token::KwAnd,
                    lhs: sublhs,
                    rhs: subrhs,
                }) = rhs {
                    // convert to left associativity
                    Ok(ExpressionNode::BinOp(BinaryOperator {
                        op: subop,
                        lhs: Some(
                            Box::new(ExpressionNode::BinOp(BinaryOperator {
                                op: op,
                                lhs: Some(Box::new(lhs)),
                                rhs: sublhs,
                            }))
                        ),
                        rhs: subrhs,
                    }))
                } else {
                    Ok(ExpressionNode::BinOp(BinaryOperator {
                        op: op,
                        lhs: Some(Box::new(lhs)),
                        rhs: Some(Box::new(rhs)),
                    }))
                }
            },
            Some(
                Token::TkParClose |
                Token::TkSqClose |
                Token::TkComma |
                Token::TkSemicolon |
                Token::TkAssign |
                Token::KwDo |
                Token::KwElse |
                Token::KwThen |
                Token::KwOr |
                Token::KwTo |
                Token::KwDownto |
                Token::KwEnd
            ) => Ok(lhs),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_e2(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E2");

        match self.lex.peek() {
            Some(Token::KwNot) => {
                self.expect_token(&Token::KwNot)?;
                Ok(ExpressionNode::BinOp(BinaryOperator {
                    op: Token::KwNot,
                    lhs: None,
                    rhs: Some(Box::new(self.parse_e2()?)),
                }))
            },
            Some(
                Token::TkParOpen |
                Token::TkAdd |
                Token::TkSub |
                Token::Ident(_) |
                Token::LitInt(_) |
                Token::LitStr(_)
            ) => Ok(self.parse_e3()?),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_e3(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E3");

        let lhs = self.parse_e4()?;
        Ok(self.parse_e3r(lhs)?)
    }

    pub fn parse_e3r(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("E3R");

        match self.lex.peek() {
            Some(op @ (Token::TkLess | Token::TkLessOrEq | Token::TkMore | Token::TkMoreOrEq | Token::TkEq | Token::TkNotEq)) => {
                let op = op.clone();

                self.expect_token(&op)?;
                let rhs = self.parse_e4()?;

                Ok(ExpressionNode::BinOp(BinaryOperator {
                    op: op,
                    lhs: Some(Box::new(lhs)),
                    rhs: Some(Box::new(rhs)),
                }))
            },
            Some(
                Token::TkParClose |
                Token::TkSqClose |
                Token::TkComma |
                Token::TkSemicolon |
                Token::TkAssign |
                Token::KwAnd |
                Token::KwDo |
                Token::KwElse |
                Token::KwThen |
                Token::KwOr |
                Token::KwTo |
                Token::KwDownto |
                Token::KwEnd
            ) => Ok(lhs), 
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_e4(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E4");

        let lhs = self.parse_e5()?;
        Ok(self.parse_more_e4(lhs)?)
    }

    pub fn parse_more_e4(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("MoreE4");

        match self.lex.peek() {
            Some(op @ (Token::TkAdd | Token::TkSub)) => {
                let op = op.clone();
                self.expect_token(&op)?;
                let rhs = self.parse_e4()?;

                if let ExpressionNode::BinOp(BinaryOperator {
                    op: subop @ (Token::TkAdd | Token::TkSub),
                    lhs: sublhs,
                    rhs: subrhs,
                }) = rhs {
                    // convert to left associativity
                    Ok(ExpressionNode::BinOp(BinaryOperator {
                        op: subop,
                        lhs: Some(
                            Box::new(ExpressionNode::BinOp(BinaryOperator {
                                op: op,
                                lhs: Some(Box::new(lhs)),
                                rhs: sublhs,
                            }))
                        ),
                        rhs: subrhs,
                    }))
                } else {
                    Ok(ExpressionNode::BinOp(BinaryOperator {
                        op: op,
                        lhs: Some(Box::new(lhs)),
                        rhs: Some(Box::new(rhs)),
                    }))
                }
            },
            Some(
                Token::TkParClose |
                Token::TkSqClose |
                Token::TkComma |
                Token::TkSemicolon |
                Token::TkAssign |
                Token::TkLess |
                Token::TkLessOrEq |
                Token::TkMore |
                Token::TkMoreOrEq |
                Token::TkEq |
                Token::TkNotEq |
                Token::KwAnd |
                Token::KwDo |
                Token::KwElse |
                Token::KwThen |
                Token::KwOr |
                Token::KwTo |
                Token::KwDownto |
                Token::KwEnd
            ) => Ok(lhs),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_e5(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E5");

        let lhs = self.parse_e6()?;
        Ok(self.parse_more_e5(lhs)?)
    }

    pub fn parse_more_e5(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("MoreE5");

        match self.lex.peek() {
            Some(op @ (Token::TkMul | Token::KwDiv | Token::KwMod)) => {
                let op = op.clone();
                self.expect_token(&op)?;
                let rhs = self.parse_e5()?;

                if let ExpressionNode::BinOp(BinaryOperator {
                    op: subop @ (Token::TkMul | Token::KwDiv | Token::KwMod),
                    lhs: sublhs,
                    rhs: subrhs,
                }) = rhs {
                    // convert to left associativity
                    Ok(ExpressionNode::BinOp(BinaryOperator {
                        op: subop,
                        lhs: Some(
                            Box::new(ExpressionNode::BinOp(BinaryOperator {
                                op: op,
                                lhs: Some(Box::new(lhs)),
                                rhs: sublhs,
                            }))
                        ),
                        rhs: subrhs,
                    }))
                } else {
                    Ok(ExpressionNode::BinOp(BinaryOperator {
                        op: op,
                        lhs: Some(Box::new(lhs)),
                        rhs: Some(Box::new(rhs)),
                    }))
                }
            },
            Some(
                Token::TkParClose |
                Token::TkSqClose |
                Token::TkComma |
                Token::TkSemicolon |
                Token::TkAssign |
                Token::TkLess |
                Token::TkLessOrEq |
                Token::TkMore |
                Token::TkMoreOrEq |
                Token::TkEq |
                Token::TkNotEq |
                Token::KwAnd |
                Token::KwDo |
                Token::KwElse |
                Token::KwThen |
                Token::KwOr |
                Token::TkAdd |
                Token::TkSub |
                Token::KwTo |
                Token::KwDownto |
                Token::KwEnd
            ) => Ok(lhs),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_e6(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E6");

        match self.lex.peek() {
            Some(op @ (Token::TkAdd | Token::TkSub)) => {
                let op = op.clone();
                self.expect_token(&op)?;
                Ok(ExpressionNode::BinOp(BinaryOperator {
                    op: op,
                    lhs: None,
                    rhs: Some(Box::new(self.parse_e6()?)),
                }))
            },
            Some(
                Token::TkParOpen |
                Token::Ident(_) |
                Token::LitInt(_) |
                Token::LitStr(_)
            ) => Ok(self.parse_e7()?),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_e7(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E7");

        let expr = match self.lex.peek() {
            Some(Token::TkParOpen) => {
                self.expect_token(&Token::TkParOpen)?;
                let expr = self.parse_e0()?;
                self.expect_token(&Token::TkParClose)?;
                expr
            },
            Some(Token::Ident(_)) => self.parse_read_or_call()?,
            Some(Token::LitInt(_)) => {
                let int = self.expect_int_lit()?;
                ExpressionNode::Literal(LiteralNode::Integer(int))
            },
            Some(Token::LitStr(_)) => {
                let s = self.expect_str_lit()?;
                ExpressionNode::Literal(LiteralNode::String(s))
            }
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()))?,
            None => Err(SyntaxError::UnexpectedEnd)?,
        };

        Ok(self.parse_more_e7(expr)?)
    }

    pub fn parse_more_e7(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("MoreE7");

        match self.lex.peek() {
            Some(Token::TkSqOpen) => {
                self.expect_token(&Token::TkSqOpen)?;
                let expr = self.parse_e0()?;
                self.expect_token(&Token::TkSqClose)?;
                Ok(self.parse_more_e7(ExpressionNode::ArrayAccess {
                    array: Box::new(lhs),
                    index: Box::new(expr),
                })?)
            },
            Some(
                Token::TkParClose |
                Token::TkSqClose |
                Token::TkMul |
                Token::TkAdd |
                Token::TkComma |
                Token::TkSub |
                Token::TkAssign |
                Token::TkSemicolon |
                Token::TkLess |
                Token::TkLessOrEq |
                Token::TkNotEq |
                Token::TkEq |
                Token::TkMore |
                Token::TkMoreOrEq |
                Token::KwAnd |
                Token::KwDiv |
                Token::KwDo |
                Token::KwElse |
                Token::KwThen |
                Token::KwMod |
                Token::KwOr |
                Token::KwTo |
                Token::KwDownto |
                Token::KwEnd
            ) => Ok(lhs),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_for(&mut self) -> ParseResult<ForLoopNode> {
        self.debug_print("For");

        self.expect_token(&Token::KwFor)?;
        let itername = self.expect_identifier()?;
        self.expect_token(&Token::TkAssign)?;
        let range = self.parse_range()?;
        self.expect_token(&Token::KwDo)?;
        let inner = self.parse_statement()?;

        Ok(ForLoopNode {
            iterating: StorageDeclarationNode {
                dtype: DataType::One("integer".to_string()),
                init: None, // will be initialized based on range
                name: itername,
            },
            range: range,
            inner: Box::new(inner),
        })
    }

    pub fn parse_range(&mut self) -> ParseResult<BinaryOperator> {
        self.debug_print("Range");

        let lhs = self.parse_e0()?;
        return Ok(self.parse_range_rest(lhs)?);
    }

    pub fn parse_range_rest(&mut self, lhs: ExpressionNode) -> ParseResult<BinaryOperator> {
        self.debug_print("RangeRest");

        let op = match self.lex.peek() {
            Some(tok @ (Token::KwTo | Token::KwDownto)) => tok.clone(),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()))?,
            None => Err(SyntaxError::UnexpectedEnd)?,
        };

        self.expect_token(&op)?;
        let rhs = self.parse_e0()?;
        Ok(BinaryOperator {
            lhs: Some(Box::new(lhs)),
            rhs: Some(Box::new(rhs)),
            op: op,
        })
    }

    pub fn parse_if(&mut self) -> ParseResult<IfStatementNode> {
        self.debug_print("If");

        self.expect_token(&Token::KwIf)?;
        let cond = self.parse_e0()?;
        self.expect_token(&Token::KwThen)?;
        let stmt = self.parse_statement()?;
        let elbl = self.parse_maybe_else()?;

        Ok(IfStatementNode {
            condition: cond,
            yes: Box::new(stmt),
            no: elbl.map(Box::new),
        })
    }

    pub fn parse_maybe_else(&mut self) -> ParseResult<Option<StatementNode>> {
        self.debug_print("MaybeElse");

        match self.lex.peek() {
            Some(Token::TkSemicolon | Token::KwEnd) => Ok(None),
            Some(Token::KwElse) => Ok(Some(self.parse_else()?)), // hack for first-follow conflict
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone())),
            None => Err(SyntaxError::UnexpectedEnd),
        }
    }

    pub fn parse_else(&mut self) -> ParseResult<StatementNode> {
        self.debug_print("Else");

        self.expect_token(&Token::KwElse)?;
        Ok(self.parse_statement()?)
    }

    pub fn parse_while(&mut self) -> ParseResult<WhileLoopNode> {
        self.debug_print("While");

        self.expect_token(&Token::KwWhile)?;
        let cond = self.parse_e0()?;
        self.expect_token(&Token::KwDo)?;
        let stmt = self.parse_statement()?;

        Ok(WhileLoopNode {
            condition: cond,
            inner: Box::new(stmt),
        })
    }
}

pub enum Declaration {
    Variables(Vec<StorageDeclarationNode>),
    Constants(Vec<StorageDeclarationNode>),
    Function(CallableDeclarationNode),
    Procedure(CallableDeclarationNode)
}

// pub enum AssignOrCall {
//     Assign(ExpressionNode),
//     Call(Vec<ExpressionNode>),
// }
