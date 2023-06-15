
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
    ImplementationError,
}

impl SyntaxError {
    pub fn panic_or_dont(self) -> Self {
        // panic!("syntax error: {self:?}");
        self
    }
}

pub type ParseResult<TNode> = Result<TNode, SyntaxError>;

pub struct Parser<'a, TLex : Iterator<Item = Token>> {
    lex: &'a mut Peekable<TLex>,
    unlex: Vec<Token>,
}

impl<'a, TLex : Iterator<Item = Token>> Parser<'a, TLex> {
    pub fn new(lex: &'a mut Peekable<TLex>) -> Self {
        Self { lex: lex, unlex: Vec::new(), }
    }

    fn debug_print(&mut self, _s: &str) {
        // if let Some(tok) = self.peek() {
        //     println!("{}, {}", s, tok);
        // } else {
        //     println!("{}, end of file", s);
        // }
    }

    fn peek(&mut self) -> Option<&Token> {
        if let Some(tok) = self.unlex.last() {
            return Some(tok)
        }
        self.lex.peek()
    }

    fn next(&mut self) -> Option<Token> {
        if let Some(tok) = self.unlex.pop() {
            return Some(tok);
        }
        self.lex.next()
    }

    fn expect_token(&mut self, token: &Token) -> ParseResult<()> {
        match self.next() {
            Some(tok) => {
                if tok == *token {
                    Ok(())
                } else {
                    Err(SyntaxError::Expected {
                        expected: token.clone(),
                        gotten: tok,
                    }.panic_or_dont())
                }
            },
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    fn expect_token_maybe(&mut self, token: &Token) -> ParseResult<bool> {
        if let Some(tok) = self.peek() {
            if *tok == *token {
                self.expect_token(token)?;
                return Ok(true);
            }
        }
        
        return Ok(false);
    }

    fn expect_identifier(&mut self) -> ParseResult<String> {
        match self.next() {
            Some(Token::Ident(s)) => Ok(s),
            Some(tok) => Err(SyntaxError::ExpectedIdentifier {
                gotten: tok,
            }.panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    fn expect_int_lit(&mut self) -> ParseResult<i64> {
        match self.next() {
            Some(Token::LitInt(i)) => Ok(i),
            Some(tok) => Err(SyntaxError::ExpectedIntLiteral {
                gotten: tok,
            }.panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    fn expect_str_lit(&mut self) -> ParseResult<String> {
        match self.next() {
            Some(Token::LitStr(s)) => Ok(s),
            Some(tok) => Err(SyntaxError::ExpectedStrLiteral {
                gotten: tok,
            }.panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
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
            params: Vec::new(),
            return_type: Some(
                DataType::OneInternal(unsafe { llvm_sys::core::LLVMInt32Type() })
            ),
            implementation: Some(CallableImplementationNode{
                variables: Vec::new(),
                implementation: main_block,
            }),
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

        match self.peek() {
            Some(Token::KwConst | Token::KwVar | Token::KwFunction | Token::KwProcedure) => {
                let first_declaration = self.parse_declaration()?;
                self.expect_token(&Token::TkSemicolon)?;
                let mut rest_of_declarations = self.parse_declarations()?;
                match first_declaration {
                    Declaration::Variables(mut storage_nodes) => {
                        storage_nodes.append(&mut rest_of_declarations.variables);
                        rest_of_declarations.variables = storage_nodes;
                    },
                    Declaration::Constants(mut storage_nodes) => {
                        storage_nodes.append(&mut rest_of_declarations.constants);
                        rest_of_declarations.constants = storage_nodes;
                    },
                    Declaration::Function(callable_node) | Declaration::Procedure(callable_node) => {
                        rest_of_declarations.callables.insert(0, callable_node);
                    }
                }
                Ok(rest_of_declarations)
            },
            Some(Token::KwBegin) => {
                Ok(ProgramDeclarations {
                    variables: Vec::new(),
                    constants: Vec::new(),
                    callables: Vec::new()
                })
            }
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_declaration(&mut self) -> ParseResult<Declaration> {
        self.debug_print("Declaration");

        match self.peek() {
            Some(Token::KwFunction) => Ok(Declaration::Function(self.parse_function()?)),
            Some(Token::KwProcedure) => Ok(Declaration::Procedure(self.parse_procedure()?)),
            Some(Token::KwConst) => Ok(Declaration::Constants(self.parse_constants()?)),
            Some(Token::KwVar) => Ok(Declaration::Variables(self.parse_variables()?)),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
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
            params: params,
            return_type: Some(DataType::One(rettype)),
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
            params: params,
            return_type: None,
        })
    }

    pub fn parse_function_or_procedure_rest(&mut self, mut wip: CallableDeclarationNode) -> ParseResult<CallableDeclarationNode> {
        self.debug_print("FunctionOrProcedureRest");
        
        match self.peek() {
            Some(Token::KwBegin | Token::KwVar) => {
                let vars = self.parse_maybe_variables_with_semicolon()?;
                let implementation = self.parse_block()?;
                
                wip.implementation = Some(CallableImplementationNode {
                    implementation: implementation,
                    variables: vars,
                });

                Ok(wip)
            },
            Some(Token::KwForward) => {
                self.expect_token(&Token::KwForward)?;

                Ok(wip) // leave implementation at None
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_maybe_variables_with_semicolon(&mut self) -> ParseResult<Vec<StorageDeclarationNode>> {
        self.debug_print("MaybeVariables");

        match self.peek() {
            Some(Token::KwVar) => { 
                let vars = self.parse_variables()?;
                self.expect_token(&Token::TkSemicolon)?;
                Ok(vars)
            },
            Some(Token::KwBegin) => Ok(Vec::new()),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_variables(&mut self) -> ParseResult<Vec<StorageDeclarationNode>> {
        self.debug_print("Variables");

        self.expect_token(&Token::KwVar)?;

        let names_with_type = self.parse_names_with_types()?;

        // let mut names_with_type = self.parse_names_with_type()?;
        // self.expect_token(&Token::TkSemicolon)?;
        // let mut more_names_with_types_2 = self.parse_more_names_with_types_2()?;
        // names_with_type.append(&mut more_names_with_types_2);

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

        match self.peek() {
            Some(Token::Ident(_)) => {
                let mut first = self.parse_names_with_type()?;
                let mut rest = self.parse_more_names_with_types()?;
                first.append(&mut rest);
                Ok(first)
            },
            Some(Token::TkParClose | Token::TkSemicolon) => Ok(Vec::new()),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_more_names_with_types(&mut self) -> ParseResult<Vec<(String, DataType)>> {
        self.debug_print("MoreNamesWithTypes");

        match self.peek() {
            Some(Token::TkSemicolon) => {
                self.expect_token(&Token::TkSemicolon)?;

                // this match expression & the 'unlex' field is the hack for
                // resolving the first-follow conflict on this nonterminal
                match self.peek() {
                    Some(Token::Ident(_)) => {
                        // identifier, so more variable definitions,
                        // so we continue expanding this rule

                        let mut first = self.parse_names_with_type()?;
                        let mut rest = self.parse_more_names_with_types()?;
                        first.append(&mut rest);
                        Ok(first)
                    },
                    _ => {
                        // not an identifier, we're actually reading
                        // a whole new declaration

                        self.unlex.push(Token::TkSemicolon);
                        Ok(Vec::new())
                    }
                }
            },
            Some(Token::TkParClose) => Ok(Vec::new()),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
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

        match self.peek() {
            Some(Token::TkColon) => Ok(Vec::new()),
            Some(Token::TkComma) => {
                self.expect_token(&Token::TkComma)?;
                let first_name = self.expect_identifier()?;
                let mut more_names = self.parse_more_names()?;
                more_names.insert(0, first_name);
                Ok(more_names)
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_type(&mut self) -> ParseResult<DataType> {
        self.debug_print("Type");

        match self.peek() {
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
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_block(&mut self) -> ParseResult<StatementBlockNode> {
        self.debug_print("Block");

        self.expect_token(&Token::KwBegin)?;
        let statements = self.parse_statements()?;
        self.expect_token(&Token::KwEnd)?;

        Ok(StatementBlockNode { statements: statements })
    }

    pub fn parse_statements(&mut self) -> ParseResult<Vec<StatementNode>> {
        match self.peek() {
            Some(Token::KwEnd) => Ok(Vec::new()),
            Some(
                Token::TkParOpen |
                Token::TkAdd |
                Token::TkSub |
                Token::Ident(_) |
                Token::KwBegin |
                Token::KwExit |
                Token::KwBreak |
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
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_more_statements(&mut self) -> ParseResult<Vec<StatementNode>> {
        match self.peek() {
            Some(Token::KwEnd) => Ok(Vec::new()),
            Some(Token::TkSemicolon) => {
                self.expect_token(&Token::TkSemicolon)?;
                Ok(self.parse_statements()?)
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_statement(&mut self) -> ParseResult<StatementNode> {
        self.debug_print("Statement");

        match self.peek() {
            Some(Token::KwIf) => Ok(StatementNode::IfStatement(self.parse_if()?)),
            Some(Token::KwWhile) => Ok(StatementNode::WhileLoop(self.parse_while()?)),
            Some(Token::KwFor) => Ok(StatementNode::ForLoop(self.parse_for()?)),
            Some(Token::KwBegin) => Ok(StatementNode::StatementBlock(self.parse_block()?)),
            Some(
                Token::TkParOpen |
                Token::TkAdd |
                Token::TkSub |
                Token::Ident(_) |
                Token::LitInt(_) |
                Token::LitStr(_) |
                Token::KwNot
            ) => Ok(self.parse_expression_or_assignment()?),
            Some(Token::KwExit) => {
                self.expect_token(&Token::KwExit)?;
                Ok(StatementNode::Exit)
            },
            Some(Token::KwBreak) => {
                self.expect_token(&Token::KwBreak)?;
                Ok(StatementNode::Break)
            }
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

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

        match self.peek() {
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
                Token::TkNe |
                Token::TkGt | 
                Token::TkLt | 
                Token::TkGe | 
                Token::TkLe | 
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
                Token::KwEnd |
                Token::KwElse
            ) => Ok(None),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_params(&mut self) -> ParseResult<Vec<ExpressionNode>> {
        self.debug_print("Params");

        match self.peek() {
            Some(Token::TkParClose) => Ok(Vec::new()),
            Some(
                Token::KwNot |
                Token::TkSub |
                Token::TkAdd |
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
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_more_params(&mut self) -> ParseResult<Vec<ExpressionNode>> {
        self.debug_print("MoreParams");

        match self.peek() {
            Some(Token::TkParClose) => Ok(Vec::new()),
            Some(Token::TkComma) => {
                self.expect_token(&Token::TkComma)?;
                let first_param = self.parse_e0()?;
                let mut more_params = self.parse_more_params()?;
                more_params.insert(0, first_param);
                Ok(more_params)
            },
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_constants(&mut self) -> ParseResult<Vec<StorageDeclarationNode>> {
        self.debug_print("Constants");

        self.expect_token(&Token::KwConst)?;
        let first_constant = self.parse_constant()?;
        let mut more_constants = self.parse_more_constants()?;
        more_constants.insert(0, first_constant);
        Ok(more_constants)
    }

    pub fn parse_more_constants(&mut self) -> ParseResult<Vec<StorageDeclarationNode>> {
        self.debug_print("MoreConstants");

        self.expect_token(&Token::TkSemicolon)?;

        match self.peek() {
            Some(Token::Ident(_)) => {
                let first_constant = self.parse_constant()?;
                let mut constants = self.parse_more_constants()?;
                constants.insert(0, first_constant);
                Ok(constants)
            },
            _ => {
                // this is yet again a hack for resolving first-follow conflict by escaping the confines of LL(1) parsing
                self.unlex.push(Token::TkSemicolon);
                Ok(Vec::new())
            }
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

        match self.parse_expression_or_assignment_rest()? {
            Some(rhs) => {
                Ok(StatementNode::Assignment(AssignmentNode {
                    target: lhs,
                    value: rhs,
                }))
            },
            None => Ok(StatementNode::Expression(lhs)),
        }
    }

    pub fn parse_expression_or_assignment_rest(&mut self) -> ParseResult<Option<ExpressionNode>> {
        self.debug_print("ExpressionOrAssignmentRest");

        match self.peek() {
            Some(Token::TkAssign) => {
                self.expect_token(&Token::TkAssign)?;
                Ok(Some(self.parse_e0()?))
            },
            Some(Token::TkSemicolon | Token::KwElse | Token::KwEnd) => Ok(None),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_e0(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E0");

        let lhs = self.parse_e1()?;
        Ok(self.parse_more_e0(lhs)?)
    }

    pub fn parse_more_e0(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("MoreE0");

        match self.peek() {
            Some(op @ Token::KwOr) => {
                let op = op.clone();
                self.expect_token(&op)?;

                let opkind: BinaryOperatorKind = op.try_into().or_else(|_| Err(SyntaxError::ImplementationError.panic_or_dont()))?;
                let rhs = self.parse_e0()?;

                if let ExpressionNode::BinaryOperator(BinaryOperatorNode {
                    kind: subop @ BinaryOperatorKind::Or,
                    lhs: sublhs,
                    rhs: subrhs,
                }) = rhs {
                    // convert to left associativity
                    Ok(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                        kind: subop,
                        lhs: Box::new(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                            kind: opkind,
                            lhs: Box::new(lhs),
                            rhs: sublhs,
                        })),
                        rhs: subrhs,
                    }))
                } else {
                    Ok(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                        kind: opkind,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
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
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_e1(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E1");

        let lhs = self.parse_e2()?;
        Ok(self.parse_more_e1(lhs)?)
    }

    pub fn parse_more_e1(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("MoreE1");

        match self.peek() {
            Some(op @ Token::KwAnd) => {
                let op = op.clone();
                self.expect_token(&op)?;

                let opkind: BinaryOperatorKind = op.try_into().or_else(|_| Err(SyntaxError::ImplementationError.panic_or_dont()))?;
                let rhs = self.parse_e1()?;

                if let ExpressionNode::BinaryOperator(BinaryOperatorNode {
                    kind: subop @ BinaryOperatorKind::And,
                    lhs: sublhs,
                    rhs: subrhs,
                }) = rhs {
                    // convert to left associativity
                    Ok(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                        kind: subop,
                        lhs: Box::new(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                            kind: opkind,
                            lhs: Box::new(lhs),
                            rhs: sublhs,
                        })),
                        rhs: subrhs,
                    }))
                } else {
                    Ok(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                        kind: opkind,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
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
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_e2(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E2");

        match self.peek() {
            Some(Token::KwNot) => {
                self.expect_token(&Token::KwNot)?;

                // this is a hack for implementing the 'not' operator: disguised = operator
                Ok(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                    kind: BinaryOperatorKind::Eq,
                    lhs: Box::new(ExpressionNode::Literal(LiteralNode::Integer(0))),
                    rhs: Box::new(self.parse_e2()?),
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
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_e3(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E3");

        let lhs = self.parse_e4()?;
        Ok(self.parse_e3r(lhs)?)
    }

    pub fn parse_e3r(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("E3R");

        match self.peek() {
            Some(op @ (Token::TkLt | Token::TkLe | Token::TkGt | Token::TkGe | Token::TkEq | Token::TkNe)) => {
                let op = op.clone();
                self.expect_token(&op)?;

                let rhs = self.parse_e4()?;
                let opkind: BinaryOperatorKind = op.try_into().or_else(|_| Err(SyntaxError::ImplementationError.panic_or_dont()))?;

                Ok(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                    kind: opkind,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
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
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_e4(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E4");

        let lhs = self.parse_e5()?;
        Ok(self.parse_more_e4(lhs)?)
    }

    pub fn parse_more_e4(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("MoreE4");

        match self.peek() {
            Some(op @ (Token::TkAdd | Token::TkSub)) => {
                let op = op.clone();
                self.expect_token(&op)?;

                let opkind: BinaryOperatorKind = op.try_into().or_else(|_| Err(SyntaxError::ImplementationError.panic_or_dont()))?;
                let rhs = self.parse_e4()?;

                if let ExpressionNode::BinaryOperator(BinaryOperatorNode {
                    kind: subop @ (BinaryOperatorKind::Add | BinaryOperatorKind::Sub),
                    lhs: sublhs,
                    rhs: subrhs,
                }) = rhs {
                    // convert to left associativity
                    Ok(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                        kind: subop,
                        lhs: Box::new(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                            kind: opkind,
                            lhs: Box::new(lhs),
                            rhs: sublhs,
                        })),
                        rhs: subrhs,
                    }))
                } else {
                    Ok(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                        kind: opkind,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }))
                }
            },
            Some(
                Token::TkParClose |
                Token::TkSqClose |
                Token::TkComma |
                Token::TkSemicolon |
                Token::TkAssign |
                Token::TkLt |
                Token::TkLe |
                Token::TkGt |
                Token::TkGe |
                Token::TkEq |
                Token::TkNe |
                Token::KwAnd |
                Token::KwDo |
                Token::KwElse |
                Token::KwThen |
                Token::KwOr |
                Token::KwTo |
                Token::KwDownto |
                Token::KwEnd
            ) => Ok(lhs),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_e5(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E5");

        let lhs = self.parse_e6()?;
        Ok(self.parse_more_e5(lhs)?)
    }

    pub fn parse_more_e5(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("MoreE5");

        match self.peek() {
            Some(op @ (Token::TkMul | Token::KwDiv | Token::KwMod)) => {
                let op = op.clone();
                self.expect_token(&op)?;

                let opkind: BinaryOperatorKind = op.try_into().or_else(|_| Err(SyntaxError::ImplementationError.panic_or_dont()))?;
                let rhs = self.parse_e5()?;

                if let ExpressionNode::BinaryOperator(BinaryOperatorNode {
                    kind: subop @ (BinaryOperatorKind::Mul | BinaryOperatorKind::Div | BinaryOperatorKind::Mod),
                    lhs: sublhs,
                    rhs: subrhs,
                }) = rhs {
                    // convert to left associativity
                    Ok(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                        kind: subop,
                        lhs: Box::new(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                            kind: opkind,
                            lhs: Box::new(lhs),
                            rhs: sublhs,
                        })),
                        rhs: subrhs,
                    }))
                } else {
                    Ok(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                        kind: opkind,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }))
                }
            },
            Some(
                Token::TkParClose |
                Token::TkSqClose |
                Token::TkComma |
                Token::TkSemicolon |
                Token::TkAssign |
                Token::TkLt |
                Token::TkLe |
                Token::TkGt |
                Token::TkGe |
                Token::TkEq |
                Token::TkNe |
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
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_e6(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E6");

        match self.peek() {
            Some(op @ (Token::TkAdd | Token::TkSub)) => {
                let op = op.clone();
                self.expect_token(&op)?;

                let opkind: BinaryOperatorKind = op.try_into().or_else(|_| Err(SyntaxError::ImplementationError.panic_or_dont()))?;

                // hack for implementing unary + / - :
                Ok(ExpressionNode::BinaryOperator(BinaryOperatorNode {
                    kind: opkind,
                    lhs: Box::new(ExpressionNode::Literal(LiteralNode::Integer(0))),
                    rhs: Box::new(self.parse_e6()?),
                }))
            },
            Some(
                Token::TkParOpen |
                Token::Ident(_) |
                Token::LitInt(_) |
                Token::LitStr(_)
            ) => Ok(self.parse_e7()?),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
        }
    }

    pub fn parse_e7(&mut self) -> ParseResult<ExpressionNode> {
        self.debug_print("E7");

        let expr = match self.peek() {
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
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont())?,
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont())?,
        };

        Ok(self.parse_more_e7(expr)?)
    }

    pub fn parse_more_e7(&mut self, lhs: ExpressionNode) -> ParseResult<ExpressionNode> {
        self.debug_print("MoreE7");

        match self.peek() {
            Some(Token::TkSqOpen) => {
                self.expect_token(&Token::TkSqOpen)?;
                let expr = self.parse_e0()?;
                self.expect_token(&Token::TkSqClose)?;
                Ok(self.parse_more_e7(ExpressionNode::ArrayAccess(ArrayAccessNode {
                    array: Box::new(lhs),
                    index: Box::new(expr),
                }))?)
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
                Token::TkLt |
                Token::TkLe |
                Token::TkNe |
                Token::TkEq |
                Token::TkGt |
                Token::TkGe |
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
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
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

    pub fn parse_range(&mut self) -> ParseResult<RangeNode> {
        self.debug_print("Range");

        let lhs = self.parse_e0()?;
        return Ok(self.parse_range_rest(lhs)?);
    }

    pub fn parse_range_rest(&mut self, lhs: ExpressionNode) -> ParseResult<RangeNode> {
        self.debug_print("RangeRest");

        let op = match self.peek() {
            Some(tok @ (Token::KwTo | Token::KwDownto)) => tok.clone(),
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont())?,
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont())?,
        };

        self.expect_token(&op)?;
        let rhs = self.parse_e0()?;

        Ok(RangeNode {
            lhs: lhs,
            rhs: rhs,
            step: if op == Token::KwTo { 1 } else if op == Token::KwDownto { -1 } else { Err(SyntaxError::ImplementationError.panic_or_dont())? }
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

        match self.peek() {
            Some(Token::TkSemicolon | Token::KwEnd) => Ok(None),
            Some(Token::KwElse) => Ok(Some(self.parse_else()?)), // hack for first-follow conflict
            Some(tok) => Err(SyntaxError::Unexpected(tok.clone()).panic_or_dont()),
            None => Err(SyntaxError::UnexpectedEnd.panic_or_dont()),
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
