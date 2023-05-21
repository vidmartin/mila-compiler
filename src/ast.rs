
use crate::tokens::Token;

pub enum ASTNode {
    Program(ProgramNode),
}

pub enum StatementNode {
    StatementBlock(StatementBlockNode),
    Assignment(AssignmentNode),
    Expression(ExpressionNode),
}

pub enum ExpressionNode {
    Call(CallNode),
    Literal(LiteralNode),
    Access(String),
    BinOp { op: Token, lhs: Option<Box<ExpressionNode>>, rhs: Option<Box<ExpressionNode>> },
}

pub enum LiteralNode {
    Integer(i64),
}

/// top level node for the entire program
pub struct ProgramNode {
    pub name: String,
    pub declarations: ProgramDeclarations,
}

pub struct ProgramDeclarations {
    pub variables: Vec<StorageDeclarationNode>,
    pub constants: Vec<StorageDeclarationNode>,
    pub callables: Vec<CallableDeclarationNode>,
}

/// declaration of a variable or a constant
pub struct StorageDeclarationNode {
    pub name: String,
    pub dtype: String,
    pub init: Option<LiteralNode>,
}

/// declaration of a function or a procedure
pub struct CallableDeclarationNode {
    pub name: String,
    pub param_types: Vec<String>,
    pub variables: Vec<StorageDeclarationNode>,
    pub return_type: Option<String>,
    pub implementation: Option<StatementBlockNode>,
}

pub struct StatementBlockNode {
    pub statements: Vec<StatementNode>,
}

pub struct AssignmentNode {
    pub varname: String,
    pub value: ExpressionNode,
}

pub struct CallNode {
    pub callable_name: String,
    pub params: Vec<ExpressionNode>,
}