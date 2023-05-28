
use crate::tokens::Token;

pub enum ASTNode {
    Program(ProgramNode),
}

pub enum StatementNode {
    StatementBlock(StatementBlockNode),
    Assignment(AssignmentNode),
    Expression(ExpressionNode),
    ForLoop(ForLoopNode),
    WhileLoop(WhileLoopNode),
    IfStatement(IfStatementNode)
}

pub struct IfStatementNode {
    pub condition: ExpressionNode,
    pub yes: Box<StatementNode>,
    pub no: Option<Box<StatementNode>>,
}

pub struct ForLoopNode {
    pub iterating: StorageDeclarationNode,
    pub range: BinaryOperator,
    pub inner: Box<StatementNode>,
}

pub struct WhileLoopNode {
    pub condition: ExpressionNode,
    pub inner: Box<StatementNode>,
}

pub enum ExpressionNode {
    Call(CallNode),
    Literal(LiteralNode),
    Access(String),
    ArrayAccess { array: Box<ExpressionNode>, index: Box<ExpressionNode> },
    BinOp(BinaryOperator),
}

pub struct BinaryOperator {
    pub op: Token,
    pub lhs: Option<Box<ExpressionNode>>,
    pub rhs: Option<Box<ExpressionNode>>,
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

#[derive(Clone)]
pub enum DataType {
    One(String),
    Array {
        item: Box<DataType>,
        from: i64,
        to: i64,
    }
}

/// declaration of a variable or a constant
pub struct StorageDeclarationNode {
    pub name: String,
    pub dtype: DataType,
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
    pub target: ExpressionNode,
    pub value: ExpressionNode,
}

pub struct CallNode {
    pub callable_name: String,
    pub params: Vec<ExpressionNode>,
}
