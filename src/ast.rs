
use crate::tokens::Token;

pub enum StatementNode {
    StatementBlock(StatementBlockNode),
    Assignment(AssignmentNode),
    Expression(ExpressionNode),
    ForLoop(ForLoopNode),
    WhileLoop(WhileLoopNode),
    IfStatement(IfStatementNode),
    Exit,
}

pub struct IfStatementNode {
    pub condition: ExpressionNode,
    pub yes: Box<StatementNode>,
    pub no: Option<Box<StatementNode>>,
}

pub struct ForLoopNode {
    pub iterating: StorageDeclarationNode,
    pub range: RangeNode,
    pub inner: Box<StatementNode>,
}

pub struct RangeNode {
    pub lhs: ExpressionNode,
    pub rhs: ExpressionNode,
    pub step: i64,
}

pub struct WhileLoopNode {
    pub condition: ExpressionNode,
    pub inner: Box<StatementNode>,
}

pub enum ExpressionNode {
    Call(CallNode),
    Literal(LiteralNode),
    Access(String),
    ArrayAccess(ArrayAccessNode),
    BinaryOperator(BinaryOperatorNode),
}

pub struct BinaryOperatorNode {
    pub kind: BinaryOperatorKind,
    pub lhs: Box<ExpressionNode>,
    pub rhs: Box<ExpressionNode>,
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOperatorKind {
    Add, Mul, Sub, Div, Mod,
    And, Or,
    Eq, Ne, Lt, Gt, Le, Ge,
}

impl TryFrom<Token> for BinaryOperatorKind {
    type Error = Token;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        Ok(match value {
            Token::TkAdd => Self::Add,
            Token::TkSub => Self::Sub,
            Token::TkMul => Self::Mul,
            Token::KwDiv => Self::Div,
            Token::KwMod => Self::Mod,
            Token::KwAnd => Self::And,
            Token::KwOr => Self::Or,
            Token::TkEq => Self::Eq,
            Token::TkNe => Self::Ne,
            Token::TkLe => Self::Le,
            Token::TkLt => Self::Lt,
            Token::TkGe => Self::Ge,
            Token::TkGt => Self::Gt,
            _ => Err(value)?
        })
    }
}

impl From<BinaryOperatorKind> for Token {
    fn from(value: BinaryOperatorKind) -> Self {
        match value {
            BinaryOperatorKind::Add => Self::TkAdd,
            BinaryOperatorKind::Mul => Self::TkMul,
            BinaryOperatorKind::Sub => Self::TkSub,
            BinaryOperatorKind::Div => Self::KwDiv,
            BinaryOperatorKind::Mod => Self::KwMod,
            BinaryOperatorKind::And => Self::KwAnd,
            BinaryOperatorKind::Or => Self::KwOr,
            BinaryOperatorKind::Eq => Self::TkEq,
            BinaryOperatorKind::Ne => Self::TkNe,
            BinaryOperatorKind::Lt => Self::TkLt,
            BinaryOperatorKind::Gt => Self::TkGt,
            BinaryOperatorKind::Le => Self::TkLe,
            BinaryOperatorKind::Ge => Self::TkGe,
        }
    }
}

pub struct ArrayAccessNode {
    pub array: Box<ExpressionNode>,
    pub index: Box<ExpressionNode>,
}

pub enum LiteralNode {
    Integer(i64),
    String(String),
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
    OneInternal(*mut llvm_sys::LLVMType),
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
    pub params: Vec<(String, DataType)>,
    pub return_type: Option<DataType>,
    pub implementation: Option<CallableImplementationNode>
}

pub struct CallableImplementationNode {
    pub variables: Vec<StorageDeclarationNode>,
    pub implementation: StatementBlockNode,
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
