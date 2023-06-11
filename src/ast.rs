
use crate::tokens::Token;

#[derive(Clone)]
pub enum StatementNode {
    StatementBlock(StatementBlockNode),
    Assignment(AssignmentNode),
    Expression(ExpressionNode),
    ForLoop(ForLoopNode),
    WhileLoop(WhileLoopNode),
    IfStatement(IfStatementNode),
    Exit,
}

#[derive(Clone)]
pub struct IfStatementNode {
    pub condition: ExpressionNode,
    pub yes: Box<StatementNode>,
    pub no: Option<Box<StatementNode>>,
}

#[derive(Clone)]
pub struct ForLoopNode {
    pub iterating: StorageDeclarationNode,
    pub range: RangeNode,
    pub inner: Box<StatementNode>,
}

#[derive(Clone)]
pub struct RangeNode {
    pub lhs: ExpressionNode,
    pub rhs: ExpressionNode,
    pub step: i64,
}

#[derive(Clone)]
pub struct WhileLoopNode {
    pub condition: ExpressionNode,
    pub inner: Box<StatementNode>,
}

#[derive(Clone)]
pub enum ExpressionNode {
    Call(CallNode),
    Literal(LiteralNode),
    Access(String),
    ArrayAccess(ArrayAccessNode),
    BinaryOperator(BinaryOperatorNode),
}

#[derive(Clone)]
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

impl BinaryOperatorKind {
    pub fn is_comparasion(&self) -> bool {
        if let Self::Eq | Self::Ne | Self::Lt | Self::Gt | Self::Le | Self::Ge = *self {
            true
        } else {
            false
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        if let Self::Add | Self::Mul | Self::Sub | Self::Div | Self::Mod = *self {
            true
        } else {
            false
        }
    }
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

#[derive(Clone)]
pub struct ArrayAccessNode {
    pub array: Box<ExpressionNode>,
    pub index: Box<ExpressionNode>,
}

#[derive(Clone)]
pub enum LiteralNode {
    Integer(i64),
    String(String),
}

/// top level node for the entire program
#[derive(Clone)]
pub struct ProgramNode {
    pub name: String,
    pub declarations: ProgramDeclarations,
}

#[derive(Clone)]
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
#[derive(Clone)]
pub struct StorageDeclarationNode {
    pub name: String,
    pub dtype: DataType,
    pub init: Option<LiteralNode>,
}

/// declaration of a function or a procedure
#[derive(Clone)]
pub struct CallableDeclarationNode {
    pub name: String,
    pub params: Vec<(String, DataType)>,
    pub return_type: Option<DataType>,
    pub implementation: Option<CallableImplementationNode>
}

#[derive(Clone)]
pub struct CallableImplementationNode {
    pub variables: Vec<StorageDeclarationNode>,
    pub implementation: StatementBlockNode,
}

#[derive(Clone)]
pub struct StatementBlockNode {
    pub statements: Vec<StatementNode>,
}

#[derive(Clone)]
pub struct AssignmentNode {
    pub target: ExpressionNode,
    pub value: ExpressionNode,
}

#[derive(Clone)]
pub struct CallNode {
    pub callable_name: String,
    pub params: Vec<ExpressionNode>,
}
