
use crate::ast;
use std::fmt;

/// removes trailing newline & indents text
pub fn indent(mut s: String, d: usize, dash: bool) -> String {
    s = s.trim_end().replace("\n", &("\n".to_string() + &" ".repeat(d)));
    if dash {
        s.insert_str(0, &(" ".repeat(d - 2) + "-" + &" ".repeat(1)));
    } else {
        s.insert_str(0, &(" ".repeat(d)));
    }
    return s;
}

impl fmt::Display for ast::ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ast::ASTNode::Program(program_node) => {
                writeln!(f, "ASTNode::Program {}", program_node.name)?;
                
                writeln!(f, "{}", if program_node.declarations.constants.is_empty() { "  - constants: (empty)" } else { "  - constants:" })?;
                for constant in program_node.declarations.constants.iter() {
                    let s = format!("{}", constant);
                    writeln!(f, "{}", indent(s, 8, true))?;
                }

                writeln!(f, "{}", if program_node.declarations.variables.is_empty() { "  - variables: (empty)" } else { "  - variables:" })?;
                for variable in program_node.declarations.variables.iter() {
                    let s = format!("{}", variable);
                    writeln!(f, "{}", indent(s, 8, true))?;
                }

                writeln!(f, "{}", if program_node.declarations.callables.is_empty() { "  - callables: (empty)" } else { "  - callables:" })?;
                for callable in program_node.declarations.callables.iter() {
                    let s = format!("{}", callable);
                    writeln!(f, "{}", indent(s, 8, true))?;
                }

                Ok(())
            },
        }
    }
}

impl fmt::Display for ast::StorageDeclarationNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(init) = self.init.as_ref() {
            writeln!(f, "{} : {} = {}", self.name, self.dtype, init)
        } else {
            writeln!(f, "{} : {}", self.name, self.dtype)
        }
    }
}

impl fmt::Display for ast::DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ast::DataType::One(typename) => write!(f, "{}", typename),
            ast::DataType::Array { item, from, to } => write!(f, "array [{} .. {}] of {}", from, to, item),
        }
    }
}

impl fmt::Display for ast::CallableDeclarationNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(s) = self.return_type.as_ref() {
            writeln!(f, "CallableDeclarationNode: function {}", self.name)?;
            writeln!(f, "  - return_type: {}", s)?;
        } else {
            writeln!(f, "CallableDeclarationNodee: procedure {}", self.name)?;
        }
        
        if self.param_types.is_empty() {
            writeln!(f, "  - param_types: (empty)")?;
        } else {
            writeln!(f, "  - param_types:")?;
            for dtype in self.param_types.iter() {
                writeln!(f, "      - {}", dtype)?;
            }
        }

        if let Some(im) = self.implementation.as_ref() {
            writeln!(f, "  - implementation:")?;
            let s = format!("{}", im);
            writeln!(f, "{}", indent(s, 8, true))?;
        } else {
            writeln!(f, "  - implementation: (none)")?;
        }

        Ok(())
    }
}

impl fmt::Display for ast::StatementBlockNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "StatementBlockNode: ")?;
        if self.statements.is_empty() {
            writeln!(f, "  - statements: (empty)")?;
        } else {
            writeln!(f, "  - statements:")?;
            for stmt in self.statements.iter() {
                let s = format!("{}", stmt);
                writeln!(f, "{}", indent(s, 8, true))?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for ast::StatementNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ast::StatementNode::StatementBlock(block) => {
                writeln!(f, "{}", block)?;
            },
            ast::StatementNode::Assignment(assignment) => {
                writeln!(f, "{}", assignment)?;
            },
            ast::StatementNode::Expression(expr) => {
                writeln!(f, "{}", expr)?;
            },
        }

        Ok(())
    }
}

impl fmt::Display for ast::AssignmentNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "AssignmentNode:")?;
        writeln!(f, "  - varname: {}", self.varname)?;
        writeln!(f, "  - value:")?;
        let s = format!("{}", self.value);
        writeln!(f, "{}", indent(s, 8, true))?;
        Ok(())
    }
}

impl fmt::Display for ast::ExpressionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ast::ExpressionNode::Call(call) => writeln!(f, "{}", call),
            ast::ExpressionNode::Literal(lit) => writeln!(f, "literal {}", lit),
            ast::ExpressionNode::Access(access) => writeln!(f, "access store {}", access),
            ast::ExpressionNode::BinOp { op, lhs: lhsopt, rhs: rhsopt } => {
                writeln!(f,
                    "{}{}{}",
                    if lhsopt.is_some() { "A " } else { "" },
                    op,
                    if rhsopt.is_some() { " B" } else { "" }
                )?;
                
                if let Some(lhs) = lhsopt {
                    writeln!(f, "{}", indent(format!("A: {}", lhs), 4, true))?;
                }

                if let Some(rhs) = rhsopt {
                    writeln!(f, "{}", indent(format!("B: {}", rhs), 4, true))?;
                }

                Ok(())
            }
        }
    }
}

impl fmt::Display for ast::LiteralNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ast::LiteralNode::Integer(integer) => writeln!(f, "integer({})", integer),
        }
    }
}

impl fmt::Display for ast::CallNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "CallNode {}:", self.callable_name)?;
        if self.params.is_empty() {
            writeln!(f, "  - params: (empty)")?;
        } else {
            writeln!(f, "  - params:")?;
            for param in self.params.iter() {
                let s = format!("{}", param);
                writeln!(f, "{}", indent(s, 8, true))?;
            }
        }

        Ok(())
    }
}
