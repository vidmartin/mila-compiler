
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

impl fmt::Display for ast::ProgramNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "program {}", self.name)?;
        
        writeln!(f, "{}", if self.declarations.constants.is_empty() { "  - constants: (empty)" } else { "  - constants:" })?;
        for constant in self.declarations.constants.iter() {
            let s = format!("{}", constant);
            writeln!(f, "{}", indent(s, 8, true))?;
        }

        writeln!(f, "{}", if self.declarations.variables.is_empty() { "  - variables: (empty)" } else { "  - variables:" })?;
        for variable in self.declarations.variables.iter() {
            let s = format!("{}", variable);
            writeln!(f, "{}", indent(s, 8, true))?;
        }

        writeln!(f, "{}", if self.declarations.callables.is_empty() { "  - callables: (empty)" } else { "  - callables:" })?;
        for callable in self.declarations.callables.iter() {
            let s = format!("{}", callable);
            writeln!(f, "{}", indent(s, 8, true))?;
        }

        Ok(())
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
            ast::DataType::OneInternal(_) => write!(f, "(internal data type)"),
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
        
        if self.params.is_empty() {
            writeln!(f, "  - params: (empty)")?;
        } else {
            writeln!(f, "  - param_types:")?;
            for (param, dtype) in self.params.iter() {
                writeln!(f, "      - {} : {}", param, dtype)?;
            }
        }

        if let Some(implementation) = self.implementation.as_ref() {
            if implementation.variables.is_empty() {
                writeln!(f, "  - variables: (empty)")?;
            } else {
                writeln!(f, "  - variables:")?;
                for var in implementation.variables.iter() {
                    writeln!(f, "      - {} : {}", var.name, var.dtype)?;
                }
            }
    
            writeln!(f, "  - implementation:")?;
            let s = format!("{}", implementation.implementation);
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
            ast::StatementNode::IfStatement(ifstmt) => {
                writeln!(f, "if")?;
                writeln!(f, "  - condition: ")?;
                let s = format!("{}", ifstmt.condition);
                writeln!(f, "{}", indent(s, 8, true))?;
                writeln!(f, "  - yes: ")?;
                let s = format!("{}", *ifstmt.yes);
                writeln!(f, "{}", indent(s, 8, true))?;
                if let Some(no) = &ifstmt.no {
                    let s = format!("{}", *no);
                    writeln!(f, "  - no: ")?;
                    writeln!(f, "{}", indent(s, 8, true))?;
                } else {
                    writeln!(f, "  - no: (empty)")?;
                }
            },
            ast::StatementNode::ForLoop(forloop) => {
                writeln!(
                    f,
                    "for {}",
                    forloop.iterating.name,
                )?;
                writeln!(f, "  - range: A --({})--> B", forloop.range.step)?;
                let s = format!("A: {}", forloop.range.lhs);
                writeln!(f, "{}", indent(s, 8, true))?;
                let s = format!("B: {}", forloop.range.rhs);
                writeln!(f, "{}", indent(s, 8, true))?;
                writeln!(f, "  - inner:")?;
                let s = format!("{}", *forloop.inner);
                writeln!(f, "{}", indent(s, 8, true))?;
            },
            ast::StatementNode::WhileLoop(wloop) => {
                writeln!(f, "while")?;
                writeln!(f, "  - condition: ")?;
                let s = format!("{}", wloop.condition);
                writeln!(f, "{}", indent(s, 8, true))?;
                writeln!(f, "  - inner: ")?;
                let s = format!("{}", wloop.inner);
                writeln!(f, "{}", indent(s, 8, true))?;
            },
            ast::StatementNode::Exit => {
                writeln!(f, "exit")?;
            },
            ast::StatementNode::Break => {
                writeln!(f, "break")?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for ast::AssignmentNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "AssignmentNode:")?;
        writeln!(f, "  - target:")?;
        let s = format!("{}", self.target);
        writeln!(f, "{}", indent(s, 8, true))?;
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
            ast::ExpressionNode::ArrayAccess(node) => {
                writeln!(f, "A [ B ]")?;
                writeln!(f, "{}", indent(format!("A: {}", node.array), 4, true))?;
                writeln!(f, "{}", indent(format!("B: {}", node.index), 4, true))?;
                Ok(())
            },
            ast::ExpressionNode::BinaryOperator(binop) => {
                writeln!(f, "{}", binop)?;
                Ok(())
            }
        }
    }
}

impl fmt::Display for ast::BinaryOperatorNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "A {} B", Into::<crate::tokens::Token>::into(self.kind))?;
        writeln!(f, "{}", indent(format!("A: {}", self.lhs), 4, true))?;
        writeln!(f, "{}", indent(format!("B: {}", self.rhs), 4, true))?;
        Ok(())
    }
}

impl fmt::Display for ast::LiteralNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ast::LiteralNode::Integer(integer) => writeln!(f, "integer({})", integer),
            ast::LiteralNode::String(string) => writeln!(f, "string({})", string),
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
