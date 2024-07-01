use super::{
    eval::EvaluateConstant,
    scope::Scope,
    symbol_table::{SymbolTable, SymbolValue},
};
use crate::{ast::*, error::Error};
use koopa::ir::{self, builder_traits::*, BinaryOp, Program, Type, Value};

/// A generator for IR.
pub struct IrGenerator {
    program: Program,
    symbol_table: SymbolTable,
}

impl IrGenerator {
    pub fn new() -> Self {
        Self {
            program: Program::new(),
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn generate_on(&mut self, ast: &CompUnit) -> Result<(), Error> {
        ast.generate(self, Scope::Program)?;
        Ok(())
    }

    pub fn program(&self) -> &Program {
        &self.program
    }

    pub fn program_mut(&mut self) -> &mut Program {
        &mut self.program
    }

    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    pub fn symbol_table_mut(&mut self) -> &mut SymbolTable {
        &mut self.symbol_table
    }
}

/// A trait for generating IR from AST nodes.
trait GenerateIr {
    type Output;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error>;
}

impl GenerateIr for CompUnit {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        self.func_def.generate(generator, scope)?;
        Ok(())
    }
}

impl GenerateIr for FuncDef {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        match scope {
            Scope::Program => (),
            _ => unreachable!("Function definition must be in Program scope!"),
        }
        // Create a new basic block for the function
        let mut func_data = ir::FunctionData::new(
            format!("@{}", self.ident),
            vec![],
            match self.func_type {
                FuncType::Int => Type::get_i32(),
            },
        );
        // Create an entry block
        let entry = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);
        // Add the function with the entry block to the program
        let func = generator.program.new_func(func_data);
        self.block.generate(generator, Scope::Function(func))?;
        Ok(())
    }
}

impl GenerateIr for Block {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::Function(f) => (
                f,
                // Get the entry block of the function (Upper FuncDef calls generation)
                generator
                    .program
                    .func(f)
                    .layout()
                    .entry_bb()
                    .ok_or(Error::SemanticError(format!(
                        "Defined function should have an entry block",
                    )))?,
            ),
            // Nested blocks
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("Block must be in a Function or BaiscBlock scope!"),
        };

        // Enter a new scope
        generator.symbol_table_mut().enter_scope();

        // Generate IR for each item in the block
        for item in self.items.iter() {
            item.generate(generator, Scope::BasicBlock(func, block))?;
        }

        // Exit the scope
        generator.symbol_table_mut().exit_scope();

        Ok(())
    }
}

impl GenerateIr for BlockItem {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        match self {
            BlockItem::Stmt(s) => s.generate(generator, scope),
            BlockItem::Decl(d) => d.generate(generator, scope),
        }
    }
}

impl GenerateIr for Decl {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        match self {
            Decl::ConstDecl(c) => c.generate(generator, scope),
            Decl::VarDecl(v) => v.generate(generator, scope),
        }
    }
}

impl GenerateIr for ConstDecl {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("ConstDecl must be in a BasicBlock scope!"),
        };

        let scope = match self.ty {
            BType::Int => Scope::Decl(func, block, Type::get_i32()),
        };

        for def in self.defs.iter() {
            def.generate(generator, scope.clone())?
        }

        Ok(())
    }
}

impl GenerateIr for ConstDef {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        match scope {
            Scope::Decl(_, _, _) => (),
            _ => unreachable!("ConstDef must be in a Decl scope!"),
        };

        let const_val = self.init_val.generate(generator, scope)?;

        let st = generator.symbol_table_mut();
        st.try_insert(self.ident.clone(), SymbolValue::Const(const_val))?;

        Ok(())
    }
}

impl GenerateIr for ConstInitVal {
    type Output = i32;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        match scope {
            Scope::Decl(_, _, _) => (),
            _ => unreachable!("ConstInitVal must be in a Decl scope!"),
        };

        match self {
            ConstInitVal::ConstExpr(e) => {
                let sym_table = generator.symbol_table();
                Ok(e.eval(sym_table)?)
            }
        }
    }
}

impl GenerateIr for VarDecl {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("VarDecl must be in a BasicBlock scope!"),
        };

        let scope = match self.ty {
            BType::Int => Scope::Decl(func, block, Type::get_i32()),
        };

        for def in self.defs.iter() {
            def.generate(generator, scope.clone())?
        }

        Ok(())
    }
}

impl GenerateIr for VarDef {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block, ty) = match scope {
            Scope::Decl(f, b, t) => (f, b, t),
            _ => unreachable!("VarDef must be in a Decl scope!"),
        };

        let func_data = generator.program_mut().func_mut(func);
        let dfg = func_data.dfg_mut();
        let alloc = dfg.new_value().alloc(ty.clone());
        func_data
            .layout_mut()
            .bb_mut(block)
            .insts_mut()
            .extend([alloc]);

        let st = generator.symbol_table_mut();
        match self {
            VarDef::Ident(ident) => {
                st.try_insert(ident.clone(), SymbolValue::Var(alloc))?;
            }
            VarDef::Init(ident, init_val) => {
                st.try_insert(ident.clone(), SymbolValue::Var(alloc))?;

                let val = init_val.generate(generator, Scope::Decl(func, block, ty.clone()))?;

                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();
                let store = dfg.new_value().store(val, alloc);
                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([store]);
            }
        }

        Ok(())
    }
}

impl GenerateIr for InitVal {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        match self {
            InitVal::Expr(e) => e.generate(generator, scope),
        }
    }
}

impl GenerateIr for Stmt {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        match self {
            Stmt::Block(b) => b.generate(generator, scope),
            Stmt::Expr(e) => match e {
                Some(e) => {
                    let _ = e.generate(generator, scope)?;
                    Ok(())
                }
                None => Ok(()),
            },
            Stmt::If(s) => s.generate(generator, scope),
            Stmt::Assign(s) => s.generate(generator, scope),
            Stmt::Return(s) => s.generate(generator, scope),
        }
    }
}

impl GenerateIr for AssignStmt {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("AssignStmt must be in a BasicBlock scope!"),
        };

        match &self.l_val {
            LVal::Ident(ident) => {
                let tbl_val = generator
                    .symbol_table()
                    .get(&ident)
                    .ok_or(Error::SemanticError(format!("Undefined symbol: {}", ident)))?;

                let &lhs = match tbl_val {
                    SymbolValue::Var(v) => v,
                    _ => {
                        return Err(Error::SemanticError(format!(
                            "Can not assign to a constant: {}",
                            ident
                        )))
                    }
                };

                let rhs = self.r_expr.generate(generator, scope)?;
                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();

                let store = dfg.new_value().store(rhs, lhs);

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([store]);
            }
        }

        Ok(())
    }
}

impl GenerateIr for IfStmt {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        todo!()
    }
}

impl GenerateIr for ReturnStmt {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("ReturnStmt must be in a BasicBlock scope!"),
        };

        match &self.ret_val {
            Some(e) => {
                let ret_val = e.generate(generator, scope)?;

                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();

                let ret = dfg.new_value().ret(Some(ret_val));

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([ret]);
            }
            None => {
                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();

                let ret = dfg.new_value().ret(None);

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([ret]);
            }
        }

        Ok(())
    }
}

impl GenerateIr for Expr {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        // no need to check scope here as Expr can only be in Stmt, and check has been done in Stmt
        // also scope not used here
        match self {
            Expr::LOrExpr(e) => e.generate(generator, scope),
        }
    }
}

impl GenerateIr for PrimaryExpr {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let func = match scope {
            Scope::BasicBlock(f, _) => f,
            Scope::Decl(f, _, _) => f,
            _ => unreachable!("PrimaryExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            PrimaryExpr::Number(n) => {
                let dfg = generator.program_mut().func_mut(func).dfg_mut();
                Ok(dfg.new_value().integer(*n))
            }
            PrimaryExpr::Expr(e) => e.generate(generator, scope),
            PrimaryExpr::LVal(l_val) => l_val.generate(generator, scope),
        }
    }
}

impl GenerateIr for LVal {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("LVal must be in a BasicBlock or Decl scope!"),
        };

        match self {
            LVal::Ident(ident) => {
                let &tbl_val = generator
                    .symbol_table()
                    .get(ident)
                    .ok_or(Error::SemanticError(format!("Undefined symbol: {}", ident)))?;

                match tbl_val {
                    SymbolValue::Const(c) => {
                        let dfg = generator.program_mut().func_mut(func).dfg_mut();
                        let c = dfg.new_value().integer(c);
                        Ok(c)
                    }
                    SymbolValue::Var(v) => {
                        let func_data = generator.program_mut().func_mut(func);
                        let dfg = func_data.dfg_mut();
                        let load = dfg.new_value().load(v);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .extend([load]);
                        Ok(load)
                    }
                }
            }
        }
    }
}

impl GenerateIr for UnaryExpr {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("UnaryExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            UnaryExpr::PrimaryExpr(e) => e.generate(generator, scope),
            UnaryExpr::Unary(op, e) => {
                let val = e.generate(generator, scope)?;
                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();

                let unary = match op {
                    UnaryOp::Positive => val,
                    UnaryOp::Negative => {
                        let zero = dfg.new_value().integer(0);
                        let neg = dfg.new_value().binary(BinaryOp::Sub, zero, val);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .extend([neg]);
                        neg
                    }
                    UnaryOp::LogicalNot => {
                        let zero = dfg.new_value().integer(0);
                        let l_not = dfg.new_value().binary(BinaryOp::Eq, val, zero);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .extend([l_not]);
                        l_not
                    }
                };

                Ok(unary)
            }
        }
    }
}

impl GenerateIr for MulExpr {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("MulExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            MulExpr::UnaryExpr(e) => e.generate(generator, scope),
            MulExpr::Mul(mul_expr, op, unary_expr) => {
                let lhs = mul_expr.generate(generator, Scope::BasicBlock(func, block))?;
                let rhs = unary_expr.generate(generator, Scope::BasicBlock(func, block))?;

                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();

                let mul = match op {
                    MulOp::Multiply => dfg.new_value().binary(BinaryOp::Mul, lhs, rhs),
                    MulOp::Divide => dfg.new_value().binary(BinaryOp::Div, lhs, rhs),
                    MulOp::Module => dfg.new_value().binary(BinaryOp::Mod, lhs, rhs),
                };

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([mul]);

                Ok(mul)
            }
        }
    }
}

impl GenerateIr for AddExpr {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("AddExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            AddExpr::MulExpr(e) => e.generate(generator, scope),
            AddExpr::Add(add_expr, op, mul_expr) => {
                let lhs = add_expr.generate(generator, Scope::BasicBlock(func, block))?;
                let rhs = mul_expr.generate(generator, Scope::BasicBlock(func, block))?;

                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();

                let mul = match op {
                    AddOp::Add => dfg.new_value().binary(BinaryOp::Add, lhs, rhs),
                    AddOp::Subtract => dfg.new_value().binary(BinaryOp::Sub, lhs, rhs),
                };

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([mul]);

                Ok(mul)
            }
        }
    }
}

impl GenerateIr for RelExpr {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("RelExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            RelExpr::AddExpr(e) => e.generate(generator, scope),
            RelExpr::Rel(rel_expr, op, add_expr) => {
                let lhs = rel_expr.generate(generator, Scope::BasicBlock(func, block))?;
                let rhs = add_expr.generate(generator, Scope::BasicBlock(func, block))?;

                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();

                let rel = match op {
                    RelOp::Less => dfg.new_value().binary(BinaryOp::Lt, lhs, rhs),
                    RelOp::LessEqual => dfg.new_value().binary(BinaryOp::Le, lhs, rhs),
                    RelOp::Greater => dfg.new_value().binary(BinaryOp::Gt, lhs, rhs),
                    RelOp::GreaterEqual => dfg.new_value().binary(BinaryOp::Ge, lhs, rhs),
                };

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([rel]);

                Ok(rel)
            }
        }
    }
}

impl GenerateIr for EqExpr {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("EqExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            EqExpr::RelExpr(e) => e.generate(generator, scope),
            EqExpr::Eq(eq_expr, op, rel_expr) => {
                let lhs = eq_expr.generate(generator, Scope::BasicBlock(func, block))?;
                let rhs = rel_expr.generate(generator, Scope::BasicBlock(func, block))?;

                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();

                let eq = match op {
                    EqOp::Equal => dfg.new_value().binary(BinaryOp::Eq, lhs, rhs),
                    EqOp::NotEqual => dfg.new_value().binary(BinaryOp::NotEq, lhs, rhs),
                };

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([eq]);

                Ok(eq)
            }
        }
    }
}

impl GenerateIr for LAndExpr {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("LAndExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            LAndExpr::EqExpr(e) => e.generate(generator, scope),
            LAndExpr::LAnd(l_and_expr, eq_expr) => {
                let lhs = l_and_expr.generate(generator, Scope::BasicBlock(func, block))?;
                let rhs = eq_expr.generate(generator, Scope::BasicBlock(func, block))?;

                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();

                let zero = dfg.new_value().integer(0);
                let logic_lhs = dfg.new_value().binary(BinaryOp::NotEq, lhs, zero);
                let logic_rhs = dfg.new_value().binary(BinaryOp::NotEq, rhs, zero);
                let l_and = dfg.new_value().binary(BinaryOp::And, logic_lhs, logic_rhs);

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([logic_lhs, logic_rhs, l_and]);

                Ok(l_and)
            }
        }
    }
}

impl GenerateIr for LOrExpr {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator, scope: Scope) -> Result<Self::Output, Error> {
        let (func, block) = match scope {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("LOrExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            LOrExpr::LAndExpr(e) => e.generate(generator, scope),
            LOrExpr::LOr(l_or_expr, l_and_expr) => {
                let lhs = l_or_expr.generate(generator, Scope::BasicBlock(func, block))?;
                let rhs = l_and_expr.generate(generator, Scope::BasicBlock(func, block))?;

                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();

                let zero = dfg.new_value().integer(0);
                let logic_lhs = dfg.new_value().binary(BinaryOp::NotEq, lhs, zero);
                let logic_rhs = dfg.new_value().binary(BinaryOp::NotEq, rhs, zero);
                let l_or = dfg.new_value().binary(BinaryOp::Or, logic_lhs, logic_rhs);

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([logic_lhs, logic_rhs, l_or]);

                Ok(l_or)
            }
        }
    }
}
