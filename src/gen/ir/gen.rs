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
    cur_scope: Scope,
}

impl IrGenerator {
    pub fn new() -> Self {
        Self {
            program: Program::new(),
            symbol_table: SymbolTable::new(),
            cur_scope: Scope::Program,
        }
    }

    pub fn generate_on(&mut self, ast: &CompUnit) -> Result<(), Error> {
        ast.generate(self)?;
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

    pub fn cur_scope(&self) -> &Scope {
        &self.cur_scope
    }

    pub fn set_cur_scope(&mut self, scope: Scope) {
        self.cur_scope = scope;
    }
}

/// A trait for generating IR from AST nodes.
trait GenerateIr {
    type Output;

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error>;
}

impl GenerateIr for CompUnit {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        self.func_def.generate(generator)?;
        Ok(())
    }
}

impl GenerateIr for FuncDef {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        match generator.cur_scope() {
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
        // Downword AST, create a new scope
        let func = generator.program.new_func(func_data);
        let old_scope = generator.cur_scope().to_owned(); // preserve the old scope
        let new_scope = Scope::Function(func);
        generator.set_cur_scope(new_scope);
        self.block.generate(generator)?;
        generator.set_cur_scope(old_scope); // restore the scope
        Ok(())
    }
}

impl GenerateIr for Block {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
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
        // ! Attention: the current scope is a BasicBlock.
        // ! And it may change in function definition when generating the multi-block function body!
        // ! So should not change the scope here when generating the block items!
        let old_scope = generator.cur_scope().to_owned(); // preserve the old scope
        let new_scope = Scope::BasicBlock(func, block);
        generator.set_cur_scope(new_scope); // downward new scope
        for item in self.items.iter() {
            dbg!(generator.cur_scope());
            item.generate(generator)?;
        }
        // Exit the scope
        generator.set_cur_scope(old_scope); // restore after generation done
        generator.symbol_table_mut().exit_scope();

        Ok(())
    }
}

impl GenerateIr for BlockItem {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        match self {
            BlockItem::Stmt(s) => s.generate(generator),
            BlockItem::Decl(d) => d.generate(generator),
        }
    }
}

impl GenerateIr for Decl {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        match self {
            Decl::ConstDecl(c) => c.generate(generator),
            Decl::VarDecl(v) => v.generate(generator),
        }
    }
}

impl GenerateIr for ConstDecl {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("ConstDecl must be in a BasicBlock scope!"),
        };

        for def in self.defs.iter() {
            let scope = match self.ty {
                BType::Int => Scope::Decl(func, block, Type::get_i32()),
            };
            generator.set_cur_scope(scope); // downward new scope
            def.generate(generator)?;
            generator.set_cur_scope(Scope::BasicBlock(func, block)); // restore after lower nodes are done
        }

        Ok(())
    }
}

impl GenerateIr for ConstDef {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        match generator.cur_scope() {
            Scope::Decl(_, _, _) => (),
            _ => unreachable!("ConstDef must be in a Decl scope!"),
        };

        let const_val = self.init_val.generate(generator)?; // const_val is known at compile time by ConstInitVal

        let st = generator.symbol_table_mut(); // insert the const value into the symbol table
        st.try_insert(self.ident.clone(), SymbolValue::Const(const_val))?;

        Ok(())
    }
}

impl GenerateIr for ConstInitVal {
    type Output = i32;

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        match generator.cur_scope() {
            Scope::Decl(_, _, _) => (),
            _ => unreachable!("ConstInitVal must be in a Decl scope!"),
        };
        // ? Why need a enum for ConstInitVal?
        // TODO: Refactor ConstInitVal to be just a ConstExpr
        match self {
            ConstInitVal::ConstExpr(e) => Ok(e.eval(generator.symbol_table())?), // evaluate the expression
        }
    }
}

impl GenerateIr for VarDecl {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("VarDecl must be in a BasicBlock scope!"),
        };

        for def in self.defs.iter() {
            let scope = match self.ty {
                BType::Int => Scope::Decl(func, block, Type::get_i32()),
            };
            generator.set_cur_scope(scope); // downward new scope
            def.generate(generator)?;
            generator.set_cur_scope(Scope::BasicBlock(func, block)); // recover
        }

        Ok(())
    }
}

impl GenerateIr for VarDef {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block, ty) = match generator.cur_scope() {
            Scope::Decl(f, b, t) => (f, b, t),
            _ => unreachable!("VarDef must be in a Decl scope!"),
        };
        // note: make borrow checker happy
        let (func, block, ty) = (*func, *block, ty.clone());
        // Create a new variable using alloc instruction
        let func_data = generator.program_mut().func_mut(func);
        let dfg = func_data.dfg_mut();
        let alloc = dfg.new_value().alloc(ty.clone());
        func_data
            .layout_mut()
            .bb_mut(block)
            .insts_mut()
            .extend([alloc]);
        // Insert the variable into the symbol table
        let st = generator.symbol_table_mut();
        match self {
            VarDef::Ident(ident) => {
                st.try_insert(ident.clone(), SymbolValue::Var(alloc))?;
            }
            VarDef::Init(ident, init_val) => {
                st.try_insert(ident.clone(), SymbolValue::Var(alloc))?;
                // Generate the initial value
                let old_scope = generator.cur_scope().to_owned(); // preserve the old scope
                let new_scope = Scope::Decl(func, block, ty.clone());
                generator.set_cur_scope(new_scope); // downward new scope and soon recover
                let val = init_val.generate(generator)?;
                generator.set_cur_scope(old_scope);
                // Store the initial value into the variable
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

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        // ? Simalar to ConstInitVal, why need a enum for InitVal?
        // TODO: Refactor InitVal to be just a Expr
        match self {
            InitVal::Expr(e) => e.generate(generator),
        }
    }
}

impl GenerateIr for Stmt {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        match self {
            Stmt::Block(b) => b.generate(generator),
            Stmt::Expr(e) => match e {
                Some(e) => {
                    let _ = e.generate(generator)?;
                    Ok(())
                }
                None => Ok(()),
            },
            Stmt::If(s) => s.generate(generator),
            Stmt::Assign(s) => s.generate(generator),
            Stmt::Return(s) => s.generate(generator),
        }
    }
}

impl GenerateIr for AssignStmt {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("AssignStmt must be in a BasicBlock scope!"),
        };
        // ? Simalar to ConstInitVal, why need a enum for LVal?
        // TODO: Refactor LVal to be just a String
        match &self.l_val {
            LVal::Ident(ident) => {
                let tbl_val = generator
                    .symbol_table()
                    .get(&ident)
                    .ok_or(Error::SemanticError(format!("Undefined symbol: {}", ident)))?;
                // Semantic check for assignment
                let &lhs = match tbl_val {
                    SymbolValue::Var(v) => v,
                    _ => {
                        return Err(Error::SemanticError(format!(
                            "Can not assign to a constant: {}",
                            ident
                        )))
                    }
                };
                // Generate the right-hand side expression
                let rhs = self.r_expr.generate(generator)?;
                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();
                // Store the new value into the variable
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

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            _ => unreachable!("IfStmt must be in a BasicBlock scope!"),
        };
        dbg!(block);
        // Generate condition
        let cond = self.cond.generate(generator)?;
        // Generate then block without jump
        let func_data = generator.program_mut().func_mut(func);
        let dfg = func_data.dfg_mut();
        let then_bb = dfg.new_bb().basic_block(Some("%then".into()));
        func_data.layout_mut().bbs_mut().extend([then_bb]);
        // Generate instructions in then block
        let old_scope = generator.cur_scope().to_owned();
        let new_scope = Scope::BasicBlock(func, then_bb);
        generator.set_cur_scope(new_scope);
        self.then_stmt.generate(generator)?;
        generator.set_cur_scope(old_scope);
        // Complete then block (except for jump), recover
        // Generate else block without jump if exists
        let else_bb = match &self.else_stmt {
            Some(else_stmt) => {
                let func_data = generator.program_mut().func_mut(func);
                let dfg = func_data.dfg_mut();
                let else_bb = dfg.new_bb().basic_block(Some("%else".into()));
                func_data.layout_mut().bbs_mut().extend([else_bb]);
                // Generate instructions in else block
                let old_scope = generator.cur_scope().to_owned();
                let new_scope = Scope::BasicBlock(func, else_bb);
                generator.set_cur_scope(new_scope);
                else_stmt.generate(generator)?;
                generator.set_cur_scope(old_scope);
                // Complete else block (except for jump), recover
                Some(else_bb)
            }
            None => None,
        };
        // Generate end block
        let func_data = generator.program_mut().func_mut(func);
        let dfg = func_data.dfg_mut();
        let end_bb = dfg.new_bb().basic_block(Some("%end".into()));
        func_data.layout_mut().bbs_mut().extend([end_bb]);
        // Generate branch and jump instructions
        let dfg = func_data.dfg_mut();
        let jump = dfg.new_value().jump(end_bb);
        match else_bb {
            Some(else_bb) => {
                let branch = dfg.new_value().branch(cond, then_bb, else_bb);
                // Insert instructions (branch to current block, jump to then and else block)
                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([branch]);
                func_data
                    .layout_mut()
                    .bb_mut(then_bb)
                    .insts_mut()
                    .extend([jump]);
                func_data
                    .layout_mut()
                    .bb_mut(else_bb)
                    .insts_mut()
                    .extend([jump]);
            }
            None => {
                let branch = dfg.new_value().branch(cond, then_bb, end_bb);
                // Insert instructions (branch to current block, jump to then block)
                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([branch]);
                func_data
                    .layout_mut()
                    .bb_mut(then_bb)
                    .insts_mut()
                    .extend([jump]);
            }
        }

        // ! Attention: after the if-else statement, the current block should be the end block
        let new_scope = Scope::BasicBlock(func, end_bb);
        generator.set_cur_scope(new_scope);

        Ok(())
    }
}

impl GenerateIr for ReturnStmt {
    type Output = ();

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            _ => {
                dbg!(generator.cur_scope());
                unreachable!("ReturnStmt must be in a BasicBlock scope!")
            }
        };

        match &self.ret_val {
            Some(e) => {
                let ret_val = e.generate(generator)?;

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

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        // ! Attention: Should not refactor LOrExpr to be just a Expr, as the recursive nature of the enum Expr
        match self {
            Expr::LOrExpr(e) => e.generate(generator),
        }
    }
}

impl GenerateIr for PrimaryExpr {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let func = match *generator.cur_scope() {
            Scope::BasicBlock(f, _) => f,
            Scope::Decl(f, _, _) => f,
            _ => unreachable!("PrimaryExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            PrimaryExpr::Number(n) => {
                let dfg = generator.program_mut().func_mut(func).dfg_mut();
                Ok(dfg.new_value().integer(*n))
            }
            PrimaryExpr::Expr(e) => e.generate(generator),
            PrimaryExpr::LVal(l_val) => l_val.generate(generator),
        }
    }
}

impl GenerateIr for LVal {
    type Output = Value;

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
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

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("UnaryExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            UnaryExpr::PrimaryExpr(e) => e.generate(generator),
            UnaryExpr::Unary(op, e) => {
                let val = e.generate(generator)?;
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

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("MulExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            MulExpr::UnaryExpr(e) => e.generate(generator),
            MulExpr::Mul(mul_expr, op, unary_expr) => {
                let lhs = mul_expr.generate(generator)?;
                let rhs = unary_expr.generate(generator)?;

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

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("AddExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            AddExpr::MulExpr(e) => e.generate(generator),
            AddExpr::Add(add_expr, op, mul_expr) => {
                let lhs = add_expr.generate(generator)?;
                let rhs = mul_expr.generate(generator)?;

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

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("RelExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            RelExpr::AddExpr(e) => e.generate(generator),
            RelExpr::Rel(rel_expr, op, add_expr) => {
                let lhs = rel_expr.generate(generator)?;
                let rhs = add_expr.generate(generator)?;

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

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("EqExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            EqExpr::RelExpr(e) => e.generate(generator),
            EqExpr::Eq(eq_expr, op, rel_expr) => {
                let lhs = eq_expr.generate(generator)?;
                let rhs = rel_expr.generate(generator)?;

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

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("LAndExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            LAndExpr::EqExpr(e) => e.generate(generator),
            LAndExpr::LAnd(l_and_expr, eq_expr) => {
                let lhs = l_and_expr.generate(generator)?;
                let rhs = eq_expr.generate(generator)?;

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

    fn generate(&self, generator: &mut IrGenerator) -> Result<Self::Output, Error> {
        let (func, block) = match *generator.cur_scope() {
            Scope::BasicBlock(f, b) => (f, b),
            Scope::Decl(f, b, _) => (f, b),
            _ => unreachable!("LOrExpr must be in a BasicBlock or Decl scope!"),
        };

        match self {
            LOrExpr::LAndExpr(e) => e.generate(generator),
            LOrExpr::LOr(l_or_expr, l_and_expr) => {
                let lhs = l_or_expr.generate(generator)?;
                let rhs = l_and_expr.generate(generator)?;

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
