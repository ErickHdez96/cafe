use crate::{
    arena::Arena,
    asm::{Condition, Direction, Inst, Insts, Register, ISA},
    ir,
    span::Span,
    symbol::Symbol,
    syntax::ast,
    ty::TyK,
    utils::mangle_symbol,
};

pub fn codegen<I>(ir: ir::Package, isa: &I, types: &Arena<TyK>) -> Insts
where
    I: ISA,
{
    Insts(Codegen::new(isa, types).generate(ir))
}

struct Codegen<'tyc, 'isa, I> {
    output: Vec<Inst>,
    /// The offset, size in the stack where the locals are stored (if at all).
    locals_stack: Vec<(u32, u32)>,
    /// A Stack of temporary registers that may be used to calculate temporary values.
    temp_registers: &'static [Register],
    temp_offset: usize,
    arg_registers: &'static [Register],
    arg_offset: usize,
    isa: &'isa I,
    types: &'tyc Arena<TyK>,
}

impl<'tyc, 'isa, I> Codegen<'tyc, 'isa, I>
where
    I: ISA,
{
    const FUNCTION_ALIGNMENT: u8 = 4;

    fn new(isa: &'isa I, types: &'tyc Arena<TyK>) -> Self {
        Self {
            output: vec![],
            locals_stack: vec![],
            temp_registers: &Register::TEMPORARY_REGISTERS,
            temp_offset: 0,
            arg_registers: &Register::PARAM_REGISTERS,
            arg_offset: 0,
            isa,
            types,
        }
    }

    fn add_insts(&mut self, mut insts: Vec<Inst>) {
        self.output.append(&mut insts);
    }

    fn i_binary_op(
        &mut self,
        op: ir::BinaryOp,
        dst: Register,
        src1: Register,
        src2: Register,
        span: Span,
    ) {
        use ir::BinaryOp::*;
        match op {
            Add => self.output.push(Inst::new(
                span,
                format!("\tadd {}, {}, {}", dst, src1, src2),
            )),
            Sub => todo!(),
            Mul => todo!(),
            Div => todo!(),
            LShift => todo!(),
            RShift => todo!(),
            And => todo!(),
            Or => todo!(),
            Xor => todo!(),
            Lt => todo!(),
            Lte => todo!(),
            Gt => todo!(),
            Gte => todo!(),
            EqEq => todo!(),
            NEq => todo!(),
        }
    }

    fn i_binary_op_i(
        &mut self,
        op: ir::BinaryOp,
        dst: Register,
        src: Register,
        imm: u16,
        span: Span,
    ) {
        use ir::BinaryOp::*;
        match op {
            Add => self
                .output
                .push(Inst::new(span, format!("\tadd {}, {}, #{}", dst, src, imm))),
            //Sub => self.output.push(Instruction::Opcode {
            //    span,
            //    opcode: Opcode::SubI {
            //        dst,
            //        src,
            //        shift: false,
            //        imm,
            //    },
            //}),
            //Mul => {
            //    let src2 = self.get_register();
            //    self.output.push(Instruction::Opcode {
            //        span,
            //        opcode: Opcode::MovZ {
            //            dst: src2,
            //            shift: 0,
            //            imm,
            //        },
            //    });
            //    self.output.push(Instruction::Opcode {
            //        span,
            //        opcode: Opcode::Mul {
            //            dst,
            //            src1: src,
            //            src2,
            //        },
            //    });
            //}
            //Div => {
            //    let src2 = self.get_register();
            //    self.output.push(Instruction::Opcode {
            //        span,
            //        opcode: Opcode::MovZ {
            //            dst: src2,
            //            shift: 0,
            //            imm,
            //        },
            //    });
            //    self.output.push(Instruction::Opcode {
            //        span,
            //        opcode: Opcode::UDiv {
            //            dst,
            //            src1: src,
            //            src2,
            //        },
            //    });
            //}
            LShift => todo!(),
            RShift => todo!(),
            And => todo!(),
            Or => todo!(),
            Xor => todo!(),
            Lt => todo!(),
            Lte => todo!(),
            Gt => todo!(),
            Gte => todo!(),
            EqEq => todo!(),
            NEq => todo!(),
            Sub => todo!(),
            Mul => todo!(),
            Div => todo!(),
        }
    }

    fn get_local_offset_from_sp(&self, local: ir::Local) -> i16 {
        let (offset, _) = self.locals_stack[local.value() as usize];
        offset.try_into().unwrap()
    }

    fn get_local_size(&self, local: ir::Local) -> u32 {
        let (_, size) = self.locals_stack[local.value() as usize];
        size
    }

    fn i_load_imm(&mut self, reg: Register, constant: u64, span: Span) {
        self.output
            .append(&mut Inst::load_immediate(reg, constant, span));
    }

    fn i_update_local(&mut self, reg: Register, local: ir::Local, span: Span) {
        if self.get_local_size(local) > 0 {
            let offset = self.get_local_offset_from_sp(local);
            self.output.push(Inst::new(
                span,
                format!(
                    "\tstr{} {}, [{}, #{}]",
                    match reg.size() {
                        1 => "b",
                        2 => "h",
                        _ => "",
                    },
                    reg,
                    Register::SP,
                    offset
                ),
            ));
        }
    }

    fn i_load_local(&mut self, reg: Register, local: ir::Local, span: Span) {
        let offset = self.get_local_offset_from_sp(local);
        self.output.push(Inst::new(
            span,
            format!(
                "\tldr{} {}, [{}, #{}]",
                match reg.size() {
                    1 => "b",
                    2 => "h",
                    _ => "",
                },
                reg,
                Register::SP,
                offset
            ),
        ));
    }

    fn i_load_label(&mut self, reg: Register, label: &ast::Path, span: Span) {
        let symbol = canonicalize_symbol(label);
        self.output.push(Inst::new(
            span,
            format!("\tadrp {}, {}@PAGE", reg, mangle_symbol(symbol)),
        ));
        self.output.push(Inst::new(
            span,
            format!("\tadd {}, {}, {}@PAGEOFF", reg, reg, mangle_symbol(symbol)),
        ));
    }

    fn i_store_register(&mut self, dst: Register, src: Register, span: Span) {
        self.output.push(Inst::new(
            span,
            format!(
                "\tstr{} {}, [{}]",
                match src.size() {
                    1 => "b",
                    2 => "h",
                    _ => "",
                },
                src,
                dst,
            ),
        ));
    }

    fn generate(mut self, mod_ir: ir::Package) -> Vec<Inst> {
        for body in mod_ir.bodies {
            self.gen_body(body);
        }

        self.output
    }

    fn gen_body(&mut self, mut body: ir::Body) {
        let label = if body.name.value.resolve() == "main" {
            body.name.value
        } else {
            canonicalize_symbol(&body.name)
        };
        self.emit(Inst::pseudo_align(Self::FUNCTION_ALIGNMENT));
        self.emit(Inst::pseudo_global(label));
        self.emit(Inst::pseudo_label(label));

        self.isa.process_body(&mut body, self.types);
        self.locals_stack = body
            .locals
            .iter()
            .map(|l| (l.stack_offset, l.size))
            .collect();
        self.add_insts(self.isa.proc_begin(&body));

        for bb in &body.basic_blocks {
            self.gen_basic_block(bb, &body);
        }
    }

    fn gen_basic_block(&mut self, bb: &ir::BasicBlockData, body: &ir::Body) {
        self.emit(Inst::pseudo_label(bb.idx.to_string().into()));

        for stmt in &bb.statements {
            self.gen_statement(stmt);
        }

        self.gen_terminator(bb.idx, &bb.terminator, body);
    }

    fn gen_statement(&mut self, stmt: &ir::Statement) {
        match &stmt.kind {
            ir::StatementKind::Nop => {}
            // local <- local
            ir::StatementKind::Copy(dst, src) => {
                let reg = self.get_register_for_local(*dst);
                self.i_load_local(reg, *src, stmt.span);
                if !dst.is_return() {
                    self.i_update_local(reg, *dst, stmt.span);
                    self.free_temp_register();
                }
            }
            // local <- imm
            ir::StatementKind::LoadI(dst, c) => {
                let reg = self.get_register_for_local(*dst);
                self.i_load_imm(reg, c.value, stmt.span);
                if !dst.is_return() {
                    self.i_update_local(reg, *dst, stmt.span);
                    self.free_temp_register();
                }
            }
            ir::StatementKind::BinaryOp(op, dst, src1, src2) => {
                use ir::BinaryOp::*;
                match op {
                    Add => {
                        let reg = self.get_register_for_local(*dst);
                        let reg1 = self.load_local(*src1, stmt.span);
                        let reg2 = self.load_local(*src2, stmt.span);
                        self.i_binary_op(*op, reg, reg1, reg2, stmt.span);
                        self.free_temp_register();
                        self.free_temp_register();
                        if !dst.is_return() {
                            self.free_temp_register();
                        }
                    }
                    Sub => todo!(),
                    Mul => todo!(),
                    Div => todo!(),
                    LShift => todo!(),
                    RShift => todo!(),
                    And => todo!(),
                    Or => todo!(),
                    Xor => todo!(),
                    Lt => todo!(),
                    Lte => todo!(),
                    Gt => todo!(),
                    Gte => todo!(),
                    EqEq => todo!(),
                    NEq => todo!(),
                }
            }
            ir::StatementKind::BinaryOpI(op, dst, src, imm) => {
                use ir::BinaryOp::*;
                match op {
                    Add | Sub | Mul | Div => {
                        let reg = self.get_register_for_local(*dst);
                        self.i_load_local(reg, *src, stmt.span);
                        self.i_binary_op_i(*op, reg, reg, imm.value.try_into().unwrap(), stmt.span);
                        if !dst.is_return() {
                            self.i_update_local(reg, *dst, stmt.span);
                            self.free_temp_register();
                        }
                    }
                    ir::BinaryOp::LShift => todo!(),
                    ir::BinaryOp::RShift => todo!(),
                    ir::BinaryOp::And => todo!(),
                    ir::BinaryOp::Or => todo!(),
                    ir::BinaryOp::Xor => todo!(),
                    ir::BinaryOp::Lt => todo!(),
                    ir::BinaryOp::Lte => todo!(),
                    ir::BinaryOp::Gt => todo!(),
                    ir::BinaryOp::Gte => todo!(),
                    ir::BinaryOp::EqEq => todo!(),
                    ir::BinaryOp::NEq => todo!(),
                }
            }
            ir::StatementKind::Load(_, _) => todo!(),
            ir::StatementKind::LoadAI(_, _, _) => todo!(),
            ir::StatementKind::LoadAO(_, _, _) => todo!(),
            ir::StatementKind::Store(_, _) => todo!(),
            ir::StatementKind::StoreAI(_, _, _) => todo!(),
            ir::StatementKind::StoreAO(_, _, _) => todo!(),
            ir::StatementKind::StoreLabel(lbl, src) => {
                let reg_src = self.load_local(*src, stmt.span);
                let reg_dst = self.get_native_register();
                self.i_load_label(reg_dst, lbl, stmt.span);
                self.i_store_register(reg_dst, reg_src, stmt.span);
                self.free_temp_register();
                self.free_temp_register();
            }
            ir::StatementKind::LoadLabel(dst, lbl) => {
                let reg = self.get_register_for_local(*dst);
                self.i_load_label(reg, lbl, stmt.span);
                if !dst.is_return() {
                    self.i_update_local(reg, *dst, stmt.span);
                    self.free_temp_register();
                }
            }
            ir::StatementKind::Arg(local) => {
                let reg = self
                    .next_arg_register()
                    .with_size(self.get_local_size(*local));
                self.i_load_local(reg, *local, stmt.span);
            }
            ir::StatementKind::Call(dst, fnloc) => {
                let reg = self.get_register_for_local(*dst);
                let reg1 = self.get_register_for_local(*fnloc);
                self.i_load_local(reg1, *fnloc, stmt.span);
                self.emit(Inst::call_register(reg1, stmt.span));
                if reg != Register::return_value() && self.get_local_size(*dst) > 0 {
                    self.emit(Inst::copy(reg, Register::return_value(), stmt.span));
                }
                self.free_temp_register();
                self.free_args();
                if !dst.is_return() {
                    self.i_update_local(reg, *dst, stmt.span);
                    self.free_temp_register();
                }
            }
            ir::StatementKind::CallLabel(dst, label) => {
                let reg = self.get_register_for_local(*dst);
                self.emit(Inst::call_label(label.value, stmt.span));
                self.free_args();
                if !dst.is_return() {
                    if self.get_local_size(*dst) > 0 {
                        self.emit(Inst::copy(reg, Register::return_value(), stmt.span));
                    }
                    self.i_update_local(reg, *dst, stmt.span);
                    self.free_temp_register();
                }
            }
        }
    }

    fn gen_terminator(&mut self, srcidx: usize, term: &ir::Terminator, body: &ir::Body) {
        match term.kind {
            ir::TerminatorKind::Goto(dstidx) => self.emit(Inst::jump(
                dstidx.value().to_string().into(),
                if dstidx.value() <= srcidx {
                    Direction::Backwards
                } else {
                    Direction::Forwards
                },
                term.span,
            )),
            ir::TerminatorKind::Return => {
                self.add_insts(self.isa.proc_end(body));
                self.emit(Inst::ret(term.span));
            }
            ir::TerminatorKind::Cond {
                cond,
                true_label,
                false_label,
            } => {
                let reg = self.load_local(cond, term.span);
                self.emit(Inst::cmp_immediate(reg, 0, term.span));
                self.emit(Inst::jump_cond(
                    true_label.value().to_string().into(),
                    if true_label.value() <= srcidx {
                        Direction::Backwards
                    } else {
                        Direction::Forwards
                    },
                    Condition::Ne,
                    term.span,
                ));
                self.emit(Inst::jump(
                    false_label.value().to_string().into(),
                    if false_label.value() <= srcidx {
                        Direction::Backwards
                    } else {
                        Direction::Forwards
                    },
                    term.span,
                ));
                if !cond.is_return() {
                    self.free_temp_register();
                }
            }
        }
    }

    fn get_register_for_local(&mut self, local: ir::Local) -> Register {
        let reg = if local.is_return() {
            Register::return_value()
        } else {
            self.next_temp_register()
        };
        reg.with_size(self.get_local_size(local))
    }

    fn get_native_register(&mut self) -> Register {
        self.next_temp_register()
            .with_size(I::POINTER_SIZE.try_into().unwrap())
    }

    fn load_local(&mut self, local: ir::Local, span: Span) -> Register {
        let reg = self.get_register_for_local(local);
        self.i_load_local(reg, local, span);
        reg
    }

    fn next_temp_register(&mut self) -> Register {
        let t = self
            .temp_registers
            .get(self.temp_offset)
            .expect("Compiler cannot handle using too many registers as of yet");
        self.temp_offset += 1;
        *t
    }

    fn free_temp_register(&mut self) {
        self.temp_offset -= 1;
    }

    fn next_arg_register(&mut self) -> Register {
        let a = self
            .arg_registers
            .get(self.arg_offset)
            .expect("Compiler cannot handle too many arguments");
        self.arg_offset += 1;
        *a
    }

    fn free_args(&mut self) {
        self.arg_offset = 0;
    }

    fn emit(&mut self, inst: Inst) {
        self.output.push(inst);
    }
}

pub fn canonicalize_symbol(path: &ast::Path) -> Symbol {
    let mod_name = path.module.resolve();
    let mut parent_path = mod_name
        .paths
        .iter()
        .map(|s| s.resolve())
        .collect::<Vec<&str>>();
    parent_path.push(path.value.resolve());
    parent_path.join(" ").into()
}

#[cfg(test)]
mod tests {
    use crate::{asm::Aarch64, interner::Interner, test::test_codegen_str};
    use expect_test::{expect, Expect};

    fn check(input: &str, expected: Expect) {
        let res = test_codegen_str(input, &Aarch64, &mut Interner::default());
        expected.assert_eq(&res.to_string());
    }

    mod primitives {
        use super::*;

        #[test]
        fn boolean() {
            check(
                "(define b #f)",
                expect![[r#"
                    .align 4
                    .global _T10_35_script8_64_init
                    _T10_35_script8_64_init:
                    	sub sp, sp, #32; (0:0..13)
                    	stp x29, x30, [sp, #16]; (0:0..13)
                    	add x29, sp, #16; (0:0..13)
                    0:
                    	mov w8, wzr; (0:10..2)
                    	strb w8, [sp, #15]; (0:10..2)
                    	ldrb w8, [sp, #15]; (0:10..2)
                    	adrp x9, _T10_35_script1b@PAGE; (0:10..2)
                    	add x9, x9, _T10_35_script1b@PAGEOFF; (0:10..2)
                    	strb w8, [x9]; (0:10..2)
                    	ldp x29, x30, [sp, #16]; (0:0..13)
                    	add sp, sp, #32; (0:0..13)
                    	ret x30; (0:0..13)"#]],
            );

            check(
                "(define b #t)",
                expect![[r#"
                    .align 4
                    .global _T10_35_script8_64_init
                    _T10_35_script8_64_init:
                    	sub sp, sp, #32; (0:0..13)
                    	stp x29, x30, [sp, #16]; (0:0..13)
                    	add x29, sp, #16; (0:0..13)
                    0:
                    	mov w8, #1; (0:10..2)
                    	strb w8, [sp, #15]; (0:10..2)
                    	ldrb w8, [sp, #15]; (0:10..2)
                    	adrp x9, _T10_35_script1b@PAGE; (0:10..2)
                    	add x9, x9, _T10_35_script1b@PAGEOFF; (0:10..2)
                    	strb w8, [x9]; (0:10..2)
                    	ldp x29, x30, [sp, #16]; (0:0..13)
                    	add sp, sp, #32; (0:0..13)
                    	ret x30; (0:0..13)"#]],
            );
        }

        //#[test]
        //fn usize() {
        //    check_intrinsics_expander(
        //        "(: main (-> usize))
        //         (define main (lambda () 100))",
        //        expect![[r#"
        //            .align 4
        //            .global main
        //            main:
        //            	sub sp, sp, #16; (79..108)
        //            	stp x29, x30, [sp, #0]; (79..108)
        //            	add x29, sp, #0; (79..108)
        //            0:
        //            	mov x0, #100; (103..106)
        //            	ldp x29, x30, [sp, #0]; (79..108)
        //            	add sp, sp, #16; (79..108)
        //            	ret x30; (103..106)"#]],
        //    );
        //}

        //#[test]
        //fn char() {
        //    check_intrinsics_expander(
        //        r"(: c (-> char))
        //          (define c (lambda () #\a))",
        //        expect![[r#"
        //            .align 4
        //            .global _T4root1c
        //            _T4root1c:
        //            	sub sp, sp, #16; (76..102)
        //            	stp x29, x30, [sp, #0]; (76..102)
        //            	add x29, sp, #0; (76..102)
        //            0:
        //            	mov w0, #97; (97..100)
        //            	ldp x29, x30, [sp, #0]; (76..102)
        //            	add sp, sp, #16; (76..102)
        //            	ret x30; (97..100)"#]],
        //    );
        //}
    }

    mod main {
        use super::*;

        #[test]
        fn success() {
            check(
                "(define main (lambda () 0))",
                expect![[r#"
                    .align 4
                    .global main
                    main:
                    	sub sp, sp, #16; (0:13..13)
                    	stp x29, x30, [sp, #0]; (0:13..13)
                    	add x29, sp, #0; (0:13..13)
                    0:
                    	mov x0, xzr; (0:24..1)
                    	ldp x29, x30, [sp, #0]; (0:13..13)
                    	add sp, sp, #16; (0:13..13)
                    	ret x30; (0:13..13)"#]],
            );
        }
    }

    //mod parameters {
    //    use super::*;

    //    #[test]
    //    fn simple_parameter() {
    //        check_intrinsics_expander(
    //            "(: idb (-> boolean boolean))
    //             (define idb (lambda (b) b))",
    //            expect![[r#"
    //                .align 4
    //                .global _T4root3idb
    //                _T4root3idb:
    //                	sub sp, sp, #32; (88..115)
    //                	stp x29, x30, [sp, #16]; (88..115)
    //                	add x29, sp, #16; (88..115)
    //                	strb w0, [sp, #15]; (88..115)
    //                0:
    //                	ldrb w0, [sp, #15]; (112..113)
    //                	ldp x29, x30, [sp, #16]; (88..115)
    //                	add sp, sp, #32; (88..115)
    //                	ret x30; (112..113)"#]],
    //        );
    //    }

    //    #[test]
    //    fn first_parameter() {
    //        check_intrinsics_expander(
    //            "(: first (-> usize boolean usize))
    //             (define first (lambda (u b) u))",
    //            expect![[r#"
    //                .align 4
    //                .global _T4root5first
    //                _T4root5first:
    //                	sub sp, sp, #32; (94..125)
    //                	stp x29, x30, [sp, #16]; (94..125)
    //                	add x29, sp, #16; (94..125)
    //                	str x0, [sp, #8]; (94..125)
    //                	strb w1, [sp, #7]; (94..125)
    //                0:
    //                	ldr x0, [sp, #8]; (122..123)
    //                	ldp x29, x30, [sp, #16]; (94..125)
    //                	add sp, sp, #32; (94..125)
    //                	ret x30; (122..123)"#]],
    //        );
    //    }

    //    #[test]
    //    fn second_parameter() {
    //        check_intrinsics_expander(
    //            "(: second (-> usize boolean boolean))
    //             (define second (lambda (u b) b))",
    //            expect![[r#"
    //                .align 4
    //                .global _T4root6second
    //                _T4root6second:
    //                	sub sp, sp, #32; (97..129)
    //                	stp x29, x30, [sp, #16]; (97..129)
    //                	add x29, sp, #16; (97..129)
    //                	str x0, [sp, #8]; (97..129)
    //                	strb w1, [sp, #7]; (97..129)
    //                0:
    //                	ldrb w0, [sp, #7]; (126..127)
    //                	ldp x29, x30, [sp, #16]; (97..129)
    //                	add sp, sp, #32; (97..129)
    //                	ret x30; (126..127)"#]],
    //        );
    //    }
    //}

    //mod primitive_fns {
    //    use super::*;

    //    #[test]
    //    fn add() {
    //        check_intrinsics(
    //            "(: main (-> usize))
    //             (define (main) (+ 4 10))",
    //            expect![[r#"
    //                .align 4
    //                .global main
    //                main:
    //                	sub sp, sp, #32; (283..307)
    //                	stp x29, x30, [sp, #16]; (283..307)
    //                	add x29, sp, #16; (283..307)
    //                0:
    //                	mov x8, #4; (301..302)
    //                	str x8, [sp, #8]; (301..302)
    //                	mov x8, #10; (303..305)
    //                	str x8, [sp, #0]; (303..305)
    //                	ldr x8, [sp, #8]; (298..306)
    //                	ldr x9, [sp, #0]; (298..306)
    //                	add x0, x8, x9; (298..306)
    //                	ldp x29, x30, [sp, #16]; (283..307)
    //                	add sp, sp, #32; (283..307)
    //                	ret x30; (298..306)"#]],
    //        );
    //    }

    //    #[test]
    //    fn write_char() {
    //        check_io(
    //            r"(: main (-> ()))
    //              (define (main) (write-char #\λ))",
    //            expect![[r#"
    //                .align 4
    //                .global main
    //                main:
    //                	sub sp, sp, #32; (231..264)
    //                	stp x29, x30, [sp, #16]; (231..264)
    //                	add x29, sp, #16; (231..264)
    //                0:
    //                	mov w8, #955; (258..262)
    //                	str w8, [sp, #12]; (258..262)
    //                	ldr w0, [sp, #12]; (258..262)
    //                	bl _T4rnrs5typed10intrinsics2io10write_char; (246..263)
    //                	ldp x29, x30, [sp, #16]; (231..264)
    //                	add sp, sp, #32; (231..264)
    //                	ret x30; (246..263)"#]],
    //        );

    //        check_io(
    //            r"(: main (-> ()))
    //              (define (main) (write-char #\𝇍))",
    //            expect![[r#"
    //                .align 4
    //                .global main
    //                main:
    //                	sub sp, sp, #32; (231..266)
    //                	stp x29, x30, [sp, #16]; (231..266)
    //                	add x29, sp, #16; (231..266)
    //                0:
    //                	mov w8, #53709; (258..264)
    //                	movk w8, #1, lsl #16; (258..264)
    //                	str w8, [sp, #12]; (258..264)
    //                	ldr w0, [sp, #12]; (258..264)
    //                	bl _T4rnrs5typed10intrinsics2io10write_char; (246..265)
    //                	ldp x29, x30, [sp, #16]; (231..266)
    //                	add sp, sp, #32; (231..266)
    //                	ret x30; (246..265)"#]],
    //        );
    //    }
    //}

    //mod fns {
    //    use super::*;

    //    #[test]
    //    fn unit_fn() {
    //        check_intrinsics_expander(
    //            "(: u (-> ()))
    //             (define (u) '())
    //             (: main (-> ()))
    //             (define (main) (u) (u))",
    //            expect![[r#"
    //                .align 4
    //                .global _T4root1u
    //                _T4root1u:
    //                	sub sp, sp, #16; (73..89)
    //                	stp x29, x30, [sp, #0]; (73..89)
    //                	add x29, sp, #0; (73..89)
    //                0:
    //                	ldp x29, x30, [sp, #0]; (73..89)
    //                	add sp, sp, #16; (73..89)
    //                	ret x30; (85..88)
    //                .align 4
    //                .global main
    //                main:
    //                	sub sp, sp, #16; (141..164)
    //                	stp x29, x30, [sp, #0]; (141..164)
    //                	add x29, sp, #0; (141..164)
    //                0:
    //                	bl _T4root1u; (156..159)
    //                	bl _T4root1u; (160..163)
    //                	ldp x29, x30, [sp, #0]; (141..164)
    //                	add sp, sp, #16; (141..164)
    //                	ret x30; (156..163)"#]],
    //        );
    //    }

    //    #[test]
    //    fn fn_as_param() {
    //        check_intrinsics(
    //            "(: call (-> (-> usize usize usize) usize usize usize))
    //             (define (call fn x y)
    //               (fn x y))
    //             (: main (-> usize))
    //             (define (main)
    //               (call + 4 5))",
    //            expect![[r#"
    //                .align 4
    //                .global _T4root4call
    //                _T4root4call:
    //                	sub sp, sp, #64; (318..368)
    //                	stp x29, x30, [sp, #48]; (318..368)
    //                	add x29, sp, #48; (318..368)
    //                	str x0, [sp, #40]; (318..368)
    //                	str x1, [sp, #32]; (318..368)
    //                	str x2, [sp, #24]; (318..368)
    //                0:
    //                	ldr x8, [sp, #32]; (363..364)
    //                	str x8, [sp, #16]; (363..364)
    //                	ldr x8, [sp, #24]; (365..366)
    //                	str x8, [sp, #8]; (365..366)
    //                	ldr x0, [sp, #16]; (363..364)
    //                	ldr x1, [sp, #8]; (365..366)
    //                	ldr x8, [sp, #40]; (359..367)
    //                	blr x8; (359..367)
    //                	ldp x29, x30, [sp, #48]; (318..368)
    //                	add sp, sp, #64; (318..368)
    //                	ret x30; (359..367)
    //                .align 4
    //                .global main
    //                main:
    //                	sub sp, sp, #48; (423..470)
    //                	stp x29, x30, [sp, #32]; (423..470)
    //                	add x29, sp, #32; (423..470)
    //                0:
    //                	adrp x8, _T4rnrs5typed10intrinsics3fns3add@PAGE; (463..464)
    //                	add x8, x8, _T4rnrs5typed10intrinsics3fns3add@PAGEOFF; (463..464)
    //                	str x8, [sp, #24]; (463..464)
    //                	mov x8, #4; (465..466)
    //                	str x8, [sp, #16]; (465..466)
    //                	mov x8, #5; (467..468)
    //                	str x8, [sp, #8]; (467..468)
    //                	ldr x0, [sp, #24]; (463..464)
    //                	ldr x1, [sp, #16]; (465..466)
    //                	ldr x2, [sp, #8]; (467..468)
    //                	bl _T4root4call; (457..469)
    //                	ldp x29, x30, [sp, #32]; (423..470)
    //                	add sp, sp, #48; (423..470)
    //                	ret x30; (457..469)"#]],
    //        );
    //    }

    //    #[test]
    //    fn call_inside_call() {
    //        check_intrinsics(
    //            "(: id (-> usize usize))
    //             (define (id u) u)
    //             (: main (-> usize))
    //             (define (main) (+ (id 1) (id 2)))",
    //            expect![[r#"
    //                .align 4
    //                .global _T4root2id
    //                _T4root2id:
    //                	sub sp, sp, #32; (287..304)
    //                	stp x29, x30, [sp, #16]; (287..304)
    //                	add x29, sp, #16; (287..304)
    //                	str x0, [sp, #8]; (287..304)
    //                0:
    //                	ldr x0, [sp, #8]; (302..303)
    //                	ldp x29, x30, [sp, #16]; (287..304)
    //                	add sp, sp, #32; (287..304)
    //                	ret x30; (302..303)
    //                .align 4
    //                .global main
    //                main:
    //                	sub sp, sp, #48; (359..392)
    //                	stp x29, x30, [sp, #32]; (359..392)
    //                	add x29, sp, #32; (359..392)
    //                0:
    //                	mov x8, #1; (381..382)
    //                	str x8, [sp, #16]; (381..382)
    //                	ldr x0, [sp, #16]; (381..382)
    //                	bl _T4root2id; (377..383)
    //                	mov x8, x0; (377..383)
    //                	str x8, [sp, #24]; (377..383)
    //                	mov x8, #2; (388..389)
    //                	str x8, [sp, #0]; (388..389)
    //                	ldr x0, [sp, #0]; (388..389)
    //                	bl _T4root2id; (384..390)
    //                	mov x8, x0; (384..390)
    //                	str x8, [sp, #8]; (384..390)
    //                	ldr x8, [sp, #24]; (374..391)
    //                	ldr x9, [sp, #8]; (374..391)
    //                	add x0, x8, x9; (374..391)
    //                	ldp x29, x30, [sp, #32]; (359..392)
    //                	add sp, sp, #48; (359..392)
    //                	ret x30; (374..391)"#]],
    //        );
    //    }
    //}

    //mod ifs {
    //    use super::*;

    //    #[test]
    //    fn simple() {
    //        check_intrinsics_expander(
    //            "(: choose (-> boolean usize usize usize))
    //             (define (choose cond true false)
    //               (if cond true false))",
    //            expect![[r#"
    //                .align 4
    //                .global _T4root6choose
    //                _T4root6choose:
    //                	sub sp, sp, #48; (101..174)
    //                	stp x29, x30, [sp, #32]; (101..174)
    //                	add x29, sp, #32; (101..174)
    //                	strb w0, [sp, #31]; (101..174)
    //                	str x1, [sp, #16]; (101..174)
    //                	str x2, [sp, #8]; (101..174)
    //                0:
    //                	ldrb w8, [sp, #31]; (157..161)
    //                	cmp w8, #0; (157..161)
    //                	b.NE 1f; (157..161)
    //                	b 2f; (157..161)
    //                1:
    //                	ldr x0, [sp, #16]; (162..166)
    //                	b 3f; (157..161)
    //                2:
    //                	ldr x0, [sp, #8]; (167..172)
    //                	b 3f; (157..161)
    //                3:
    //                	ldp x29, x30, [sp, #32]; (101..174)
    //                	add sp, sp, #48; (101..174)
    //                	ret x30; (153..173)"#]],
    //        );
    //    }
    //}
}
