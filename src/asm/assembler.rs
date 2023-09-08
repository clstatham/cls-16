use anyhow::{Error, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    asm::{lexer::lex_program, Mnemonic, Span},
    plat::{Immediate, InstrFormat, Instruction, Opcode, PlatformError, Register},
};

use super::{AsmError, Compound, Token, WithSpan};

/// A basic block of code in an assembly listing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label<'a> {
    pub name: &'a str,
    pub link_status: Immediate<'a>,
    pub instructions: Vec<Instruction<'a>>,
}

impl<'a> Label<'a> {
    pub fn size_in_bytes(&self) -> usize {
        self.instructions.len() * 4
    }
}

impl<'a, 'b> Instruction<'a> {
    #[allow(clippy::type_complexity)]
    pub(crate) fn from_tokens(
        mut toks: &'b [WithSpan<'a, Token<'a>>],
        known_labels: FxHashMap<&'a str, Label<'a>>,
    ) -> Result<(
        &'b [WithSpan<'a, Token<'a>>],
        Option<Vec<Self>>,
        FxHashSet<&'a str>,
    )> {
        let mut out = vec![];
        let mut discovered_labels = FxHashSet::default();
        let mut take1 = || {
            let t = toks[0];
            toks = &toks[1..];
            t.item
        };
        if let Token::Mnemonic(op) = take1() {
            match op {
                Mnemonic::Regular(op) => match op {
                    // OpOnly instructions
                    Opcode::Halt | Opcode::Nop => {
                        out.push(Instruction {
                            op,
                            format: InstrFormat::OpOnly,
                        });
                    }
                    // RRR instructions
                    Opcode::Add
                    | Opcode::And
                    | Opcode::Or
                    | Opcode::Shl
                    | Opcode::Shr
                    | Opcode::Sub
                    | Opcode::Xor => {
                        let a = take1();
                        let b = take1();
                        let c = take1();
                        if let Token::Register(a) = a {
                            if let Token::Register(b) = b {
                                if let Token::Register(c) = c {
                                    out.push(Instruction {
                                        op,
                                        format: InstrFormat::RRR(a, b, c),
                                    });
                                } else {
                                    return Err(Error::from(AsmError::InvalidInstruction));
                                }
                            } else {
                                return Err(Error::from(AsmError::InvalidInstruction));
                            }
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction));
                        }
                    }
                    // RR instructions
                    Opcode::Ldh | Opcode::Ldl | Opcode::Sth | Opcode::Stl | Opcode::Not => {
                        let a = take1();
                        let b = take1();
                        if let Token::Register(a) = a {
                            if let Token::Register(b) = b {
                                out.push(Instruction {
                                    op,
                                    format: InstrFormat::RR(a, b),
                                });
                            } else {
                                return Err(Error::from(AsmError::InvalidInstruction));
                            }
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction));
                        }
                    }
                    // RI instructions
                    Opcode::Ldi => {
                        // LDI is special, because it can be used to load labels (immediate addresses) into a register
                        let a = take1();
                        let imm = take1();
                        if let Token::Register(a) = a {
                            if let Token::Immediate(imm) = imm {
                                out.push(Instruction {
                                    op,
                                    format: InstrFormat::RI(a, Immediate::Linked(imm)),
                                });
                            } else if let Token::Label(name) = imm {
                                if let Some(label) = known_labels.get(name) {
                                    out.push(Instruction {
                                        op,
                                        format: InstrFormat::RI(a, label.link_status),
                                    });
                                } else {
                                    out.push(Instruction {
                                        op,
                                        format: InstrFormat::RI(a, Immediate::Unlinked(name)),
                                    });
                                    discovered_labels.insert(name);
                                }
                            } else {
                                return Err(Error::from(AsmError::InvalidInstruction));
                            }
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction));
                        }
                    }
                    Opcode::Addi | Opcode::Subi => {
                        let a = take1();
                        let imm = take1();
                        if let Token::Register(a) = a {
                            if let Token::Immediate(imm) = imm {
                                out.push(Instruction {
                                    op,
                                    format: InstrFormat::RI(a, Immediate::Linked(imm)),
                                });
                            } else {
                                return Err(Error::from(AsmError::InvalidInstruction));
                            }
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction));
                        }
                    }
                    // R instructions
                    Opcode::Jz | Opcode::Printc | Opcode::Printi => {
                        let a = take1();
                        if let Token::Register(a) = a {
                            out.push(Instruction {
                                op,
                                format: InstrFormat::R(a),
                            });
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction));
                        }
                    }
                },
                Mnemonic::Compound(op) => match op {
                    Compound::Push => {
                        let a = take1();
                        if let Token::Register(a) = a {
                            // stl     sp regA
                            // subi    sp $1
                            // sth     sp regA
                            // subi    sp $1
                            out.push(Instruction {
                                op: Opcode::Stl,
                                format: InstrFormat::RR(Register::SP, a),
                            });
                            out.push(Instruction {
                                op: Opcode::Subi,
                                format: InstrFormat::RI(Register::SP, Immediate::Linked(1)),
                            });
                            out.push(Instruction {
                                op: Opcode::Sth,
                                format: InstrFormat::RR(Register::SP, a),
                            });
                            out.push(Instruction {
                                op: Opcode::Subi,
                                format: InstrFormat::RI(Register::SP, Immediate::Linked(1)),
                            });
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction));
                        }
                    }
                    Compound::Pop => {
                        let a = take1();
                        if let Token::Register(a) = a {
                            // addi    sp $1
                            // ldh     regA sp
                            // addi    sp $1
                            // ldl     regA sp
                            out.push(Instruction {
                                op: Opcode::Addi,
                                format: InstrFormat::RI(Register::SP, Immediate::Linked(1)),
                            });
                            out.push(Instruction {
                                op: Opcode::Ldh,
                                format: InstrFormat::RR(a, Register::SP),
                            });
                            out.push(Instruction {
                                op: Opcode::Addi,
                                format: InstrFormat::RI(Register::SP, Immediate::Linked(1)),
                            });
                            out.push(Instruction {
                                op: Opcode::Ldl,
                                format: InstrFormat::RR(a, Register::SP),
                            });
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction));
                        }
                    }
                },
            }
            Ok((toks, Some(out), discovered_labels))
        } else {
            Ok((toks, None, discovered_labels))
        }
    }
}

/// An assembly context capable of translating a source file into binary / machine code.
#[derive(Debug, Default)]
pub struct Assembler<'a> {
    labels: FxHashMap<&'a str, Label<'a>>,
    undefined_references: FxHashSet<String>,
}

impl<'a> Assembler<'a> {
    /// Generates binary / machine code from assembly source code.
    ///
    /// # Errors
    ///
    /// This function will return an error if the provided assembly source is not valid syntax, or if there is an error while linking.
    pub fn assemble(&mut self, program_text: &'a str) -> Result<Box<[u8]>> {
        let prog_span = Span::new_extra(program_text, program_text);
        let tokens = lex_program(&prog_span)?;
        let mut tokens = &tokens[..];

        // loop over the tokens, converting them into instructions
        while let Some(next_tok) = tokens.first() {
            match next_tok.item {
                Token::Eof => break,
                Token::Label(name) => {
                    self.undefined_references.remove(name);
                    let mut lab = Label {
                        name,
                        link_status: Immediate::Unlinked(name),
                        instructions: vec![],
                    };

                    tokens = &tokens[1..];
                    loop {
                        let (rest, maybe_instrs, mut discovered_labels) =
                            Instruction::from_tokens(tokens, self.labels.clone())?;

                        for discovered_name in discovered_labels.drain() {
                            if !self.labels.contains_key(discovered_name) {
                                self.undefined_references.insert(discovered_name.to_owned());
                            }
                        }
                        if let Some(instrs) = maybe_instrs {
                            tokens = rest;
                            lab.instructions.extend_from_slice(&instrs);
                        } else {
                            break;
                        }
                    }
                    let old = self.labels.insert(name, lab);
                    if let Some(old) = old {
                        return Err(Error::from(AsmError::DuplicateLabel(old.name.to_owned())));
                    }
                }
                // todo: directives
                _tok => {
                    return Err(Error::from(AsmError::UnexpectedToken(
                        tokens[0].span.fragment().trim().to_string(),
                    )))
                }
            }
        }

        // build the binary from the parsed instructions
        let mut out = vec![0u8, 0u8]; // null word for null-pointer catching
        let mut iters = 0;
        'link: loop {
            let labels_clone = self.labels.clone();
            for (_label_name, label) in self.labels.iter_mut() {
                let mut tmp_out = vec![];
                // check if we've resolved any references in this label's code yet
                for instr in label.instructions.iter_mut() {
                    if let InstrFormat::RI(reg, Immediate::Unlinked(undefined)) = instr.format {
                        if let Some(maybe_linked_lab) = labels_clone.get(undefined) {
                            if let Immediate::Linked(defined) = maybe_linked_lab.link_status {
                                // we've resolved the reference! replace it with the linked version
                                instr.format = InstrFormat::RI(reg, Immediate::Linked(defined));
                                self.undefined_references.remove(undefined);
                            }
                        }
                    }

                    match instr.to_bytes() {
                        Ok(bytes) => {
                            tmp_out.extend_from_slice(&bytes);
                        }
                        Err(e) => match e.downcast::<PlatformError>()? {
                            PlatformError::UndefinedReference(_) => {
                                break; // stop generating machine code for this label, because it has undefined references
                            }
                            e => return Err(Error::from(e)),
                        },
                    }
                }
                // we all good?
                if self.undefined_references.get(label.name).is_none() {
                    let pc = out.len() as u16;
                    label.link_status = Immediate::Linked(pc);
                    out.extend_from_slice(&tmp_out);
                }
            }

            iters += 1;
            if iters > 1024 {
                // prevent infinite loop
                if self.undefined_references.is_empty() {
                    return Err(Error::from(AsmError::MaxLinkerIters));
                } else {
                    return Err(Error::from(AsmError::UndefinedReferences(Vec::from_iter(
                        self.undefined_references.iter().map(|s| s.to_string()),
                    ))));
                }
            }

            for (_label_name, label) in self.labels.iter() {
                if let Immediate::Unlinked(_) = label.link_status {
                    continue 'link;
                }
                for instr in label.instructions.iter() {
                    if let InstrFormat::RI(_, Immediate::Unlinked(_)) = instr.format {
                        continue 'link;
                    }
                }
            }
            break;
        }
        dbg!(&self.labels);

        Ok(out.into_boxed_slice())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_assemble() {
        let program = "
%start
    ; comment
    ldi     r1 %startthree
    printi  r1
    halt
%startthree
    nop
";
        let mut asm = Assembler::default();
        let bin = asm.assemble(program).unwrap();
        dbg!(bin);
    }
}
