use std::collections::BTreeMap;

use anyhow::{Error, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    asm::{lexer::lex_program, Mnemonic, Span},
    plat::{Immediate, InstrFormat, Instruction, Opcode, Register},
};

use super::{AsmError, AsmToken, Compound, WithSpan};

/// A basic block of code in an assembly listing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label<'a> {
    pub order: usize,
    pub name: &'a str,
    pub link_status: Immediate,
    pub instructions: Vec<Instruction>,
}

impl<'a> Label<'a> {
    pub fn size_in_bytes(&self) -> usize {
        self.instructions.len() * 4
    }
}

impl<'a, 'b> Instruction {
    #[allow(clippy::type_complexity)]
    pub(crate) fn from_tokens(
        mut toks: &'b [WithSpan<'a, AsmToken<'a>>],
        label_names: FxHashMap<&'a str, usize>,
        known_labels: BTreeMap<usize, Label<'a>>,
    ) -> Result<(
        &'b [WithSpan<'a, AsmToken<'a>>],
        Option<Vec<Self>>,
        FxHashSet<&'a str>,
    )> {
        let mut out = vec![];
        let mut discovered_labels = FxHashSet::default();
        macro_rules! take1 {
            () => {{
                let t = toks[0];
                toks = &toks[1..];
                t
            }};
        }
        if let AsmToken::Mnemonic(op) = take1!().item {
            match op {
                Mnemonic::Regular(op) => match op {
                    // OpOnly instructions
                    Opcode::Halt | Opcode::Nop | Opcode::B => {
                        out.push(Instruction {
                            op,
                            format: InstrFormat::OpOnly,
                        });
                    }
                    // RRR | RRI instructions
                    Opcode::Add
                    | Opcode::And
                    | Opcode::Or
                    | Opcode::Sub
                    | Opcode::Xor
                    | Opcode::Div
                    | Opcode::Mul => {
                        let a = take1!();
                        if let AsmToken::Register(a) = a.item {
                            let b = take1!();
                            if let AsmToken::Register(b) = b.item {
                                let c = take1!();
                                if let AsmToken::Register(c) = c.item {
                                    out.push(Instruction {
                                        op,
                                        format: InstrFormat::RRR(a, b, c),
                                    });
                                } else if let AsmToken::Immediate(imm) = c.item {
                                    out.push(Instruction {
                                        op,
                                        format: InstrFormat::RRI(a, b, Immediate::Linked(imm)),
                                    });
                                } else if let AsmToken::Label(name) = c.item {
                                    if let Some(label) =
                                        label_names.get(name).and_then(|l| known_labels.get(l))
                                    {
                                        out.push(Instruction {
                                            op,
                                            format: InstrFormat::RRI(
                                                a,
                                                b,
                                                label.link_status.clone(),
                                            ),
                                        });
                                    } else {
                                        out.push(Instruction {
                                            op,
                                            format: InstrFormat::RRI(
                                                a,
                                                b,
                                                Immediate::Unlinked(name.to_owned()),
                                            ),
                                        });
                                        discovered_labels.insert(name);
                                    }
                                } else {
                                    return Err(Error::from(AsmError::InvalidInstruction {
                                        loc: c.span.location_line() as usize,
                                        span: c.span.fragment().lines().next().unwrap().to_string(),
                                    }));
                                }
                            } else {
                                return Err(Error::from(AsmError::InvalidInstruction {
                                    loc: b.span.location_line() as usize,
                                    span: b.span.fragment().lines().next().unwrap().to_string(),
                                }));
                            }
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction {
                                loc: a.span.location_line() as usize,
                                span: a.span.fragment().lines().next().unwrap().to_string(),
                            }));
                        }
                    }
                    // RR | RI instructions
                    Opcode::Mov => {
                        let a = take1!();
                        if let AsmToken::Register(a) = a.item {
                            let b = take1!();
                            match b.item {
                                AsmToken::Register(b) => out.push(Instruction {
                                    op,
                                    format: InstrFormat::RR(a, b),
                                }),
                                AsmToken::Immediate(imm) => out.push(Instruction {
                                    op,
                                    format: InstrFormat::RI(a, Immediate::Linked(imm)),
                                }),
                                AsmToken::Label(name) => {
                                    if let Some(label) =
                                        label_names.get(name).and_then(|l| known_labels.get(l))
                                    {
                                        out.push(Instruction {
                                            op,
                                            format: InstrFormat::RI(a, label.link_status.clone()),
                                        });
                                    } else {
                                        out.push(Instruction {
                                            op,
                                            format: InstrFormat::RI(
                                                a,
                                                Immediate::Unlinked(name.to_owned()),
                                            ),
                                        });
                                        discovered_labels.insert(name);
                                    }
                                }
                                _ => {
                                    return Err(Error::from(AsmError::InvalidInstruction {
                                        loc: b.span.location_line() as usize,
                                        span: b.span.fragment().lines().next().unwrap().to_string(),
                                    }));
                                }
                            }
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction {
                                loc: a.span.location_line() as usize,
                                span: a.span.fragment().lines().next().unwrap().to_string(),
                            }));
                        }
                    }
                    // RR | RRI instructions
                    Opcode::Ldh | Opcode::Ldl | Opcode::Sth | Opcode::Stl => {
                        let a = take1!();
                        let b = take1!();
                        if let AsmToken::Register(a) = a.item {
                            if let AsmToken::Register(b) = b.item {
                                let peek = toks[0];
                                if let AsmToken::Immediate(imm) = peek.item {
                                    take1!();
                                    out.push(Instruction {
                                        op,
                                        format: InstrFormat::RRI(a, b, Immediate::Linked(imm)),
                                    });
                                } else {
                                    out.push(Instruction {
                                        op,
                                        format: InstrFormat::RR(a, b),
                                    });
                                }
                            } else {
                                return Err(Error::from(AsmError::InvalidInstruction {
                                    loc: b.span.location_line() as usize,
                                    span: b.span.fragment().lines().next().unwrap().to_string(),
                                }));
                            }
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction {
                                loc: a.span.location_line() as usize,
                                span: a.span.fragment().lines().next().unwrap().to_string(),
                            }));
                        }
                    }
                    // R instructions
                    Opcode::Not | Opcode::Shl | Opcode::Shr => {
                        let a = take1!();
                        if let AsmToken::Register(a) = a.item {
                            out.push(Instruction {
                                op,
                                format: InstrFormat::R(a),
                            });
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction {
                                loc: a.span.location_line() as usize,
                                span: a.span.fragment().lines().next().unwrap().to_string(),
                            }));
                        }
                    }
                    // R | I instructions
                    Opcode::Jmp | Opcode::Jz | Opcode::Jc | Opcode::Printc | Opcode::Printi => {
                        let a = take1!();
                        if let AsmToken::Register(a) = a.item {
                            out.push(Instruction {
                                op,
                                format: InstrFormat::R(a),
                            });
                        } else if let AsmToken::Immediate(imm) = a.item {
                            out.push(Instruction {
                                op,
                                format: InstrFormat::I(Immediate::Linked(imm)),
                            });
                        } else if let AsmToken::Label(name) = a.item {
                            if let Some(label) =
                                label_names.get(name).and_then(|l| known_labels.get(l))
                            {
                                out.push(Instruction {
                                    op,
                                    format: InstrFormat::I(label.link_status.clone()),
                                });
                            } else {
                                out.push(Instruction {
                                    op,
                                    format: InstrFormat::I(Immediate::Unlinked(name.to_owned())),
                                });
                                discovered_labels.insert(name);
                            }
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction {
                                loc: a.span.location_line() as usize,
                                span: a.span.fragment().lines().next().unwrap().to_string(),
                            }));
                        }
                    }
                },
                Mnemonic::Compound(op) => match op {
                    Compound::Push => {
                        // stl     sp regA
                        // sub     sp $1
                        // sth     sp regA
                        // sub     sp $1
                        let a = take1!();
                        if let AsmToken::Register(a) = a.item {
                            out.push(Instruction {
                                op: Opcode::Stl,
                                format: InstrFormat::RR(Register::SP, a),
                            });
                            out.push(Instruction {
                                op: Opcode::Sub,
                                format: InstrFormat::RRI(
                                    Register::SP,
                                    Register::SP,
                                    Immediate::Linked(1),
                                ),
                            });
                            out.push(Instruction {
                                op: Opcode::Sth,
                                format: InstrFormat::RR(Register::SP, a),
                            });
                            out.push(Instruction {
                                op: Opcode::Sub,
                                format: InstrFormat::RRI(
                                    Register::SP,
                                    Register::SP,
                                    Immediate::Linked(1),
                                ),
                            });
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction {
                                loc: a.span.location_line() as usize,
                                span: a.span.fragment().lines().next().unwrap().to_string(),
                            }));
                        }
                    }
                    Compound::Pop => {
                        // add     sp $1
                        // ldh     regA sp
                        // add     sp $1
                        // ldl     regA sp
                        let a = take1!();
                        if let AsmToken::Register(a) = a.item {
                            out.push(Instruction {
                                op: Opcode::Add,
                                format: InstrFormat::RRI(
                                    Register::SP,
                                    Register::SP,
                                    Immediate::Linked(1),
                                ),
                            });
                            out.push(Instruction {
                                op: Opcode::Ldh,
                                format: InstrFormat::RR(a, Register::SP),
                            });
                            out.push(Instruction {
                                op: Opcode::Add,
                                format: InstrFormat::RRI(
                                    Register::SP,
                                    Register::SP,
                                    Immediate::Linked(1),
                                ),
                            });
                            out.push(Instruction {
                                op: Opcode::Ldl,
                                format: InstrFormat::RR(a, Register::SP),
                            });
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction {
                                loc: a.span.location_line() as usize,
                                span: a.span.fragment().lines().next().unwrap().to_string(),
                            }));
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
pub struct Assembler;

impl Assembler {
    /// Generates binary / machine code from assembly source code.
    ///
    /// # Errors
    ///
    /// This function will return an error if the provided assembly source is not valid syntax, or if there is an error while linking.
    pub fn assemble(&mut self, program_text: &str) -> Result<Box<[u8]>> {
        let mut labels = BTreeMap::new();
        let mut label_names = FxHashMap::default();
        let mut undefined_references = FxHashSet::default();

        let prog_span = Span::new_extra(program_text, program_text);
        let tokens = lex_program(&prog_span)?;
        let mut tokens = &tokens[..];

        // loop over the tokens, converting them into instructions
        while let Some(next_tok) = tokens.first() {
            tokens = &tokens[1..];
            match next_tok.item {
                AsmToken::Eof => break,
                AsmToken::Label(name) => {
                    undefined_references.remove(name);
                    let mut lab = Label {
                        order: labels.len(),
                        name,
                        link_status: Immediate::Unlinked(name.to_owned()),
                        instructions: vec![],
                    };

                    loop {
                        let (rest, maybe_instrs, mut discovered_labels) =
                            Instruction::from_tokens(tokens, label_names.clone(), labels.clone())?;

                        for discovered_name in discovered_labels.drain() {
                            if !label_names.contains_key(discovered_name) {
                                undefined_references.insert(discovered_name.to_owned());
                            }
                        }
                        if let Some(instrs) = maybe_instrs {
                            tokens = rest;
                            lab.instructions.extend_from_slice(&instrs);
                        } else {
                            break;
                        }
                    }
                    let old = label_names.insert(lab.name, lab.order);
                    labels.insert(lab.order, lab);
                    if old.is_some() {
                        return Err(Error::from(AsmError::DuplicateLabel(
                            next_tok.span.location_line() as usize,
                            name.to_owned(),
                        )));
                    }
                }
                // todo: directives
                _tok => {
                    return Err(Error::from(AsmError::UnexpectedToken(
                        next_tok.span.location_line() as usize,
                        next_tok
                            .span
                            .fragment()
                            .split_whitespace()
                            .next()
                            .unwrap()
                            .to_string(),
                    )))
                }
            }
        }

        // build the binary from the parsed instructions
        let mut undefined_locations = FxHashMap::default();
        let mut out = vec![0u8, 0u8, 0u8, 0u8]; // null instruction word for null-pointer catching
        let mut pc = out.len() as u16;

        let mut codegen_label =
            |label: &mut Label<'_>, undefined_locations: &mut FxHashMap<u16, String>| {
                label.link_status = Immediate::Linked(pc);
                // check if we've resolved any references in this label's code yet
                for instr in label.instructions.iter_mut() {
                    if let InstrFormat::RRI(_, _, Immediate::Unlinked(undefined)) = &instr.format {
                        undefined_locations.insert(pc, undefined.to_string());
                    } else if let InstrFormat::RI(_, Immediate::Unlinked(undefined)) = &instr.format
                    {
                        undefined_locations.insert(pc, undefined.to_string());
                    } else if let InstrFormat::I(Immediate::Unlinked(undefined)) = &instr.format {
                        undefined_locations.insert(pc, undefined.to_string());
                    }

                    let bytes = instr.to_bytes()?;
                    out.extend_from_slice(&bytes);
                    pc = out.len() as u16;
                }
                Ok::<(), Error>(())
            };

        // codegen the "start" label first
        let mut start_label = label_names
            .get("start")
            .and_then(|l| labels.remove(l))
            .ok_or(Error::from(AsmError::UndefinedReferences(vec![
                "start".to_owned()
            ])))?;
        codegen_label(&mut start_label, &mut undefined_locations)?;

        for (_label_name, label) in labels.iter_mut() {
            codegen_label(label, &mut undefined_locations)?;
        }
        // reinsert for linking
        labels.insert(*label_names.get("start").unwrap(), start_label);

        let mut iters = 0;
        loop {
            let undefined_locations_clone = undefined_locations.clone();
            for (loc, undefined) in undefined_locations_clone.iter() {
                if let Some(lab) = label_names
                    .get(undefined.as_str())
                    .and_then(|l| labels.get(l))
                {
                    if let Immediate::Linked(linked_loc) = lab.link_status {
                        out[*loc as usize + 2..*loc as usize + 4]
                            .copy_from_slice(&linked_loc.to_le_bytes());
                        undefined_locations.remove(loc);
                    } else {
                        return Err(Error::from(AsmError::UndefinedReferences(vec![
                            undefined.to_owned()
                        ])));
                    }
                } else {
                    return Err(Error::from(AsmError::UndefinedReferences(vec![
                        undefined.to_owned()
                    ])));
                }
            }
            if undefined_locations.is_empty() {
                break;
            }
            iters += 1;
            if iters > 1024 {
                // prevent infinite loop
                if undefined_locations.is_empty() {
                    return Err(Error::from(AsmError::MaxLinkerIters));
                } else {
                    return Err(Error::from(AsmError::UndefinedReferences(Vec::from_iter(
                        undefined_locations.values().map(|s| s.to_string()),
                    ))));
                }
            }
        }

        Ok(out.into_boxed_slice())
    }
}
