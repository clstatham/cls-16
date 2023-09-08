use std::collections::VecDeque;

use anyhow::{Error, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    asm::{lexer::lex_program, Mnemonic, Span},
    plat::{Immediate, InstrFormat, Instruction, Opcode, PlatformError},
};

use super::{AsmError, Token, WithSpan};

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
            t
        };
        if let Token::Mnemonic(op) = take1().item {
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
                    Opcode::Add | Opcode::And | Opcode::Or | Opcode::Sub | Opcode::Xor => {
                        let a = take1();
                        let b = take1();
                        let c = take1();
                        if let Token::Register(a) = a.item {
                            if let Token::Register(b) = b.item {
                                if let Token::Register(c) = c.item {
                                    out.push(Instruction {
                                        op,
                                        format: InstrFormat::RRR(a, b, c),
                                    });
                                } else {
                                    return Err(Error::from(AsmError::InvalidInstruction {
                                        loc: c.span.location_line() as usize,
                                    }));
                                }
                            } else {
                                return Err(Error::from(AsmError::InvalidInstruction {
                                    loc: b.span.location_line() as usize,
                                }));
                            }
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction {
                                loc: a.span.location_line() as usize,
                            }));
                        }
                    }
                    // RR instructions
                    Opcode::Ldh
                    | Opcode::Ldl
                    | Opcode::Sth
                    | Opcode::Stl
                    | Opcode::Not
                    | Opcode::Shl
                    | Opcode::Shr => {
                        let a = take1();
                        let b = take1();
                        if let Token::Register(a) = a.item {
                            if let Token::Register(b) = b.item {
                                out.push(Instruction {
                                    op,
                                    format: InstrFormat::RR(a, b),
                                });
                            } else {
                                return Err(Error::from(AsmError::InvalidInstruction {
                                    loc: b.span.location_line() as usize,
                                }));
                            }
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction {
                                loc: a.span.location_line() as usize,
                            }));
                        }
                    }
                    // RI instructions
                    Opcode::Ldi => {
                        // LDI is special, because it can be used to load labels (immediate addresses) into a register
                        let a = take1();
                        let imm = take1();
                        if let Token::Register(a) = a.item {
                            if let Token::Immediate(imm) = imm.item {
                                out.push(Instruction {
                                    op,
                                    format: InstrFormat::RI(a, Immediate::Linked(imm)),
                                });
                            } else if let Token::Label(name) = imm.item {
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
                                return Err(Error::from(AsmError::InvalidInstruction {
                                    loc: imm.span.location_line() as usize,
                                }));
                            }
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction {
                                loc: a.span.location_line() as usize,
                            }));
                        }
                    }
                    // R instructions
                    Opcode::Jmp | Opcode::Jz | Opcode::Printc | Opcode::Printi => {
                        let a = take1();
                        if let Token::Register(a) = a.item {
                            out.push(Instruction {
                                op,
                                format: InstrFormat::R(a),
                            });
                        } else {
                            return Err(Error::from(AsmError::InvalidInstruction {
                                loc: a.span.location_line() as usize,
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
        let mut labels = FxHashMap::default();
        let mut undefined_references = FxHashSet::default();

        let prog_span = Span::new_extra(program_text, program_text);
        let tokens = lex_program(&prog_span)?;
        let mut tokens = &tokens[..];

        // loop over the tokens, converting them into instructions
        while let Some(next_tok) = tokens.first() {
            tokens = &tokens[1..];
            match next_tok.item {
                Token::Eof => break,
                Token::Label(name) => {
                    undefined_references.remove(name);
                    let mut lab = Label {
                        name,
                        link_status: Immediate::Unlinked(name),
                        instructions: vec![],
                    };

                    loop {
                        let (rest, maybe_instrs, mut discovered_labels) =
                            Instruction::from_tokens(tokens, labels.clone())?;

                        for discovered_name in discovered_labels.drain() {
                            if !labels.contains_key(discovered_name) {
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
                    let old = labels.insert(name, lab);
                    if let Some(old) = old {
                        return Err(Error::from(AsmError::DuplicateLabel(
                            next_tok.span.location_line() as usize,
                            old.name.to_owned(),
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
                    if let InstrFormat::RI(_, Immediate::Unlinked(undefined)) = instr.format {
                        undefined_locations.insert(pc, undefined.to_string());
                    }

                    let bytes = instr.to_bytes()?;
                    out.extend_from_slice(&bytes);
                    pc = out.len() as u16;
                }
                Ok::<(), Error>(())
            };

        // codegen the "start" label first
        let mut start_label =
            labels
                .remove("start")
                .ok_or(Error::from(AsmError::UndefinedReferences(vec![
                    "start".to_owned()
                ])))?;
        codegen_label(&mut start_label, &mut undefined_locations)?;

        for (_label_name, label) in labels.iter_mut() {
            codegen_label(label, &mut undefined_locations)?;
        }
        // reinsert for linking
        labels.insert("start", start_label);

        let mut iters = 0;
        loop {
            let undefined_locations_clone = undefined_locations.clone();
            for (loc, undefined) in undefined_locations_clone.iter() {
                if let Some(lab) = labels.get(undefined.as_str()) {
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
