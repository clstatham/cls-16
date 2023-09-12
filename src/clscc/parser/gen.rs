use crate::clscc::common::*;
use crate::clscc::parser::{Ast, AstNode, Tokens};
use nom::bytes::complete::*;
use nom::combinator::*;
use nom::IResult;

macro_rules! tag_token {
    ($func:ident, $typ:ident, $tok:ident) => {
        pub fn $func(inp: Tokens<'_>) -> IResult<Tokens<'_>, AstNode<'_>> {
            let (rest, t) = verify(take(1usize), |t: &Tokens<'_>| {
                t.peek()
                    .map(|t| t.variant == TokenVariant::$typ($typ::$tok))
                    .unwrap_or(false)
            })(inp)?;
            Ok((rest, AstNode::new(Ast::Token(t.tok[0].clone()), None)))
        }
    };
}

tag_token! { punc_obrack, Punctuator, OBrack }
tag_token! { punc_cbrack, Punctuator, CBrack }
tag_token! { punc_oparen, Punctuator, OParen }
tag_token! { punc_cparen, Punctuator, CParen }
tag_token! { punc_obrace, Punctuator, OBrace }
tag_token! { punc_cbrace, Punctuator, CBrace }
tag_token! { punc_period, Punctuator, Period }
tag_token! { punc_rarrow, Punctuator, RArrow }
tag_token! { punc_plusplus, Punctuator, PlusPlus }
tag_token! { punc_minusminus, Punctuator, MinusMinus }
tag_token! { punc_ampersand, Punctuator, Ampersand }
tag_token! { punc_star, Punctuator, Star }
tag_token! { punc_plus, Punctuator, Plus }
tag_token! { punc_minus, Punctuator, Minus }
tag_token! { punc_tilde, Punctuator, Tilde }
tag_token! { punc_bang, Punctuator, Bang }
tag_token! { punc_fslash, Punctuator, FSlash }
tag_token! { punc_percent, Punctuator, Percent }
tag_token! { punc_ltlt, Punctuator, LtLt }
tag_token! { punc_gtgt, Punctuator, GtGt }
tag_token! { punc_lt, Punctuator, Lt }
tag_token! { punc_gt, Punctuator, Gt }
tag_token! { punc_lteq, Punctuator, LtEq }
tag_token! { punc_gteq, Punctuator, GtEq }
tag_token! { punc_eqeq, Punctuator, EqEq }
tag_token! { punc_bangeq, Punctuator, BangEq }
tag_token! { punc_caret, Punctuator, Caret }
tag_token! { punc_bar, Punctuator, Bar }
tag_token! { punc_andand, Punctuator, AndAnd }
tag_token! { punc_barbar, Punctuator, BarBar }
tag_token! { punc_question, Punctuator, Question }
tag_token! { punc_colon, Punctuator, Colon }
tag_token! { punc_semicolon, Punctuator, Semicolon }
tag_token! { punc_ellipsis, Punctuator, Ellipsis }
tag_token! { punc_equals, Punctuator, Equals }
tag_token! { punc_stareq, Punctuator, StarEq }
tag_token! { punc_slasheq, Punctuator, SlashEq }
tag_token! { punc_percenteq, Punctuator, PercentEq }
tag_token! { punc_pluseq, Punctuator, PlusEq }
tag_token! { punc_minuseq, Punctuator, MinusEq }
tag_token! { punc_ltlteq, Punctuator, LtLtEq }
tag_token! { punc_gtgteq, Punctuator, GtGtEq }
tag_token! { punc_andeq, Punctuator, AndEq }
tag_token! { punc_careteq, Punctuator, CaretEq }
tag_token! { punc_bareq, Punctuator, BarEq }
tag_token! { punc_comma, Punctuator, Comma }
tag_token! { punc_hash, Punctuator, Hash }
tag_token! { punc_hashhash, Punctuator, HashHash }

tag_token! { kw_auto, Keyword, Auto }
tag_token! { kw_break, Keyword, Break }
tag_token! { kw_case, Keyword, Case }
tag_token! { kw_char, Keyword, Char }
tag_token! { kw_const, Keyword, Const }
tag_token! { kw_continue, Keyword, Continue }
tag_token! { kw_default, Keyword, Default }
tag_token! { kw_do, Keyword, Do }
tag_token! { kw_double, Keyword, Double }
tag_token! { kw_else, Keyword, Else }
tag_token! { kw_enum, Keyword, Enum }
tag_token! { kw_extern, Keyword, Extern }
tag_token! { kw_float, Keyword, Float }
tag_token! { kw_for, Keyword, For }
tag_token! { kw_goto, Keyword, Goto }
tag_token! { kw_if, Keyword, If }
tag_token! { kw_inline, Keyword, Inline }
tag_token! { kw_int, Keyword, Int }
tag_token! { kw_long, Keyword, Long }
tag_token! { kw_register, Keyword, Register }
tag_token! { kw_restrict, Keyword, Restrict }
tag_token! { kw_return, Keyword, Return }
tag_token! { kw_short, Keyword, Short }
tag_token! { kw_signed, Keyword, Signed }
tag_token! { kw_sizeof, Keyword, Sizeof }
tag_token! { kw_static, Keyword, Static }
tag_token! { kw_struct, Keyword, Struct }
tag_token! { kw_switch, Keyword, Switch }
tag_token! { kw_typedef, Keyword, Typedef }
tag_token! { kw_union, Keyword, Union }
tag_token! { kw_unsigned, Keyword, Unsigned }
tag_token! { kw_void, Keyword, Void }
tag_token! { kw_volatile, Keyword, Volatile }
tag_token! { kw_while, Keyword, While }
