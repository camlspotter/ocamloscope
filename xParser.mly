/* CR jfuruse: This file is already diverted from the original enough.
  We need clean-up 
*/

/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* The parser definition */

%{
open Location
open Asttypes
open Longident
open Parsetree

let mktyp d =
  { ptyp_desc = d; ptyp_loc = symbol_rloc() }
let mkpat d =
  { ppat_desc = d; ppat_loc = symbol_rloc() }
let mkmty d =
  { pmty_desc = d; pmty_loc = symbol_rloc() }
let mksig d =
  { psig_desc = d; psig_loc = symbol_rloc() }
let mkfield d =
  { pfield_desc = d; pfield_loc = symbol_rloc() }
let mkcty d =
  { pcty_desc = d; pcty_loc = symbol_rloc() }
let mkctf d =
  { pctf_desc = d; pctf_loc = symbol_rloc () }
let mkrhs rhs pos = mkloc rhs (rhs_loc pos)
let mkoption d =
  let loc = {d.ptyp_loc with loc_ghost = true} in
  { ptyp_desc = Ptyp_constr(mkloc (Ldot (Lident "*predef*", "option")) loc,[d]);
    ptyp_loc = loc}

let reloc_pat x = { x with ppat_loc = symbol_rloc () };;

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghpat d = { ppat_desc = d; ppat_loc = symbol_gloc () };;
let ghtyp d = { ptyp_desc = d; ptyp_loc = symbol_gloc () };;

let mkpat_cons consloc args loc =
  {ppat_desc = Ppat_construct(mkloc (Lident "::") consloc, Some args, false);
   ppat_loc = loc}

let rec mktailpat nilloc = function
    [] ->
      let loc = { nilloc with loc_ghost = true } in
      let nil = { txt = Lident "[]"; loc = loc } in
      { ppat_desc = Ppat_construct (nil, None, false); ppat_loc = loc }
  | p1 :: pl ->
      let pat_pl = mktailpat nilloc pl in
      let l = {loc_start = p1.ppat_loc.loc_start;
               loc_end = pat_pl.ppat_loc.loc_end;
               loc_ghost = true}
      in
      let arg = {ppat_desc = Ppat_tuple [p1; pat_pl]; ppat_loc = l} in
      mkpat_cons {l with loc_ghost = true} arg l

let rec deep_mkrangepat c1 c2 =
  if c1 = c2 then ghpat(Ppat_constant(Const_char c1)) else
  ghpat(Ppat_or(ghpat(Ppat_constant(Const_char c1)),
                deep_mkrangepat (Char.chr(Char.code c1 + 1)) c2))

let rec mkrangepat c1 c2 =
  if c1 > c2 then mkrangepat c2 c1 else
  if c1 = c2 then mkpat(Ppat_constant(Const_char c1)) else
  reloc_pat (deep_mkrangepat c1 c2)

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_num closing_name closing_num =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(rhs_loc opening_num, opening_name,
                                           rhs_loc closing_num, closing_name)))

let expecting pos nonterm =
    raise Syntaxerr.(Error(Expecting(rhs_loc pos, nonterm)))

let lapply p1 p2 =
  if !Clflags.applicative_functors
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(Syntaxerr.Applicative_path (symbol_rloc())))

let pat_of_label lbl pos =
  mkpat (Ppat_var (mkrhs (Longident.last lbl) pos))


%}

/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
/* %token PARSER */
%token PLUS
%token PLUSDOT
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Location.t> COMMENT

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT  /* expr (e OP e OP e) */
%left     INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT


/* Entry points */

%start interface                        /* for interface files */
%type <Parsetree.signature> interface
%start any_longident
%type <Longident.t> any_longident

%start pattern_longident
%type <Longident.t> pattern_longident

%start poly_type
%type <Parsetree.core_type> poly_type

%%

/* Entry points */

interface:
    signature EOF                        { List.rev $1 }
;

/* Module types */

module_type:
    mty_longident
      { mkmty(Pmty_ident (mkrhs $1 1)) }
  | SIG signature END
      { mkmty(Pmty_signature(List.rev $2)) }
  | SIG signature error
      { unclosed "sig" 1 "end" 3 }
  | FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_type
      %prec below_WITH
      { mkmty(Pmty_functor(mkrhs $3 3, $5, $8)) }
  | module_type WITH with_constraints
      { mkmty(Pmty_with($1, List.rev $3)) }
/*
  | MODULE TYPE OF module_expr
      { mkmty(Pmty_typeof $4) }
*/
  | LPAREN module_type RPAREN
      { $2 }
  | LPAREN module_type error
      { unclosed "(" 1 ")" 3 }
;
signature:
    /* empty */                                 { [] }
  | signature signature_item                    { $2 :: $1 }
  | signature signature_item SEMISEMI           { $2 :: $1 }
;
signature_item:
    VAL val_ident COLON core_type
      { mksig(Psig_value(mkrhs $2 2, {pval_type = $4; pval_prim = [];
          pval_loc = symbol_rloc()})) }
/*
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
      { mksig(Psig_value(mkrhs $2 2, {pval_type = $4; pval_prim = $6;
          pval_loc = symbol_rloc()})) }
  | TYPE type_declarations
      { mksig(Psig_type(List.rev $2)) }
  | EXCEPTION UIDENT constructor_arguments
      { mksig(Psig_exception(mkrhs $2 2, $3)) }
  | MODULE UIDENT module_declaration
      { mksig(Psig_module(mkrhs $2 2, $3)) }
  | MODULE REC module_rec_declarations
      { mksig(Psig_recmodule(List.rev $3)) }
  | MODULE TYPE ident
      { mksig(Psig_modtype(mkrhs $3 3, Pmodtype_abstract)) }
  | MODULE TYPE ident EQUAL module_type
      { mksig(Psig_modtype(mkrhs $3 3, Pmodtype_manifest $5)) }
  | OPEN override_flag mod_longident
      { mksig(Psig_open ($2, mkrhs $3 3)) }
  | INCLUDE module_type
      { mksig(Psig_include $2) }
  | CLASS class_descriptions
      { mksig(Psig_class (List.rev $2)) }
  | CLASS TYPE class_type_declarations
      { mksig(Psig_class_type (List.rev $3)) }
*/
;

module_declaration:
    COLON module_type
      { $2 }
  | LPAREN UIDENT COLON module_type RPAREN module_declaration
      { mkmty(Pmty_functor(mkrhs $2 2, $4, $6)) }
;
module_rec_declarations:
    module_rec_declaration                              { [$1] }
  | module_rec_declarations AND module_rec_declaration  { $3 :: $1 }
;
module_rec_declaration:
    UIDENT COLON module_type                            { (mkrhs $1 1, $3) }
;

/* Class expressions */

class_type_parameters:
    /*empty*/                                   { [], symbol_gloc () }
  | LBRACKET type_parameter_list RBRACKET       { List.rev $2, symbol_rloc () }
;
parent_binder:
    AS LIDENT
          { Some $2 }
  | /* empty */
          { None }
;
virtual_value:
    override_flag MUTABLE VIRTUAL label COLON core_type
      { if $1 = Override then syntax_error ();
        mkloc $4 (rhs_loc 4), Mutable, $6 }
  | VIRTUAL mutable_flag label COLON core_type
      { mkrhs $3 3, $2, $5 }
;
virtual_method:
    METHOD override_flag PRIVATE VIRTUAL label COLON poly_type
      { if $2 = Override then syntax_error ();
        mkloc $5 (rhs_loc 5), Private, $7 }
  | METHOD override_flag VIRTUAL private_flag label COLON poly_type
      { if $2 = Override then syntax_error ();
        mkloc $5 (rhs_loc 5), $4, $7 }
;

/* Class types */

class_type:
    class_signature
      { $1 }
  | QUESTION LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
      { mkcty(Pcty_fun("?" ^ $2 , mkoption $4, $6)) }
  | OPTLABEL simple_core_type_or_tuple MINUSGREATER class_type
      { mkcty(Pcty_fun("?" ^ $1, mkoption $2, $4)) }
  | LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
      { mkcty(Pcty_fun($1, $3, $5)) }
  | simple_core_type_or_tuple MINUSGREATER class_type
      { mkcty(Pcty_fun("", $1, $3)) }
;
class_signature:
    LBRACKET core_type_comma_list RBRACKET clty_longident
      { mkcty(Pcty_constr (mkloc $4 (rhs_loc 4), List.rev $2)) }
  | clty_longident
      { mkcty(Pcty_constr (mkrhs $1 1, [])) }
  | OBJECT class_sig_body END
      { mkcty(Pcty_signature $2) }
  | OBJECT class_sig_body error
      { unclosed "object" 1 "end" 3 }
;
class_sig_body:
    class_self_type class_sig_fields
    { { pcsig_self = $1; pcsig_fields = List.rev $2;
      pcsig_loc = symbol_rloc(); } }
;
class_self_type:
    LPAREN core_type RPAREN
      { $2 }
  | /* empty */
      { mktyp(Ptyp_any) }
;
class_sig_fields:
    /* empty */                                 { [] }
| class_sig_fields class_sig_field     { $2 :: $1 }
;
class_sig_field:
    INHERIT class_signature       { mkctf (Pctf_inher $2) }
  | VAL value_type              { mkctf (Pctf_val $2) }
  | virtual_method_type         { mkctf (Pctf_virt $1) }
  | method_type                 { mkctf (Pctf_meth $1) }
  | CONSTRAINT constrain_field        { mkctf (Pctf_cstr $2) }
;
value_type:
    VIRTUAL mutable_flag label COLON core_type
      { $3, $2, Virtual, $5 }
  | MUTABLE virtual_flag label COLON core_type
      { $3, Mutable, $2, $5 }
  | label COLON core_type
      { $1, Immutable, Concrete, $3 }
;
method_type:
    METHOD private_flag label COLON poly_type
      { $3, $2, $5 }
;
virtual_method_type:
    METHOD PRIVATE VIRTUAL label COLON poly_type
      { $4, Private, $6 }
  | METHOD VIRTUAL private_flag label COLON poly_type
      { $4, $3, $6 }
;
constrain:
        core_type EQUAL core_type          { $1, $3, symbol_rloc() }
;
constrain_field:
        core_type EQUAL core_type          { $1, $3 }
;
class_descriptions:
    class_descriptions AND class_description    { $3 :: $1 }
  | class_description                           { [$1] }
;
class_description:
    virtual_flag class_type_parameters LIDENT COLON class_type
      { let params, variance = List.split (fst $2) in
        {pci_virt = $1; pci_params = params, snd $2;
         pci_name = mkrhs $3 3; pci_expr = $5; pci_variance = variance;
         pci_loc = symbol_rloc ()} }
;
class_type_declarations:
    class_type_declarations AND class_type_declaration  { $3 :: $1 }
  | class_type_declaration                              { [$1] }
;
class_type_declaration:
    virtual_flag class_type_parameters LIDENT EQUAL class_signature
      { let params, variance = List.split (fst $2) in
        {pci_virt = $1; pci_params = params, snd $2;
         pci_name = mkrhs $3 3; pci_expr = $5; pci_variance = variance;
         pci_loc = symbol_rloc ()} }
;

/* Core expressions */

pattern_var:
    LIDENT            { mkpat(Ppat_var (mkrhs $1 1)) }
  | UNDERSCORE        { mkpat Ppat_any }
;
label_let_pattern:
    label_var
      { $1 }
  | label_var COLON core_type
      { let (lab, pat) = $1 in (lab, mkpat(Ppat_constraint(pat, $3))) }
;
label_var:
    LIDENT    { ($1, mkpat(Ppat_var (mkrhs $1 1))) }
;
let_pattern:
    pattern
      { $1 }
  | pattern COLON core_type
      { mkpat(Ppat_constraint($1, $3)) }
;

lident_list:
    LIDENT                            { [$1] }
  | LIDENT lident_list                { $1 :: $2 }
;
type_constraint:
    COLON core_type                             { (Some $2, None) }
  | COLON core_type COLONGREATER core_type      { (Some $2, Some $4) }
  | COLONGREATER core_type                      { (None, Some $2) }
  | COLON error                                 { syntax_error() }
  | COLONGREATER error                          { syntax_error() }
;

/* Patterns */

pattern:
    simple_pattern
      { $1 }
  | pattern AS val_ident
      { mkpat(Ppat_alias($1, mkrhs $3 3)) }
  | pattern AS error
      { expecting 3 "identifier" }
  | pattern_comma_list  %prec below_COMMA
      { mkpat(Ppat_tuple(List.rev $1)) }
  | constr_longident pattern %prec prec_constr_appl
      { mkpat(Ppat_construct(mkrhs $1 1, Some $2, false)) }
  | name_tag pattern %prec prec_constr_appl
      { mkpat(Ppat_variant($1, Some $2)) }
  | pattern COLONCOLON pattern
      { mkpat_cons (rhs_loc 2) (ghpat(Ppat_tuple[$1;$3])) (symbol_rloc()) }
  | pattern COLONCOLON error
      { expecting 3 "pattern" }
  | LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern RPAREN
      { mkpat_cons (rhs_loc 2) (ghpat(Ppat_tuple[$5;$7])) (symbol_rloc()) }
  | LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern error
      { unclosed "(" 4 ")" 8 }
  | pattern BAR pattern
      { mkpat(Ppat_or($1, $3)) }
  | pattern BAR error
      { expecting 3 "pattern" }
  | LAZY simple_pattern
      { mkpat(Ppat_lazy $2) }
;
simple_pattern:
    val_ident %prec below_EQUAL
      { mkpat(Ppat_var (mkrhs $1 1)) }
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
  | CHAR DOTDOT CHAR
      { mkrangepat $1 $3 }
  | constr_longident
      { mkpat(Ppat_construct(mkrhs $1 1, None, false)) }
  | name_tag
      { mkpat(Ppat_variant($1, None)) }
  | SHARP type_longident
      { mkpat(Ppat_type (mkrhs $2 2)) }
  | LBRACE lbl_pattern_list RBRACE
      { let (fields, closed) = $2 in mkpat(Ppat_record(fields, closed)) }
  | LBRACE lbl_pattern_list error
      { unclosed "{" 1 "}" 3 }
  | LBRACKET pattern_semi_list opt_semi RBRACKET
      { reloc_pat (mktailpat (rhs_loc 4) (List.rev $2)) }
  | LBRACKET pattern_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
  | LBRACKETBAR pattern_semi_list opt_semi BARRBRACKET
      { mkpat(Ppat_array(List.rev $2)) }
  | LBRACKETBAR BARRBRACKET
      { mkpat(Ppat_array []) }
  | LBRACKETBAR pattern_semi_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
  | LPAREN pattern RPAREN
      { reloc_pat $2 }
  | LPAREN pattern error
      { unclosed "(" 1 ")" 3 }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" 1 ")" 5 }
  | LPAREN pattern COLON error
      { expecting 4 "type" }
  | LPAREN MODULE UIDENT RPAREN
      { mkpat(Ppat_unpack (mkrhs $3 3)) }
  | LPAREN MODULE UIDENT COLON package_type RPAREN
      { mkpat(Ppat_constraint(mkpat(Ppat_unpack (mkrhs $3 3)),
                              ghtyp(Ptyp_package $5))) }
  | LPAREN MODULE UIDENT COLON package_type error
      { unclosed "(" 1 ")" 6 }
;

pattern_comma_list:
    pattern_comma_list COMMA pattern            { $3 :: $1 }
  | pattern COMMA pattern                       { [$3; $1] }
  | pattern COMMA error                         { expecting 3 "pattern" }
;
pattern_semi_list:
    pattern                                     { [$1] }
  | pattern_semi_list SEMI pattern              { $3 :: $1 }
;
lbl_pattern_list:
    lbl_pattern { [$1], Closed }
  | lbl_pattern SEMI { [$1], Closed }
  | lbl_pattern SEMI UNDERSCORE opt_semi { [$1], Open }
  | lbl_pattern SEMI lbl_pattern_list
      { let (fields, closed) = $3 in $1 :: fields, closed }
;
lbl_pattern:
    label_longident EQUAL pattern
      { (mkrhs $1 1,$3) }
  | label_longident
      { (mkrhs $1 1, pat_of_label $1 1) }
;

/* Primitive declarations */

primitive_declaration:
    STRING                                      { [$1] }
  | STRING primitive_declaration                { $1 :: $2 }
;

/* Type declarations */

type_declarations:
    type_declaration                            { [$1] }
  | type_declarations AND type_declaration      { $3 :: $1 }
;

type_declaration:
    optional_type_parameters LIDENT type_kind constraints
      { let (params, variance) = List.split $1 in
        let (kind, private_flag, manifest) = $3 in
        (mkrhs $2 2, {ptype_params = params;
              ptype_cstrs = List.rev $4;
              ptype_kind = kind;
              ptype_private = private_flag;
              ptype_manifest = manifest;
              ptype_variance = variance;
              ptype_loc = symbol_rloc() }) }
;
constraints:
        constraints CONSTRAINT constrain        { $3 :: $1 }
      | /* empty */                             { [] }
;
type_kind:
    /*empty*/
      { (Ptype_abstract, Public, None) }
  | EQUAL core_type
      { (Ptype_abstract, Public, Some $2) }
  | EQUAL PRIVATE core_type
      { (Ptype_abstract, Private, Some $3) }
  | EQUAL constructor_declarations
      { (Ptype_variant(List.rev $2), Public, None) }
  | EQUAL PRIVATE constructor_declarations
      { (Ptype_variant(List.rev $3), Private, None) }
  | EQUAL private_flag BAR constructor_declarations
      { (Ptype_variant(List.rev $4), $2, None) }
  | EQUAL private_flag LBRACE label_declarations opt_semi RBRACE
      { (Ptype_record(List.rev $4), $2, None) }
  | EQUAL core_type EQUAL private_flag opt_bar constructor_declarations
      { (Ptype_variant(List.rev $6), $4, Some $2) }
  | EQUAL core_type EQUAL private_flag LBRACE label_declarations opt_semi RBRACE
      { (Ptype_record(List.rev $6), $4, Some $2) }
;
optional_type_parameters:
    /*empty*/                                   { [] }
  | optional_type_parameter                              { [$1] }
  | LPAREN optional_type_parameter_list RPAREN  { List.rev $2 }
;
optional_type_parameter:
    type_variance QUOTE ident                   { Some (mkrhs $3 3), $1 }
  | type_variance UNDERSCORE                    { None, $1 }
;
optional_type_parameter_list:
    optional_type_parameter                              { [$1] }
  | optional_type_parameter_list COMMA optional_type_parameter    { $3 :: $1 }
;



type_parameters:
    /*empty*/                                   { [] }
  | type_parameter                              { [$1] }
  | LPAREN type_parameter_list RPAREN           { List.rev $2 }
;
type_parameter:
    type_variance QUOTE ident                   { mkrhs $3 3, $1 }
;
type_variance:
    /* empty */                                 { false, false }
  | PLUS                                        { true, false }
  | MINUS                                       { false, true }
;
type_parameter_list:
    type_parameter                              { [$1] }
  | type_parameter_list COMMA type_parameter    { $3 :: $1 }
;
constructor_declarations:
    constructor_declaration                     { [$1] }
  | constructor_declarations BAR constructor_declaration { $3 :: $1 }
;
constructor_declaration:

  | constr_ident generalized_constructor_arguments
      { let arg_types,ret_type = $2 in
        (mkrhs $1 1, arg_types,ret_type, symbol_rloc()) }
;

constructor_arguments:
    /*empty*/                                   { [] }
  | OF core_type_list                           { List.rev $2 }
;

generalized_constructor_arguments:
    /*empty*/                                   { ([],None) }
  | OF core_type_list                           { (List.rev $2,None) }
  | COLON core_type_list MINUSGREATER simple_core_type
                                                { (List.rev $2,Some $4) }
  | COLON simple_core_type                      { ([],Some $2) }
;



label_declarations:
    label_declaration                           { [$1] }
  | label_declarations SEMI label_declaration   { $3 :: $1 }
;
label_declaration:
    mutable_flag label COLON poly_type
      { (mkrhs $2 2, $1, $4, symbol_rloc()) }
;

/* "with" constraints (additional type equations over signature components) */

with_constraints:
    with_constraint                             { [$1] }
  | with_constraints AND with_constraint        { $3 :: $1 }
;
with_constraint:
    TYPE type_parameters label_longident with_type_binder core_type constraints
      { let params, variance = List.split $2 in
        (mkrhs $3 3,
         Pwith_type {ptype_params = List.map (fun x -> Some x) params;
                     ptype_cstrs = List.rev $6;
                     ptype_kind = Ptype_abstract;
                     ptype_manifest = Some $5;
                     ptype_private = $4;
                     ptype_variance = variance;
                     ptype_loc = symbol_rloc()}) }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
  | TYPE type_parameters label COLONEQUAL core_type
      { let params, variance = List.split $2 in
        (mkrhs (Lident $3) 3,
         Pwith_typesubst { ptype_params = List.map (fun x -> Some x) params;
                           ptype_cstrs = [];
                           ptype_kind = Ptype_abstract;
                           ptype_manifest = Some $5;
                           ptype_private = Public;
                           ptype_variance = variance;
                           ptype_loc = symbol_rloc()}) }
  | MODULE mod_longident EQUAL mod_ext_longident
      { (mkrhs $2 2, Pwith_module (mkrhs $4 4)) }
  | MODULE UIDENT COLONEQUAL mod_ext_longident
      { (mkrhs (Lident $2) 2, Pwith_modsubst (mkrhs $4 4)) }
;
with_type_binder:
    EQUAL          { Public }
  | EQUAL PRIVATE  { Private }
;

/* Polymorphic types */

typevar_list:
        QUOTE ident                             { [$2] }
      | typevar_list QUOTE ident                { $3 :: $1 }
;
poly_type:
        core_type
          { mktyp(Ptyp_poly([], $1)) }
      | typevar_list DOT core_type
          { mktyp(Ptyp_poly(List.rev $1, $3)) }
;

/* Core types */

core_type:
    core_type2
      { $1 }
  | core_type2 AS QUOTE ident
      { mktyp(Ptyp_alias($1, $4)) }
;
core_type2:
    simple_core_type_or_tuple
      { $1 }
  | QUESTION LIDENT COLON core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("?" ^ $2 , mkoption $4, $6)) }
  | OPTLABEL core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("?" ^ $1 , mkoption $2, $4)) }
  | LIDENT COLON core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow($1, $3, $5)) }
  | core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("", $1, $3)) }
;

simple_core_type:
    simple_core_type2  %prec below_SHARP
      { $1 }
  | LPAREN core_type_comma_list RPAREN %prec below_SHARP
      { match $2 with [sty] -> sty | _ -> raise Parse_error }
;
simple_core_type2:
    QUOTE ident
      { mktyp(Ptyp_var $2) }
  | UNDERSCORE
      { mktyp(Ptyp_any) }
  | type_longident
      { mktyp(Ptyp_constr(mkrhs $1 1, [])) }
  | simple_core_type2 type_longident
      { mktyp(Ptyp_constr(mkrhs $2 2, [$1])) }
  | LPAREN core_type_comma_list RPAREN type_longident
      { mktyp(Ptyp_constr(mkrhs $4 4, List.rev $2)) }
  | LESS meth_list GREATER
      { mktyp(Ptyp_object $2) }
  | LESS GREATER
      { mktyp(Ptyp_object []) }
  | SHARP class_longident opt_present
      { mktyp(Ptyp_class(mkrhs $2 2, [], $3)) }
  | simple_core_type2 SHARP class_longident opt_present
      { mktyp(Ptyp_class(mkrhs $3 3, [$1], $4)) }
  | LPAREN core_type_comma_list RPAREN SHARP class_longident opt_present
      { mktyp(Ptyp_class(mkrhs $5 5, List.rev $2, $6)) }
  | LBRACKET tag_field RBRACKET
      { mktyp(Ptyp_variant([$2], true, None)) }
/* PR#3835: this is not LR(1), would need lookahead=2
  | LBRACKET simple_core_type RBRACKET
      { mktyp(Ptyp_variant([$2], true, None)) }
*/
  | LBRACKET BAR row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, true, None)) }
  | LBRACKET row_field BAR row_field_list RBRACKET
      { mktyp(Ptyp_variant($2 :: List.rev $4, true, None)) }
  | LBRACKETGREATER opt_bar row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, false, None)) }
  | LBRACKETGREATER RBRACKET
      { mktyp(Ptyp_variant([], false, None)) }
  | LBRACKETLESS opt_bar row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, true, Some [])) }
  | LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, true, Some (List.rev $5))) }
  | LPAREN MODULE package_type RPAREN
      { mktyp(Ptyp_package $3) }
;
package_type:
    mty_longident { (mkrhs $1 1, []) }
  | mty_longident WITH package_type_cstrs { (mkrhs $1 1, $3) }
;
package_type_cstr:
    TYPE label_longident EQUAL core_type { (mkrhs $2 2, $4) }
;
package_type_cstrs:
    package_type_cstr { [$1] }
  | package_type_cstr AND package_type_cstrs { $1::$3 }
;
row_field_list:
    row_field                                   { [$1] }
  | row_field_list BAR row_field                { $3 :: $1 }
;
row_field:
    tag_field                                   { $1 }
  | simple_core_type                            { Rinherit $1 }
;
tag_field:
    name_tag OF opt_ampersand amper_type_list
      { Rtag ($1, $3, List.rev $4) }
  | name_tag
      { Rtag ($1, true, []) }
;
opt_ampersand:
    AMPERSAND                                   { true }
  | /* empty */                                 { false }
;
amper_type_list:
    core_type                                   { [$1] }
  | amper_type_list AMPERSAND core_type         { $3 :: $1 }
;
opt_present:
    LBRACKETGREATER name_tag_list RBRACKET      { List.rev $2 }
  | /* empty */                                 { [] }
;
name_tag_list:
    name_tag                                    { [$1] }
  | name_tag_list name_tag                      { $2 :: $1 }
;
simple_core_type_or_tuple:
    simple_core_type                            { $1 }
  | simple_core_type STAR core_type_list
      { mktyp(Ptyp_tuple($1 :: List.rev $3)) }
;
core_type_comma_list:
    core_type                                   { [$1] }
  | core_type_comma_list COMMA core_type        { $3 :: $1 }
;
core_type_list:
    simple_core_type                            { [$1] }
  | core_type_list STAR simple_core_type        { $3 :: $1 }
;
meth_list:
    field SEMI meth_list                        { $1 :: $3 }
  | field opt_semi                              { [$1] }
  | DOTDOT                                      { [mkfield Pfield_var] }
;
field:
    label COLON poly_type                       { mkfield(Pfield($1, $3)) }
;
label:
    LIDENT                                      { $1 }
;

/* Constants */

constant:
    INT                                         { Const_int $1 }
  | CHAR                                        { Const_char $1 }
  | STRING                                      { Const_string $1 }
  | FLOAT                                       { Const_float $1 }
  | INT32                                       { Const_int32 $1 }
  | INT64                                       { Const_int64 $1 }
  | NATIVEINT                                   { Const_nativeint $1 }
;
signed_constant:
    constant                               { $1 }
  | MINUS INT                              { Const_int(- $2) }
  | MINUS FLOAT                            { Const_float("-" ^ $2) }
  | MINUS INT32                            { Const_int32(Int32.neg $2) }
  | MINUS INT64                            { Const_int64(Int64.neg $2) }
  | MINUS NATIVEINT                        { Const_nativeint(Nativeint.neg $2) }
  | PLUS INT                               { Const_int $2 }
  | PLUS FLOAT                             { Const_float $2 }
  | PLUS INT32                             { Const_int32 $2 }
  | PLUS INT64                             { Const_int64 $2 }
  | PLUS NATIVEINT                         { Const_nativeint $2 }
;

/* Identifiers and long identifiers */

ident:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
;
val_ident:
    LIDENT                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
  | LPAREN operator error                       { unclosed "(" 1 ")" 3 }
  | LPAREN error                                { expecting 2 "operator" }
  | LPAREN MODULE error                         { expecting 3 "module-expr" }
;
operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | BANG                                        { "!" }
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
;
constr_ident:
    UIDENT                                      { $1 }
/*  | LBRACKET RBRACKET                           { "[]" } */
  | LPAREN RPAREN                               { "()" }
  | COLONCOLON                                  { "::" }
/*  | LPAREN COLONCOLON RPAREN                    { "::" } */
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;

val_longident:
    val_ident                                   { Lident $1 }
  | mod_longident DOT val_ident                 { Ldot($1, $3) }
;
constr_longident:
    mod_longident       %prec below_DOT         { $1 }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;
label_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;
type_longident:
    LIDENT                                      { Lident $1 }
  | TILDE LIDENT                                { Lident ("~" ^ $2) }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mod_ext_longident:
    UIDENT                                      { Lident $1 }
  | TILDE LIDENT                                { Lident ("~" ^ $2) }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
  | mod_ext_longident DOT TILDE LIDENT          { Ldot($1, "~" ^ $4) }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN { lapply $1 $3 }
  | LBRACE package_ident RBRACE                 { Lident ("{" ^ $2 ^ "}") }
;
mty_longident:
    ident                                       { Lident $1 }
  | mod_ext_longident DOT ident                 { Ldot($1, $3) }
;
clty_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
class_longident:
    LIDENT                                      { Lident $1 }
/*
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
*/
  | mod_ext_longident DOT LIDENT                    { Ldot($1, $3) }
;
any_longident:
    val_ident                                   { Lident $1 }
  | mod_ext_longident DOT val_ident             { Ldot ($1, $3) }
  | mod_ext_longident                           { $1 }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;

/* Miscellaneous */

name_tag:
    BACKQUOTE ident                             { $2 }
;
rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
private_flag:
    /* empty */                                 { Public }
  | PRIVATE                                     { Private }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
virtual_flag:
    /* empty */                                 { Concrete }
  | VIRTUAL                                     { Virtual }
;
override_flag:
    /* empty */                                 { Fresh }
  | BANG                                        { Override }
;
opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;
additive:
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
;





pattern_ident:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
  | UNDERSCORE                                  { "_" }
  | STAR                                        { "_*_" }
  | LPAREN operator RPAREN                      { $2 }
  | LPAREN UNDERSCORE RPAREN                    { "(_)" }
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
  | LPAREN LPAREN RPAREN RPAREN                 { "()" }
  | LPAREN LBRACKET RBRACKET RPAREN             { "[]" }
  | LPAREN COLONCOLON RPAREN                    { "::" }
;

package_ident:
    simple_package_dotted                       { $1 }
  | STAR LIDENT STAR                            { "*" ^ $2 ^ "*" }
  | LBRACKET RBRACKET                           { "[]" }
  | LBRACKET package_idents RBRACKET            { "[" ^ String.concat ";" (List.rev $2) ^ "]" }
;

simple_package_dotted:
    simple_package_name                              { $1 }
  | simple_package_dotted DOT simple_package_name    { $1 ^ "." ^ $3 }
  | simple_package_dotted MINUS simple_package_name  { $1 ^ "-" ^ $3 }
  | simple_package_dotted SHARP INT                  { $1 ^ "#" ^ string_of_int $3 }
;

simple_package_name:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
  | FUNCTOR                                     { "functor" (* tyxml.functor *) }
  | TYPE                                        { "type" (* eliom.syntax.type *) }
  | WITH                                        { "with" (* odn.with *) }
  | FOR { "for" (* nethttpd-for-netcgi2 *) }
;

package_idents:
  | simple_package_dotted { [$1] }
  | package_idents SEMI simple_package_dotted { $3 :: $1 }
;

pattern_longident:
    pattern_ident                                      { Lident $1 }
  | LBRACE package_ident RBRACE                        { Lident ("{" ^ $2 ^ "}") }
  | pattern_longident DOT pattern_ident                { Ldot($1, $3) }
  /* Very ugly workaround since *. is parsed as INFIXOP3 */      
  | pattern_longident DOT INFIXOP3 pattern_ident       { match $3 with
                                                         | "*." -> Ldot(Ldot($1, "_*_"), $4) 
                                                             (* CR jfuruse: we can have "*.*." *)
                                                         | _ -> failwith "pattern_longident: strange sym followed after *" }
  | pattern_longident LPAREN pattern_longident RPAREN  { Lapply($1, $3) }
;

%%
