/***** Executable and Linkable Lisp Compiler *****/

#ifndef ELLC_H
#define ELLC_H

#include "ellrt.h"

struct ellc_id;
struct ellc_ast;
struct ellc_params;
struct ellc_args;

/**** Normal Form ****/

/* After macroexpansion, the S-expression input gets converted to AST
   objects in this form.  Further passes destructively modify or
   augment this information. */

/* Variable reference. */
struct ellc_ast_ref {
    struct ellc_id *id;
};

/* Variable update. */
struct ellc_ast_set {
    struct ellc_id *id;
    struct ellc_ast *val;
};

/* Global variable definition or update. */
struct ellc_ast_def {
    struct ellc_id *id;
    struct ellc_ast *val;
};

/* Conditional. */
struct ellc_ast_cond {
    struct ellc_ast *test;
    struct ellc_ast *consequent;
    struct ellc_ast *alternative;
};

/* Sequential. */
struct ellc_ast_seq {
    list_t *exprs; // ast
};

/* Function application. */
struct ellc_ast_app {
    struct ellc_ast *op;
    struct ellc_args *args;
};

/* Abstraction.

   .env: Free variables, populated during closure conversion.
   Initially consists of ast_refs, which then get converted to
   argument or environment references.

   .code_id: Sequence number of lambda in compilation unit, for
   linking the closure to its generated C function.  Corresponds to
   offset of lambda in compiler state's list of lambdas from the
   current compilation unit. */
struct ellc_ast_lam {
    struct ellc_params *params;
    struct ellc_ast *body;
    struct ell_obj *name; // sym
    dict_t *env;
    unsigned code_id;
};

/* Checks whether identifier names a defined global variable.
   Unlike Common Lisp's BOUNDP, does not evaluate its argument. */
struct ellc_ast_defp {
    struct ellc_id *id;
};

/* Infinite loop. */
struct ellc_ast_loop {
    struct ellc_ast *body;
};

/* Literal symbol, produced by QUOTE. */
struct ellc_ast_lit_sym {
    struct ell_obj *sym;
};

/* Literal string. */
struct ellc_ast_lit_str {
    struct ell_obj *str;
};

/* Literal number. */
struct ellc_ast_lit_num {
    struct ell_obj *num;
};

/* Literal syntax object, produced by QUASISYNTAX. */
struct ellc_ast_lit_stx {
    struct ell_obj *stx; // ast?
};

/* Context node for maintenance of SRFI 72's improved hygiene
   condition.  This binds the `__ell_cur_cx' (`ellrt.h') variable for
   the body of code. It is introduced for every quasisyntax that is
   not considered enclosed in another quasisyntax. */
struct ellc_ast_cx {
    struct ellc_ast *body;
};

/* Inline C expressions. */
struct ellc_ast_snip {
    struct ellc_ast *body; // seq of strings and other expressions
};

/**** Explicit Form ****/

/* During closure conversion, the normal form AST gets destructively
   refined to distinguish between references and updates to global
   variables, arguments of the current function, or closed-over
   environment variables of superordinate functions.  Every reference
   or update gets transformed to one of the following AST nodes: */

struct ellc_ast_glo_ref {
    struct ellc_id *id;
};

struct ellc_ast_glo_set {
    struct ellc_id *id;
    struct ellc_ast *val;
};

struct ellc_ast_arg_ref {
    struct ellc_param *param;
};

struct ellc_ast_arg_set {
    struct ellc_param *param;
    struct ellc_ast *val;
};

struct ellc_ast_env_ref {
    struct ellc_param *param;
};

struct ellc_ast_env_set {
    struct ellc_param *param;
    struct ellc_ast *val;
};

/**** AST Representation ****/

enum ellc_ast_type {
    ELLC_AST_REF  = 1, // -> glo_ref | arg_ref | env_ref
    ELLC_AST_DEF  = 2,
    ELLC_AST_SET  = 3, // -> glo_set | arg_set | env_set
    ELLC_AST_COND = 4,
    ELLC_AST_SEQ  = 5,
    ELLC_AST_APP  = 6,
    ELLC_AST_LAM  = 7,
    ELLC_AST_DEFP = 8,
    ELLC_AST_LOOP = 9,
    ELLC_AST_CX   = 10,
    ELLC_AST_SNIP = 11,

    ELLC_AST_GLO_REF = 101,
    ELLC_AST_GLO_SET = 102,
    ELLC_AST_ARG_REF = 103,
    ELLC_AST_ARG_SET = 104,
    ELLC_AST_ENV_REF = 105,
    ELLC_AST_ENV_SET = 106,

    ELLC_AST_LIT_SYM = 201,
    ELLC_AST_LIT_STR = 202,
    ELLC_AST_LIT_STX = 203,
    ELLC_AST_LIT_NUM = 204,
};

struct ellc_ast {
    enum ellc_ast_type type;
    __extension__ union {
        struct ellc_ast_ref ref;
        struct ellc_ast_def def;
        struct ellc_ast_set set;
        struct ellc_ast_cond cond;
        struct ellc_ast_seq seq;
        struct ellc_ast_app app;
        struct ellc_ast_lam lam;
        struct ellc_ast_defp defp;
        struct ellc_ast_loop loop;
        struct ellc_ast_cx cx;
        struct ellc_ast_snip snip;

        struct ellc_ast_glo_ref glo_ref;
        struct ellc_ast_glo_set glo_set;
        struct ellc_ast_arg_ref arg_ref;
        struct ellc_ast_arg_set arg_set;
        struct ellc_ast_env_ref env_ref;
        struct ellc_ast_env_set env_set;

        struct ellc_ast_lit_sym lit_sym;
        struct ellc_ast_lit_str lit_str;
        struct ellc_ast_lit_num lit_num;
        struct ellc_ast_lit_stx lit_stx;
    };
};

/* A note on namespaces, i.e. Lisp-N.  The language is a Lisp-2 on the
   surface, but internally, all the compiler cares about is
   identifiers (`struct ellc_id').  Identifiers carry a namespace
   number (`enum ellc_ns'), which means that in the future we'll be
   able to add an unbounded number of additional namespaces.  Just
   kidding.

   Note that (syntax) symbols themselves do *not* carry a namespace.
   That's because the namespace of a symbol is determined by its
   position in the syntax.  For example, the first element of a
   compound form is always put into the function namespace (Nr. 2).

   Symbols are converted to identifiers during normalization. */
enum ellc_ns {
    ELLC_NS_VAR = 1,
    ELLC_NS_FUN = 2,
};

/* Variable identifier. */
struct ellc_id {
    struct ell_obj *sym;     // name symbol
    enum ellc_ns ns;         // namespace
    struct ell_cx *cx;       // hygiene context
};

/* Parameters of a function. */
struct ellc_params {
    list_t *req; // param
    list_t *opt; // param
    list_t *key; // param
    struct ellc_param *rest;     // maybe NULL
    struct ellc_param *all_keys; // maybe NULL
};

/* A single parameter.  Optional and keyword parameters may have an
   initialization form that's used when the parameter is not supplied.
   During closure conversion, it is determined whether the parameter
   is potentially updated (mutable), and whether it is referenced or
   updated in subordinate lambdas (closed).  Parameters that are both
   mutable and closed are put into heap allocated boxes. */
struct ellc_param {
    struct ellc_id *id;
    struct ellc_ast *init; // maybe NULL
    bool mutable;
    bool closed;
};

/* The arguments to a function call. */
struct ellc_args {
    list_t pos; // ast
    dict_t key; // sym -> ast
};

/**** Compiler State ****/

/* Compiler state, as opposed to compilation state, is maintained
   across unit compilations.  Compiler state comprises the following
   variables: */

/* Table of macros.  Maps macro symbols to expander functions, that
   take a syntax object, and return a syntax object.  To have macro
   definitions available across REPL uses, this table is maintained in
   the compiler process across compilation units.  In the ordinary,
   file compilation use case of the compiler, this makes no difference
   as the compiler process is torn down after every unit.  In
   interactive REPL mode however, this design is crucial: the REPL
   process spawns a new process for the compiler.  Every user input is
   sent interactively to the compiler process, and the resulting
   shared object is again loaded in the REPL process.  But!  - macro
   expanders are not evaluated in the REPL process, they only live in
   the compiler process. */
static dict_t ellc_mac_tab; // sym -> clo

/**** Compilation State ****/

/* Compilation state, as opposed to compiler state, is reset between
   unit compilations. */

/* Lexical contour, helper object used during closure conversion to
   mirror the runtime lexical environment of lambdas. */
struct ellc_contour {
    struct ellc_ast_lam *lam;
    struct ellc_contour *up; // maybe NULL
};

/* Compilation state, maintained during the compilation of a unit. */
struct ellc_st {
    /* Keeps track of all global variables defined in the compilation
       unit.  Populated during normalization. */
    list_t *defined_globals; // id
    /* Keeps track of all macro definitions in the compilation unit.
       Populated during normalization. */
    dict_t *defined_macros; // sym -> stx
    /* Keeps track of lexical contours during normalization and
       closure conversion. */
    struct ellc_contour *bottom_contour; // maybe NULL
    /* Keeps track of all globals used in the compilation unit.
       Populated during closure conversion. */
    list_t *globals; // id
    /* Keeps track of all lambdas created in the compilation unit.
       Populated during closure conversion. */
    list_t *lambdas; // lam
    /* Whether we are inside a quasisyntax, for hygiene condition. */
    bool in_quasisyntax;
    /* The output file for C code during emission. */
    FILE *f;
};

/**** API ****/

int
ellc_compile_file(char *infile, char *faslfile, char *cfaslfile);

#endif
