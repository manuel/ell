/***** Executable and Linkable Lisp Compiler *****/

#ifndef ELLC_H
#define ELLC_H

#include "ellrt.h"

struct ellc_id;
struct ellc_ast;
struct ellc_params;
struct ellc_args;

// Normal Form

struct ellc_ast_ref {
    struct ellc_id *id;
};

struct ellc_ast_set {
    struct ellc_id *id;
    struct ellc_ast *val;
};

struct ellc_ast_def {
    struct ellc_id *id;
    struct ellc_ast *val;
};

struct ellc_ast_cond {
    struct ellc_ast *test;
    struct ellc_ast *consequent;
    struct ellc_ast *alternative;
};

struct ellc_ast_seq {
    list_t *exprs; // ast
};

struct ellc_ast_app {
    struct ellc_ast *op;
    struct ellc_args *args;
};

struct ellc_ast_lam {
    struct ellc_params *params;
    struct ellc_ast *body;
    dict_t *env; // id -> ref
    unsigned code_id;
};

struct ellc_ast_defp { // "boundp"
    struct ellc_id *id;
};

struct ellc_ast_lit_sym {
    struct ell_obj *sym;
};

struct ellc_ast_lit_str {
    struct ell_obj *str;
};

struct ellc_ast_lit_stx {
    struct ell_obj *stx;
};

/* Context node for maintenance of SRFI 72's improved hygiene
   condition.  Introduces a new hygienic context if there currently is
   no context, i.e. if the syntax occurs doesn't occur nested inside a
   quasisyntax. */
struct ellc_ast_cx {
    struct ellc_ast *body;
};

// Explicit Form

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

// AST Representation

enum ellc_ast_type {
    ELLC_AST_REF  = 1, // -> glo_ref | arg_ref | env_ref
    ELLC_AST_DEF  = 2,
    ELLC_AST_SET  = 3, // -> glo_set | arg_set | env_set
    ELLC_AST_COND = 4,
    ELLC_AST_SEQ  = 5,
    ELLC_AST_APP  = 6,
    ELLC_AST_LAM  = 7,
    ELLC_AST_DEFP = 8,

    ELLC_AST_GLO_REF = 101,
    ELLC_AST_GLO_SET = 102,
    ELLC_AST_ARG_REF = 103,
    ELLC_AST_ARG_SET = 104,
    ELLC_AST_ENV_REF = 105,
    ELLC_AST_ENV_SET = 106,

    ELLC_AST_LIT_SYM = 201,
    ELLC_AST_LIT_STR = 202,
    ELLC_AST_LIT_STX = 203,

    ELLC_AST_CX      = 301,
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

        struct ellc_ast_glo_ref glo_ref;
        struct ellc_ast_glo_set glo_set;
        struct ellc_ast_arg_ref arg_ref;
        struct ellc_ast_arg_set arg_set;
        struct ellc_ast_env_ref env_ref;
        struct ellc_ast_env_set env_set;

        struct ellc_ast_lit_sym lit_sym;
        struct ellc_ast_lit_str lit_str;
        struct ellc_ast_lit_stx lit_stx;

        struct ellc_ast_cx      cx;
    };
};

enum ellc_ns { ELLC_NS_VAR, ELLC_NS_FUN };

struct ellc_id {
    struct ell_obj *sym;
    enum ellc_ns ns;
    struct ell_cx *cx;
};

struct ellc_params {
    list_t *req; // param
    list_t *opt; // param
    list_t *key; // param
    struct ellc_param *rest;     // maybe NULL
    struct ellc_param *all_keys; // maybe NULL
};

struct ellc_param {
    struct ellc_id *id;
    struct ellc_ast *init; // maybe NULL
    bool mutable;
    bool closed;
};

struct ellc_args {
    list_t pos; // ast
    dict_t key; // sym -> ast
};

struct ellc_contour {
    struct ellc_ast_lam *lam;
    struct ellc_contour *up; // maybe NULL
};

// Compiler State

static dict_t ellc_mac_tab; // sym -> clo
static dict_t ellc_norm_tab; // sym -> norm_fun

struct ellc_st {
    /* Keeps track of all global variables defined in the compilation
       unit.  Populated during normalization. */
    list_t *defined_globals; // id
    /* Keeps track of lexical contours during normalization and
       closure conversion. */
    struct ellc_contour *bottom_contour; // maybe NULL
    /* Keeps track of all globals used in the compilation unit.
       Populated during closure conversion. */
    list_t *globals; // id
    /* Keeps track of all lambdas created in the compilation unit.
       Populated during closure conversion. */
    list_t *lambdas; // lam
    /* Whether we are inside a quasisyntax, for improved hygiene
       condition.  Not fully clear whether this is correct. */
    bool in_quasisyntax;
    /* The output file for C code during emission. */
    FILE *f;
};

// API

struct ell_obj *
ellc_eval(struct ell_obj *stx_lst);

#endif
