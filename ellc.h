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

    ELLC_AST_GLO_REF = 101,
    ELLC_AST_GLO_SET = 102,
    ELLC_AST_ARG_REF = 103,
    ELLC_AST_ARG_SET = 104,
    ELLC_AST_ENV_REF = 105,
    ELLC_AST_ENV_SET = 106,
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

        struct ellc_ast_glo_ref glo_ref;
        struct ellc_ast_glo_set glo_set;
        struct ellc_ast_arg_ref arg_ref;
        struct ellc_ast_arg_set arg_set;
        struct ellc_ast_env_ref env_ref;
        struct ellc_ast_env_set env_set;
    };
};

enum ellc_ns { ELLC_NS_VAR, ELLC_NS_FUN };

struct ellc_id {
    struct ell_obj *sym;
    enum ellc_ns ns;
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

// Compiler State

struct ellc_contour {
    struct ellc_ast_lam *lam;
    struct ellc_contour *up; // maybe NULL
};

struct ellc_st {
    struct ellc_contour *bottom_contour; // maybe NULL
    list_t *globals; // id
    list_t *lambdas; // lam
};

// Symbols

ELL_DEFSYM(core_fref, "ell-fref")
ELL_DEFSYM(core_def,  "ell-def")
ELL_DEFSYM(core_fdef, "ell-fdef")
ELL_DEFSYM(core_set,  "ell-set")
ELL_DEFSYM(core_fset, "ell-fset")
ELL_DEFSYM(core_cond, "ell-cond")
ELL_DEFSYM(core_seq,  "ell-seq")
ELL_DEFSYM(core_lam,  "ell-lam")
ELL_DEFSYM(core_app,  "ell-app")

ELL_DEFSYM(param_optional, "&optional")
ELL_DEFSYM(param_key, "&key")
ELL_DEFSYM(param_rest, "&rest")
ELL_DEFSYM(param_all_keys, "&all-keys")

#endif
