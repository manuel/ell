/***** Executable and Linkable Lisp Compiler *****/

#ifndef ELLC_H
#define ELLC_H

#include "ellrt.h"

struct ellc_id;
struct ellc_ast;
struct ellc_params;
struct ellc_args;
struct ellc_lex_addr;

/**** Normal Form ****/

struct ellc_ast_ref {
    struct ellc_id *id;
};

struct ellc_ast_fref {
    struct ellc_id *id;
};

struct ellc_ast_def {
    struct ellc_id *id;
    struct ellc_ast *val;
};

struct ellc_ast_fdef {
    struct ellc_id *id;
    struct ellc_ast *val;
};

struct ellc_ast_set {
    struct ellc_id *id;
    struct ellc_ast *val;
};

struct ellc_ast_fset {
    struct ellc_id *id;
    struct ellc_ast *val;
};

struct ellc_ast_cond {
    struct ellc_ast *test;
    struct ellc_ast *consequent;
    struct ellc_ast *alternative;
};

struct ellc_ast_seq {
    list_t *exprs;
};

struct ellc_ast_app {
    struct ellc_ast *op;
    struct ellc_args *args;
};

struct ellc_ast_lam {
    struct ellc_params *params;
    struct ellc_ast *body;
};

/**** Explicit Form ****/

struct ellc_ast_glo_ref {
    struct ellc_id *id;
};

struct ellc_ast_glo_fref {
    struct ellc_id *id;
};

struct ellc_ast_glo_set {
    struct ellc_id *id;
    struct ellc_ast *val;
};

struct ellc_ast_glo_fset {
    struct ellc_id *id;
    struct ellc_ast *val;
};

struct ellc_ast_glo_app {
    struct ellc_id *id;
    struct ellc_args *args;
};

struct ellc_ast_loc_ref {
    struct ellc_lex_addr *lex_addr;
};

struct ellc_ast_loc_fref {
    struct ellc_lex_addr *lex_addr;
};

struct ellc_ast_loc_set {
    struct ellc_lex_addr *lex_addr;
    struct ellc_ast *val;
};

struct ellc_ast_loc_fset {
    struct ellc_lex_addr *lex_addr;
    struct ellc_ast *val;
};

struct ellc_ast_loc_app {
    struct ellc_lex_addr *lex_addr;
    struct ellc_args *args;
};

struct ellc_ast_clo {
    unsigned *code_id;
    list_t *env_vals;
};

/**** Expression Representation ****/

enum ellc_ast_type {
    ELLC_AST_REF,
    ELLC_AST_FREF,
    ELLC_AST_DEF,
    ELLC_AST_FDEF,
    ELLC_AST_SET,
    ELLC_AST_FSET,
    ELLC_AST_COND,
    ELLC_AST_SEQ,
    ELLC_AST_APP,
    ELLC_AST_LAM,
    
    ELLC_AST_GLO_REF,
    ELLC_AST_GLO_FREF,
    ELLC_AST_GLO_SET,
    ELLC_AST_GLO_FSET,
    ELLC_AST_GLO_APP,
    ELLC_AST_LOC_REF,
    ELLC_AST_LOC_FREF,
    ELLC_AST_LOC_SET,
    ELLC_AST_LOC_FSET,
    ELLC_AST_LOC_APP,
    ELLC_AST_CLO,
};

struct ellc_ast {
    enum ellc_ast_type type;
    __extension__ union {
        struct ellc_ast_ref  ref;
        struct ellc_ast_fref fref;
        struct ellc_ast_def  def;
        struct ellc_ast_fdef fdef;
        struct ellc_ast_set  set;
        struct ellc_ast_fset fset;
        struct ellc_ast_cond cond;
        struct ellc_ast_seq  seq;
        struct ellc_ast_app  app;
        struct ellc_ast_lam  lam;

        struct ellc_ast_glo_ref  glo_ref;
        struct ellc_ast_glo_fref glo_fref;
        struct ellc_ast_glo_set  glo_set;
        struct ellc_ast_glo_fset glo_fset;
        struct ellc_ast_glo_app  glo_app;
        struct ellc_ast_loc_ref  loc_ref;
        struct ellc_ast_loc_fref loc_fref;
        struct ellc_ast_loc_set  loc_set;
        struct ellc_ast_loc_fset loc_fset;
        struct ellc_ast_loc_app  loc_app;
        struct ellc_ast_clo      clo;
    };
};

struct ellc_id {
    struct ell_obj *sym;
};

struct ellc_params {
    list_t *req; // param
    list_t *opt; // param
    list_t *key; // param
    struct ellc_id *rest;
    struct ellc_id *all_keys;
};

struct ellc_param {
    enum { ELLC_PARAM_VAR, ELLC_PARAM_FUN } type;
    struct ellc_id *id;
    struct ellc_ast *init;
};

struct ellc_args {
    list_t pos; // ast
    dict_t key; // sym -> ast
};

struct ellc_lex_addr {
    unsigned depth;
    unsigned pos;
};

/**** Compiler State ****/

struct ellc_contour {
    struct ellc_contour *up;
    list_t formals; // id
};

/**** Symbols ****/

ELL_DEFSYM(core_def,  "ell-def")
ELL_DEFSYM(core_fdef, "ell-fdef")
ELL_DEFSYM(core_fref, "ell-fref")
ELL_DEFSYM(core_set,  "ell-set")
ELL_DEFSYM(core_fset, "ell-fset")
ELL_DEFSYM(core_lam,  "ell-lam")
ELL_DEFSYM(core_app,  "ell-app")
ELL_DEFSYM(core_cond, "ell-cond")
ELL_DEFSYM(core_seq,  "ell-seq")

ELL_DEFSYM(param_optional, "&optional")
ELL_DEFSYM(param_key, "&key")
ELL_DEFSYM(param_rest, "&rest")
ELL_DEFSYM(param_all_keys, "&all-keys")

#endif
