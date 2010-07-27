/***** Executable and Linkable Lisp Compiler *****/

//////////////////////////////////////////////////////////////////
//                                                              //
//   _|                  _|      _|_|  _|        _|    _|       //
//   _|_|_|      _|_|_|  _|    _|      _|_|_|        _|_|_|_|   //
//   _|    _|  _|    _|  _|  _|_|_|_|  _|    _|  _|    _|       //
//   _|    _|  _|    _|  _|    _|      _|    _|  _|    _|       //
//   _|    _|    _|_|_|  _|    _|      _|_|_|    _|      _|_|   //
//                                                              //
//                                                              //
//////////////////////////////////////////////////////////////////

#define _GNU_SOURCE
#include <stdio.h>
#include <dlfcn.h>

#include "ellc.h"

struct ell_obj *
ellc_eval(struct ell_obj *stx_lst);

/**** AST Utilities ****/

static struct ellc_id *
ellc_make_id_cx(struct ell_obj *sym, enum ellc_ns ns, struct ell_cx *cx)
{
    ell_assert_brand(sym, ELL_BRAND(sym));
    struct ellc_id *id = (struct ellc_id *) ell_alloc(sizeof(*id));
    id->sym = sym;
    id->ns = ns;
    id->cx = cx;
    return id;    
}

static struct ellc_id *
ellc_make_id(struct ell_obj *sym, enum ellc_ns ns)
{
    return ellc_make_id_cx(sym, ns, NULL);
}

static bool
ellc_id_equal(struct ellc_id *a, struct ellc_id *b)
{
    return (a->sym == b->sym) && (a->ns == b->ns) && (ell_cx_equal(a->cx, b->cx));
}

static int
ellc_id_cmp(struct ellc_id *a, struct ellc_id *b)
{
    int sym_cmp = ell_sym_cmp(a->sym, b->sym);
    if (sym_cmp != 0) {
        return sym_cmp;
    } else {
        int ns_cmp = a->ns - b->ns;
        if (ns_cmp != 0) {
            return ns_cmp;
        } else {
            return ell_cx_cmp(a->cx, b->cx);
        }
    }
}

static struct ellc_ast_seq *
ellc_make_ast_seq()
{
    struct ellc_ast_seq *ast_seq = ell_alloc(sizeof(*ast_seq));
    ast_seq->exprs = ell_util_make_list();
    return ast_seq;
}

static void
ellc_ast_seq_add(struct ellc_ast_seq *ast_seq, struct ellc_ast *expr)
{
    ell_util_list_add(ast_seq->exprs, expr);
}

static struct ellc_ast *
ellc_make_ast(enum ellc_ast_type type)
{
    struct ellc_ast *ast = (struct ellc_ast *) ell_alloc(sizeof(*ast));
    ast->type = type;
    return ast;
}

static int
ellc_param_boxed(struct ellc_param *p)
{
    return (p->closed && p->mutable);
}

/**** Lexical Contour Utilities ****/

static struct ellc_param *
ellc_params_list_lookup(list_t *list, struct ellc_id *id)
{
    for (lnode_t *n = list_first(list); n; n = list_next(list, n)) {
        struct ellc_param *p = (struct ellc_param *) lnode_get(n);
        if (ellc_id_equal(p->id, id))
            return p;
    }
    return NULL;
}

static struct ellc_param *
ellc_params_lookup(struct ellc_params *params, struct ellc_id *id)
{
    struct ellc_param *p;
    if ((p = ellc_params_list_lookup(params->req, id))) return p;
    if ((p = ellc_params_list_lookup(params->opt, id))) return p;
    if ((p = ellc_params_list_lookup(params->key, id))) return p;
    if (params->rest && ellc_id_equal(params->rest->id, id)) return params->rest;
    if (params->all_keys && ellc_id_equal(params->all_keys->id, id)) return params->all_keys;
    return NULL;
}

// Returns the contour containing a parameter with the given ID,
// from the contour C upwards, or NULL if there is no countour
// containing that parameter.  If found, sets OUT to the parameter.
static struct ellc_contour *
ellc_contour_lookup(struct ellc_contour *c, struct ellc_id *id, struct ellc_param **out)
{
    if (!c) return NULL;
    struct ellc_param *p = ellc_params_lookup(c->lam->params, id);
    if (p) {
        if (out) *out = p;
        return c;
    } else {
        return ellc_contour_lookup(c->up, id, out);
    }
}

/**** Normalization: Syntax Objects -> AST ****/

static struct ellc_ast *
ellc_norm_stx(struct ellc_st *st, struct ell_obj *stx);

typedef struct ellc_ast *
(ellc_norm_fun)(struct ellc_st *st, struct ell_obj *stx_lst);

/* (Simple Forms) */

static struct ellc_ast *
ellc_make_ref(struct ellc_st *st, struct ell_obj *stx_sym, enum ellc_ns ns)
{
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_REF);
    ast->ref.id = ellc_make_id_cx(ell_stx_sym_sym(stx_sym), ns, ell_stx_sym_cx(stx_sym));
    return ast;
}

static struct ellc_ast *
ellc_norm_ref(struct ellc_st *st, struct ell_obj *stx_sym)
{
    return ellc_make_ref(st, stx_sym, ELLC_NS_VAR);
}

static struct ellc_ast *
ellc_norm_fref(struct ellc_st *st, struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 2);
    return ellc_make_ref(st, ELL_SEND(stx_lst, second), ELLC_NS_FUN);
}

static struct ellc_ast *
ellc_make_def(struct ellc_st *st, struct ell_obj *stx_lst, enum ellc_ns ns)
{
    ell_assert_stx_lst_len(stx_lst, 3);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_DEF);
    struct ell_obj *stx_sym = ELL_SEND(stx_lst, second);
    ast->def.id = ellc_make_id_cx(ell_stx_sym_sym(stx_sym), ns, ell_stx_sym_cx(stx_sym));
    ast->def.val = ellc_norm_stx(st, ELL_SEND(stx_lst, third));
    ell_util_set_add(st->defined_globals, ast->def.id, (dict_comp_t) &ellc_id_cmp);
    return ast;
}

static struct ellc_ast *
ellc_norm_def(struct ellc_st *st, struct ell_obj *stx_lst)
{
    return ellc_make_def(st, stx_lst, ELLC_NS_VAR);
}

static struct ellc_ast *
ellc_norm_fdef(struct ellc_st *st, struct ell_obj *stx_lst)
{
    return ellc_make_def(st, stx_lst, ELLC_NS_FUN);
}

static struct ellc_ast *
ellc_make_defp(struct ellc_st *st, struct ell_obj *stx_lst, enum ellc_ns ns)
{
    ell_assert_stx_lst_len(stx_lst, 2);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_DEFP);
    struct ell_obj *stx_sym = ELL_SEND(stx_lst, second);
    ast->defp.id = ellc_make_id_cx(ell_stx_sym_sym(stx_sym), ns, ell_stx_sym_cx(stx_sym));
    return ast;
}

static struct ellc_ast *
ellc_norm_defp(struct ellc_st *st, struct ell_obj *stx_lst)
{
    return ellc_make_defp(st, stx_lst, ELLC_NS_VAR);
}

static struct ellc_ast *
ellc_norm_fdefp(struct ellc_st *st, struct ell_obj *stx_lst)
{
    return ellc_make_defp(st, stx_lst, ELLC_NS_FUN);
}

static struct ellc_ast *
ellc_make_set(struct ellc_st *st, struct ell_obj *stx_lst, enum ellc_ns ns)
{
    ell_assert_stx_lst_len(stx_lst, 3);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_SET);
    struct ell_obj *stx_sym = ELL_SEND(stx_lst, second);
    ast->set.id = ellc_make_id_cx(ell_stx_sym_sym(stx_sym), ns, ell_stx_sym_cx(stx_sym));
    ast->set.val = ellc_norm_stx(st, ELL_SEND(stx_lst, third));
    return ast;
}

static struct ellc_ast *
ellc_norm_set(struct ellc_st *st, struct ell_obj *stx_lst)
{
    return ellc_make_set(st, stx_lst, ELLC_NS_VAR);
}

static struct ellc_ast *
ellc_norm_fset(struct ellc_st *st, struct ell_obj *stx_lst)
{
    return ellc_make_set(st, stx_lst, ELLC_NS_FUN);
}

static struct ellc_ast *
ellc_norm_cond(struct ellc_st *st, struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 4);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_COND);
    ast->cond.test = ellc_norm_stx(st, ELL_SEND(stx_lst, second));
    ast->cond.consequent = ellc_norm_stx(st, ELL_SEND(stx_lst, third));
    ast->cond.alternative = ellc_norm_stx(st, ELL_SEND(stx_lst, fourth));
    return ast;
}

static struct ellc_ast *
ellc_norm_seq(struct ellc_st *st, struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len_min(stx_lst, 1);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_SEQ);
    ast->seq.exprs = ell_util_make_list();
    list_t *elts_stx = ell_util_sublist(ell_stx_lst_elts(stx_lst), 1);

    for (lnode_t *n = list_first(elts_stx); n; n = list_next(elts_stx, n)) {
        struct ell_obj *stx = (struct ell_obj *) lnode_get(n);
        ellc_ast_seq_add(&ast->seq, ellc_norm_stx(st, stx));
    }

    return ast;
}

/* (Application and Arguments Dissection) */

static bool
ellc_is_key_arg_sym(struct ell_obj *sym)
{
    struct ell_obj *name_str = ell_sym_name(sym);
    size_t len = ell_str_len(name_str);
    return (len > 1) && (ell_str_char_at(name_str, len - 1) == ':');
}

static struct ell_obj *
ellc_clean_key_arg_sym(struct ell_obj *sym)
{
    ell_assert_brand(sym, ELL_BRAND(sym));
    struct ell_obj *name_str = ell_sym_name(sym);
    return ell_intern(ell_str_poplast(name_str));
}

static struct ellc_args *
ellc_make_args()
{
    struct ellc_args *args = (struct ellc_args *) ell_alloc(sizeof(*args));
    list_init(&args->pos, LISTCOUNT_T_MAX);
    dict_init(&args->key, DICTCOUNT_T_MAX, (dict_comp_t) &ell_sym_cmp);
    return args;
}

static struct ellc_args *
ellc_dissect_args(struct ellc_st *st, list_t *args_stx)
{
    struct ellc_args *args = ellc_make_args();
    for (lnode_t *n = list_first(args_stx); n; n = list_next(args_stx, n)) {
        struct ell_obj *arg_stx = lnode_get(n);
        if ((arg_stx->brand == ELL_BRAND(stx_sym)) &&
            ellc_is_key_arg_sym(ell_stx_sym_sym(arg_stx)))
        {
            n = list_next(args_stx, n);
            if (!n) {
                printf("missing value for keyword argument\n");
                exit(EXIT_FAILURE);
            }
            struct ell_obj *key_arg_sym =
                ellc_clean_key_arg_sym(ell_stx_sym_sym(arg_stx));
            struct ellc_ast *key_arg_val_ast =
                ellc_norm_stx(st, (struct ell_obj *) lnode_get(n));
            ell_util_dict_put(&args->key, key_arg_sym, key_arg_val_ast);
        } else {
            ell_util_list_add(&args->pos, ellc_norm_stx(st, arg_stx));
        }
    }
    return args;
}

static struct ellc_ast *
ellc_make_app(struct ellc_st *st, struct ellc_ast *op, list_t *arg_stx_lst)
{
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_APP);
    ast->app.op = op;
    ast->app.args = ellc_dissect_args(st, arg_stx_lst);
    return ast;
}

static struct ellc_ast *
ellc_norm_app(struct ellc_st *st, struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len_min(stx_lst, 2);
    return ellc_make_app(st, ellc_norm_stx(st, ELL_SEND(stx_lst, second)),
                         ell_util_sublist(ell_stx_lst_elts(stx_lst), 2));
}

static struct ellc_ast *
ellc_norm_ordinary_app(struct ellc_st *st, struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len_min(stx_lst, 1);
    struct ell_obj *op_sym_stx = ELL_SEND(stx_lst, first);
    ell_assert_brand(op_sym_stx, ELL_BRAND(stx_sym));
    struct ellc_ast *op_ast = ellc_make_ast(ELLC_AST_REF);
    op_ast->ref.id = ellc_make_id_cx(ell_stx_sym_sym(op_sym_stx), ELLC_NS_FUN,
                                     ell_stx_sym_cx(op_sym_stx));
    return ellc_make_app(st, op_ast,
                         ell_util_sublist(ell_stx_lst_elts(stx_lst), 1));
}

/* (Abstraction and Parameters Dissection) */

static struct ellc_param *
ellc_dissect_param(struct ellc_st *st, struct ell_obj *p_stx, dict_t *deferred_inits)
{
    struct ellc_param *p = (struct ellc_param *) ell_alloc(sizeof(*p));
    if (p_stx->brand == ELL_BRAND(stx_sym)) {
        p->id = ellc_make_id_cx(ell_stx_sym_sym(p_stx), ELLC_NS_VAR,
                                ell_stx_sym_cx(p_stx));
    } else if (p_stx->brand == ELL_BRAND(stx_lst)) {
        ell_assert_stx_lst_len(p_stx, 2);
        struct ell_obj *name_stx = ELL_SEND(p_stx, first);
        struct ell_obj *init_stx = ELL_SEND(p_stx, second);
        p->id = ellc_make_id_cx(ell_stx_sym_sym(name_stx), ELLC_NS_VAR,
                                ell_stx_sym_cx(name_stx));
        ell_util_dict_put(deferred_inits, p, init_stx);
    }
    return p;
}

static struct ellc_params *
ellc_dissect_params(struct ellc_st *st, list_t *params_stx, dict_t *deferred_inits)
{
    struct ellc_params *params =
        (struct ellc_params *) ell_alloc(sizeof(*params));

    list_t *req = ell_util_make_list();
    list_t *opt = ell_util_make_list();
    list_t *key = ell_util_make_list();
    list_t *rest = ell_util_make_list();
    list_t *all_keys = ell_util_make_list();
    
    list_t *cur = req;
    for (lnode_t *n = list_first(params_stx); n; n = list_next(params_stx, n)) {
        struct ell_obj *p_stx = lnode_get(n);
        if (p_stx->brand == ELL_BRAND(stx_sym)) {
            struct ell_obj *p_sym = ell_stx_sym_sym(p_stx);
            if (p_sym == ELL_SYM(param_optional)) {
                cur = opt;
                continue;
            } else if (p_sym == ELL_SYM(param_key)) {
                cur = key;
                continue;
            } else if (p_sym == ELL_SYM(param_rest)) {
                cur = rest;
                continue;
            } else if (p_sym == ELL_SYM(param_all_keys)) {
                cur = all_keys;
                continue;
            }
        }
        ell_util_list_add(cur, ellc_dissect_param(st, p_stx, deferred_inits));
    }

    if ((list_count(rest) > 1) || (list_count(all_keys) > 1)) {
        printf("more than one rest or all-keys parameter\n");
        exit(EXIT_FAILURE);
    }

    params->req = req;
    params->opt = opt;
    params->key = key;
    if (list_count(rest) == 1)
        params->rest = lnode_get(list_first(rest));
    if (list_count(all_keys) == 1)
        params->all_keys = lnode_get(list_first(all_keys));

    return params;
}

static struct ellc_ast *
ellc_norm_lam(struct ellc_st *st, struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 3);
    struct ell_obj *params_stx = ELL_SEND(stx_lst, second);
    ell_assert_brand(params_stx, ELL_BRAND(stx_lst));
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_LAM);
    struct ellc_contour *c = (struct ellc_contour *) ell_alloc(sizeof(*c));
    c->lam = &ast->lam;
    c->up = st->bottom_contour;
    st->bottom_contour = c;

    /* We have to defer normalization of parameter init forms until
       after all parameters have been seen, and have been added to the
       current lambda, so that local variable references work correctly. */
    dict_t *deferred_inits = ell_util_make_dict((dict_comp_t) &ell_ptr_cmp); // param -> init_stx

    ast->lam.params = ellc_dissect_params(st, ell_stx_lst_elts(params_stx), deferred_inits);

    for (dnode_t *dn = dict_first(deferred_inits); dn; dn = dict_next(deferred_inits, dn)) {
        struct ellc_param *param = (struct ellc_param *) dnode_getkey(dn);
        struct ell_obj *init_stx = (struct ell_obj *) dnode_get(dn);
        param->init = ellc_norm_stx(st, init_stx);
    }

    ast->lam.body = ellc_norm_stx(st, ELL_SEND(stx_lst, third));
    ast->lam.env = ell_util_make_dict((dict_comp_t) &ellc_id_cmp); // unused during norm.
    st->bottom_contour = c->up;
    return ast;
}

static struct ellc_ast *
ellc_norm_quote(struct ellc_st *st, struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 2);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_LIT_SYM);
    ast->lit_sym.sym = ell_stx_sym_sym(ELL_SEND(stx_lst, second));
    return ast;
}

static struct ellc_ast *
ellc_norm_loop(struct ellc_st *st, struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 2);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_LOOP);
    ast->loop.body = ellc_norm_stx(st, ELL_SEND(stx_lst, second));
    return ast;
}

/* (Quasisyntax) */

static struct ellc_ast *
ellc_norm_qs(struct ellc_st *st, struct ell_obj *arg_stx, unsigned depth);

static struct ellc_ast *
ellc_build_syntax(struct ellc_st *st, struct ell_obj *stx)
{
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_LIT_STX);
    if (!((stx->brand == ELL_BRAND(stx_sym))
          || (stx->brand == ELL_BRAND(stx_str)))) {
        printf("can't build syntax AST from non-syntax object\n");
        exit(EXIT_FAILURE);
    }
    ast->lit_stx.stx = stx;
    return ast;
}

static struct ellc_ast *
ellc_build_syntax_list(struct ellc_st *st, list_t *asts)
{
    struct ellc_args *args = ellc_make_args();
    list_transfer(&args->pos, asts, list_first(asts));
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_APP);
    ast->app.op = ellc_make_ref(st, ell_make_stx_sym(ELL_SYM(core_syntax_list)), ELLC_NS_FUN);
    ast->app.args = args;
    return ast;
}

static struct ellc_ast *
ellc_build_append_syntax_lists(struct ellc_st *st, list_t *asts)
{
    struct ellc_args *args = ellc_make_args();
    list_transfer(&args->pos, asts, list_first(asts));
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_APP);
    ast->app.op = ellc_make_ref(st, ell_make_stx_sym(ELL_SYM(core_append_syntax_lists)), ELLC_NS_FUN);
    ast->app.args = args;
    return ast;
}

static struct ellc_ast *
ellc_build_quasisyntax(struct ellc_st *st, struct ell_obj *stx, unsigned depth)
{
    list_t *asts = ell_util_make_list();
    ell_util_list_add(asts, ellc_build_syntax(st, ell_make_stx_sym(ELL_SYM(core_quasisyntax))));
    ell_util_list_add(asts, ellc_norm_qs(st, stx, depth));
    return ellc_build_syntax_list(st, asts);
}

static struct ellc_ast *
ellc_build_unsyntax(struct ellc_st *st, struct ell_obj *stx, unsigned depth)
{
    list_t *asts = ell_util_make_list();
    ell_util_list_add(asts, ellc_build_syntax(st, ell_make_stx_sym(ELL_SYM(core_unsyntax))));
    ell_util_list_add(asts, ellc_norm_qs(st, stx, depth));
    return ellc_build_syntax_list(st, asts);
}

static struct ellc_ast *
ellc_build_unsyntax_splicing(struct ellc_st *st, struct ell_obj *stx, unsigned depth)
{
    list_t *asts = ell_util_make_list();
    ell_util_list_add(asts, ellc_build_syntax(st, ell_make_stx_sym(ELL_SYM(core_unsyntax_splicing))));
    ell_util_list_add(asts, ellc_norm_qs(st, stx, depth));
    return ellc_build_syntax_list(st, asts);
}

static bool
ellc_is_unsyntax_splicing_list(struct ellc_st *st, struct ell_obj *stx)
{
    if (stx->brand != ELL_BRAND(stx_lst)) return 0;
    if (ell_stx_lst_len(stx) != 2) return 0;
    struct ell_obj *op_stx = ELL_SEND(stx, first);
    return ((op_stx->brand == ELL_BRAND(stx_sym))
            && (ell_stx_sym_sym(op_stx) == ELL_SYM(core_unsyntax_splicing)));
}

static bool
ellc_is_unsyntax(struct ellc_st *st, struct ell_obj *op_stx)
{
    return ((op_stx->brand == ELL_BRAND(stx_sym))
            && (ell_stx_sym_sym(op_stx) == ELL_SYM(core_unsyntax)));
}

static bool
ellc_is_quasisyntax(struct ellc_st *st, struct ell_obj *op_stx)
{
    return ((op_stx->brand == ELL_BRAND(stx_sym))
            && (ell_stx_sym_sym(op_stx) == ELL_SYM(core_quasisyntax)));
}

/* Unfortunately, this code is very hard to understand.  If you want
   to make sense of it, first understand Alan Bawden's implementation
   in appendix B of his paper "Quasiquotation in Lisp".  Ha. */

static struct ellc_ast *
ellc_norm_qs_lst_helper(struct ellc_st *st, struct ell_obj *stx_lst, unsigned depth)
{
    list_t *in_elts = ell_stx_lst_elts(stx_lst);
    list_t *lsts = ell_util_make_list();
    list_t *cur_elts = ell_util_make_list();
    
    for (lnode_t *n = list_first(in_elts); n; n = list_next(in_elts, n)) {
        struct ell_obj *sub = (struct ell_obj *) lnode_get(n);
        if (ellc_is_unsyntax_splicing_list(st, sub)) {
            if (list_count(cur_elts) > 0) {
                ell_util_list_add(lsts, ellc_build_syntax_list(st, cur_elts));
                cur_elts = ell_util_make_list();
            }
            if (depth == 0) {
                ell_util_list_add(lsts, ellc_norm_stx(st, ELL_SEND(sub, second)));
            } else {
                ell_util_list_add(lsts, ellc_build_unsyntax_splicing(st, ELL_SEND(sub, second), depth - 1));
            }
        } else {
            ell_util_list_add(cur_elts, ellc_norm_qs(st, sub, depth));
        }
    }
    if (list_count(cur_elts) > 0)
        ell_util_list_add(lsts, ellc_build_syntax_list(st, cur_elts));
    return ellc_build_append_syntax_lists(st, lsts);
}

static struct ellc_ast *
ellc_norm_qs_lst(struct ellc_st *st, struct ell_obj *stx_lst, unsigned depth)
{
    if (ell_stx_lst_len(stx_lst) == 0) {
        return ellc_build_syntax_list(st, ell_stx_lst_elts(stx_lst));
    } else {
        struct ell_obj *op_stx = ELL_SEND(stx_lst, first);
        if (ellc_is_unsyntax(st, op_stx)) {
            ell_assert_stx_lst_len(stx_lst, 2);
            struct ell_obj *arg_stx = ELL_SEND(stx_lst, second);
            if (depth == 0) {
                return ellc_norm_stx(st, arg_stx);
            } else {
                return ellc_build_unsyntax(st, arg_stx, depth - 1);
            }
        } else if (ellc_is_quasisyntax(st, op_stx)) {
            ell_assert_stx_lst_len(stx_lst, 2);
            struct ell_obj *arg_stx = ELL_SEND(stx_lst, second);
            return ellc_build_quasisyntax(st, arg_stx, depth + 1);
        } else {
            return ellc_norm_qs_lst_helper(st, stx_lst, depth);
        }
    }
}

static struct ellc_ast *
ellc_norm_qs(struct ellc_st *st, struct ell_obj *arg_stx, unsigned depth)
{
    if (depth < 0) {
        printf("negative quasiquotation depth\n");
        exit(EXIT_FAILURE);
    }

    if ((arg_stx->brand == ELL_BRAND(stx_str)) ||
        (arg_stx->brand == ELL_BRAND(stx_sym))) {
        return ellc_build_syntax(st, arg_stx);
    } else if (arg_stx->brand == ELL_BRAND(stx_lst)) {
        return ellc_norm_qs_lst(st, arg_stx, depth);
    } else {
        printf("bad quasiquoted syntax object\n");
        exit(EXIT_FAILURE);
    }
}

static struct ellc_ast *
ellc_norm_quasisyntax(struct ellc_st *st, struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 2);
    struct ell_obj *arg_stx = ELL_SEND(stx_lst, second);
    struct ellc_ast *body = ellc_norm_qs(st, arg_stx, 0);
    
    struct ellc_ast *cx_ast = ellc_make_ast(ELLC_AST_CX);
    cx_ast->cx.body = body;
    return cx_ast;
}

/* (Macroexpansion) */

static bool
ellc_is_seq(struct ell_obj *stx)
{
    if (stx->brand != ELL_BRAND(stx_lst)) return 0;
    if (list_count(ell_stx_lst_elts(stx)) < 2) return 0; // todo: handle better
    struct ell_obj *op_stx = ELL_SEND(stx, first);
    return ((op_stx->brand == ELL_BRAND(stx_sym))
            && (ell_stx_sym_sym(op_stx) == ELL_SYM(core_seq)));
}

static bool
ellc_is_mdef(struct ell_obj *stx)
{
    if (stx->brand != ELL_BRAND(stx_lst)) return 0;
    if (list_count(ell_stx_lst_elts(stx)) != 3) return 0; // todo: handle better
    struct ell_obj *op_stx = ELL_SEND(stx, first);
    return ((op_stx->brand == ELL_BRAND(stx_sym))
            && (ell_stx_sym_sym(op_stx) == ELL_SYM(core_mdef)));
}

static struct ellc_ast *
ellc_norm_mdef(struct ellc_st *st, struct ell_obj *mdef_stx)
{
    ell_assert_stx_lst_len(mdef_stx, 3);
    struct ell_obj *name_stx = ELL_SEND(mdef_stx, second);
    ell_assert_brand(name_stx, ELL_BRAND(stx_sym));
    struct ell_obj *expander_stx = ELL_SEND(mdef_stx, third);
    ell_util_dict_put(st->defined_macros, ell_stx_sym_sym(name_stx), expander_stx);
    // Right now, eval requires a syntax list as input, so we need to
    // wrap the expander expression in one.
    struct ell_obj *stx_lst = ell_make_stx_lst();
    ELL_SEND(stx_lst, add, expander_stx);
    ell_util_dict_put(&ellc_mac_tab, ell_stx_sym_sym(name_stx), ellc_eval(stx_lst));
    // Note that a macro definition is the only expression that has no
    // runtime effect.  This case is handled specially by 'ellc_norm'.
    return NULL;
}

/* (Putting it All Together) */

__attribute__((constructor(300))) static void
ellc_init()
{
    dict_init(&ellc_mac_tab, DICTCOUNT_T_MAX, (dict_comp_t) &ell_sym_cmp);
    dict_init(&ellc_norm_tab, DICTCOUNT_T_MAX, (dict_comp_t) &ell_sym_cmp);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_fref), &ellc_norm_fref);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_def), &ellc_norm_def);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_fdef), &ellc_norm_fdef);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_defp), &ellc_norm_defp);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_fdefp), &ellc_norm_fdefp);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_set), &ellc_norm_set);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_fset), &ellc_norm_fset);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_cond), &ellc_norm_cond);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_seq), &ellc_norm_seq);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_app), &ellc_norm_app);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_lam), &ellc_norm_lam);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_loop), &ellc_norm_loop);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_quote), &ellc_norm_quote);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_quasisyntax), &ellc_norm_quasisyntax);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_syntax), &ellc_norm_quasisyntax);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_mdef), &ellc_norm_mdef);
}

static struct ellc_ast *
ellc_norm_lst(struct ellc_st *st, struct ell_obj *stx_lst)
{
    ell_assert_brand(stx_lst, ELL_BRAND(stx_lst));
    struct ell_obj *op_stx = ELL_SEND(stx_lst, first);
    ell_assert_brand(op_stx, ELL_BRAND(stx_sym));
    struct ell_obj *op_sym = ell_stx_sym_sym(op_stx);
    struct ell_cx *cx = ell_stx_sym_cx(op_stx);
    struct ellc_id *id = ellc_make_id_cx(op_sym, ELLC_NS_FUN, cx);

    if (ellc_contour_lookup(st->bottom_contour, id, NULL)) {
        // operator is lexically fbound
        return ellc_norm_ordinary_app(st, stx_lst);
    } else {
        dnode_t *exp_node = dict_lookup(&ellc_mac_tab, op_sym);
        if (exp_node) {
            // operator is a macro
            struct ell_obj *expander = (struct ell_obj *) dnode_get(exp_node);
            struct ell_obj *expansion_stx = ELL_CALL(expander, stx_lst);
            return ellc_norm_stx(st, expansion_stx);
        } else {
            dnode_t *norm_node = dict_lookup(&ellc_norm_tab, op_sym);
            if (norm_node) {
                // operator is a special form
                ellc_norm_fun *norm_fun = (ellc_norm_fun *) dnode_get(norm_node);
                return norm_fun(st, stx_lst);
            } else {
                // operator is assumed to be global function
                return ellc_norm_ordinary_app(st, stx_lst);
            }
        }
    }
}

static struct ellc_ast *
ellc_norm_lit_str(struct ellc_st *st, struct ell_obj *stx)
{
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_LIT_STR);
    ast->lit_str.str = ell_stx_str_str(stx);
    return ast;
}

static struct ellc_ast *
ellc_norm_stx(struct ellc_st *st, struct ell_obj *stx)
{
    if (stx->brand == ELL_BRAND(stx_sym)) {
        return ellc_norm_ref(st, stx);
    } else if (stx->brand == ELL_BRAND(stx_lst)) {
        return ellc_norm_lst(st, stx);
    } else if (stx->brand == ELL_BRAND(stx_str)) {
        return ellc_norm_lit_str(st, stx);
    } else {
        printf("syntax normalization failure\n");
        exit(EXIT_FAILURE);
    }
}

static list_t *
ellc_norm_macro_pass(struct ellc_st *st, list_t *stx_elts, list_t *deferred)
{
    for (lnode_t *n = list_first(stx_elts); n; n = list_next(stx_elts, n)) {
        struct ell_obj *stx = (struct ell_obj *) lnode_get(n);
        if (ellc_is_seq(stx)) {
            ellc_norm_macro_pass(st, ell_util_sublist(ell_stx_lst_elts(stx), 1), deferred);
        } else if (ellc_is_mdef(stx)) {
            ellc_norm_mdef(st, stx);
        } else {
            ell_util_list_add(deferred, stx);
        }
    }
}

static struct ellc_ast_seq *
ellc_norm(struct ellc_st *st, struct ell_obj *stx_lst)
{
    ell_assert_brand(stx_lst, ELL_BRAND(stx_lst));
    list_t *deferred = ell_util_make_list();
    ellc_norm_macro_pass(st, ell_stx_lst_elts(stx_lst), deferred);

    struct ellc_ast_seq *ast_seq = ellc_make_ast_seq();
    for (lnode_t *n = list_first(deferred); n; n = list_next(deferred, n)) {
        struct ell_obj *stx = (struct ell_obj *) lnode_get(n);
        struct ellc_ast *res = ellc_norm_stx(st, stx);
        if (res) // check for mdef... could be done better?
            ellc_ast_seq_add(ast_seq, res);
    }
    return ast_seq;
}

/**** Closure Conversion ****/

static void
ellc_conv_ast(struct ellc_st *st, struct ellc_ast *ast);

static bool
ellc_defined_at_toplevel(struct ellc_st *st, struct ellc_id *id)
{
    return ell_util_list_contains(st->defined_globals, id, (dict_comp_t) &ellc_id_cmp);
}

static void
ellc_env_add_ref(struct ellc_ast_lam *lam, struct ellc_id *id)
{
    if (!dict_lookup(lam->env, id)) {
        struct ellc_ast *ref = ellc_make_ast(ELLC_AST_REF);
        ref->ref.id = id;
        ell_util_dict_put(lam->env, id, ref);
    }
}

static void
ellc_conv_ref(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_param *p = NULL;
    struct ellc_contour *c = ellc_contour_lookup(st->bottom_contour, ast->ref.id, &p);
    if (!c) {
        /* The identifier isn't lexically bound.  Now, we still need
           to check whether it's defined at the top-level in the
           current unit, before we follow the rule that all variables
           are considered implicitly bound at the top-level.  For such
           implicitly bound variables we need to ignore the hygiene
           context. */
        if (!ellc_defined_at_toplevel(st, ast->ref.id)) {
            ast->ref.id->cx = NULL;
        }
        struct ellc_id *tmp_id = ast->ref.id;
        ast->type = ELLC_AST_GLO_REF;
        ast->glo_ref.id = tmp_id;
        ell_util_set_add(st->globals, tmp_id, (dict_comp_t) &ellc_id_cmp);
    } else if (c == st->bottom_contour) {
        ast->type = ELLC_AST_ARG_REF;
        ast->arg_ref.param = p;
    } else {
        ast->type = ELLC_AST_ENV_REF;
        ast->env_ref.param = p;
        p->closed = 1;
        ellc_env_add_ref(st->bottom_contour->lam, p->id);
    }
}

static void
ellc_conv_def(struct ellc_st *st, struct ellc_ast *ast)
{
    ell_util_set_add(st->globals, ast->def.id, (dict_comp_t) &ellc_id_cmp);
    ellc_conv_ast(st, ast->def.val);
}

static void
ellc_conv_defp(struct ellc_st *st, struct ellc_ast *ast)
{
    /* See comment in ellc_conv_ref. */
    if (!ellc_defined_at_toplevel(st, ast->defp.id)) {
        ast->defp.id->cx = NULL;
    }
    ell_util_set_add(st->globals, ast->defp.id, (dict_comp_t) &ellc_id_cmp);
}

static void
ellc_conv_set(struct ellc_st *st, struct ellc_ast *ast)
{
    ellc_conv_ast(st, ast->set.val);
    struct ellc_param *p;
    struct ellc_contour *c = ellc_contour_lookup(st->bottom_contour, ast->set.id, &p);
    if (!c) {
        /* See comment in ellc_conv_ref. */
        if (!ellc_defined_at_toplevel(st, ast->ref.id)) {
            ast->set.id->cx = NULL;
        }
        struct ellc_id *tmp_id = ast->set.id;
        ast->type = ELLC_AST_GLO_SET;
        ast->glo_set.id = tmp_id;
        ell_util_set_add(st->globals, tmp_id, (dict_comp_t) &ellc_id_cmp);
    } else if (c == st->bottom_contour) {
        struct ellc_ast *tmp_val = ast->set.val;
        ast->type = ELLC_AST_ARG_SET;
        ast->arg_set.param = p;
        ast->arg_set.val = tmp_val;
        p->mutable = 1;
    } else {
        struct ellc_ast *tmp_val = ast->set.val;
        ast->type = ELLC_AST_ENV_SET;
        ast->env_set.param = p;
        ast->env_set.val = tmp_val;
        p->closed = 1;
        p->mutable = 1;
        ellc_env_add_ref(st->bottom_contour->lam, p->id);
    }
}

static void
ellc_conv_cond(struct ellc_st *st, struct ellc_ast *ast)
{
    ellc_conv_ast(st, ast->cond.test);
    ellc_conv_ast(st, ast->cond.consequent);
    ellc_conv_ast(st, ast->cond.alternative);
}

static void
ellc_conv_seq(struct ellc_st *st, struct ellc_ast *ast)
{
    for (lnode_t *n = list_first(ast->seq.exprs); n; n = list_next(ast->seq.exprs, n))
        ellc_conv_ast(st, (struct ellc_ast *) lnode_get(n));
}

static void
ellc_conv_args(struct ellc_st *st, struct ellc_args *args)
{
    for (lnode_t *n = list_first(&args->pos); n; n = list_next(&args->pos, n))
        ellc_conv_ast(st, (struct ellc_ast *) lnode_get(n));
    for (dnode_t *n = dict_first(&args->key); n; n = dict_next(&args->key, n))
        ellc_conv_ast(st, (struct ellc_ast *) dnode_get(n));
}

static void
ellc_conv_app(struct ellc_st *st, struct ellc_ast *ast)
{
    ellc_conv_ast(st, ast->app.op);
    ellc_conv_args(st, ast->app.args);
}

static void
ellc_conv_params_list_inits(struct ellc_st *st, list_t *params)
{
    for (lnode_t *n = list_first(params); n; n = list_next(params, n)) {
        struct ellc_param *p = (struct ellc_param *) lnode_get(n);
        if (p->init)
            ellc_conv_ast(st, p->init);
    }
}

static void
ellc_conv_param_inits(struct ellc_st *st, struct ellc_params *params)
{
    ellc_conv_params_list_inits(st, params->opt);
    ellc_conv_params_list_inits(st, params->key);
}

static void
ellc_conv_lam(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_contour *c = (struct ellc_contour *) ell_alloc(sizeof(*c));
    c->lam = &ast->lam;
    c->up = st->bottom_contour;
    st->bottom_contour = c;
    ellc_conv_param_inits(st, ast->lam.params);
    ellc_conv_ast(st, ast->lam.body);
    st->bottom_contour = c->up;
    for (dnode_t *n = dict_first(ast->lam.env); n; n = dict_next(ast->lam.env, n))
        ellc_conv_ast(st, (struct ellc_ast *) dnode_get(n));
    ast->lam.code_id = list_count(st->lambdas);
    ell_util_list_add(st->lambdas, &ast->lam);
}

static void
ellc_conv_loop(struct ellc_st *st, struct ellc_ast *ast)
{
    ellc_conv_ast(st, ast->loop.body);
}

static void
ellc_conv_cx(struct ellc_st *st, struct ellc_ast *ast)
{
    ellc_conv_ast(st, ast->cx.body);
}

static void
ellc_conv_ast(struct ellc_st *st, struct ellc_ast *ast)
{
    switch(ast->type) {
    case ELLC_AST_REF: ellc_conv_ref(st, ast); break;
    case ELLC_AST_DEF: ellc_conv_def(st, ast); break;
    case ELLC_AST_DEFP: ellc_conv_defp(st, ast); break;
    case ELLC_AST_SET: ellc_conv_set(st, ast); break;
    case ELLC_AST_COND: ellc_conv_cond(st, ast); break;
    case ELLC_AST_SEQ: ellc_conv_seq(st, ast); break;
    case ELLC_AST_APP: ellc_conv_app(st, ast); break;
    case ELLC_AST_LAM: ellc_conv_lam(st, ast); break;
    case ELLC_AST_LOOP: ellc_conv_loop(st, ast); break;
    case ELLC_AST_LIT_SYM: break;
    case ELLC_AST_LIT_STR: break;
    case ELLC_AST_LIT_STX: break;
    case ELLC_AST_CX: ellc_conv_cx(st, ast); break;
    default:
        printf("conversion error: %d\n", ast->type);
        exit(EXIT_FAILURE);
    }
}

static void
ellc_conv(struct ellc_st *st, struct ellc_ast_seq *ast_seq)
{
    if (st->bottom_contour != NULL) {
        printf("contour tracking bug or error in compilation unit\n");
        exit(EXIT_FAILURE);
    }
    for (lnode_t *n = list_first(ast_seq->exprs); n; n = list_next(ast_seq->exprs, n))
        ellc_conv_ast(st, (struct ellc_ast *) lnode_get(n));
}

/**** Emission ****/

static void
ellc_emit_ast(struct ellc_st *st, struct ellc_ast *ast);

static char
ellc_mangle_char(char c)
{
    // Needs to be kept in sync with sym-char in 'grammar.leg'.
    switch (c) {
    case '&': return 'A';
    case ':': return 'S';
    case '_': return 'U';
    case '-': return 'D';
    case '#': return 'O';
    case '/': return 'F';
    case '<': return 'L';
    case '>': return 'G';
    case '*': return 'Z';
    default: return c;
    }
}

static char *
ellc_mangle_str(char *s)
{
    size_t len = strlen(s);
    char *out = (char *) ell_alloc(len + 1);
    for (int i = 0; i < len; i++) {
        out[i] = ellc_mangle_char(s[i]);
    }
    out[len] = '\0';
    return out;
}

static char *ELLC_NO_CX = "";

static char *
ellc_mangle_cx(struct ell_cx *cx)
{
    if (cx->uuid != NULL) {
        char *out = (char *) ell_alloc(37);
        uuid_unparse(cx->uuid, out);
        return ellc_mangle_str(out);
    } else {
        return ELLC_NO_CX;
    }
}

static char *
ellc_mangle_id(char *prefix, struct ellc_id *id)
{
    char *std = "__ell";
    char *name = ellc_mangle_str(ell_str_chars(ell_sym_name(id->sym)));
    char *cx = ellc_mangle_cx(id->cx);
    size_t prefix_len = strlen(prefix);
    size_t std_len = strlen(std);
    size_t name_len = strlen(name);
    size_t cx_len = strlen(cx);
    size_t len = std_len + prefix_len + name_len + cx_len
        + 4  // separators
        + 1  // ns (single digit, lest this become a Lisp-10)
        + 1; // zero
    char *out = (char *) ell_alloc(len);
    snprintf(out, len, "%s_%s_%s_%u_%s", std, prefix, name, id->ns, cx);
    return out;
}

static char *
ellc_mangle_glo_id(struct ellc_id *id)
{
    return ellc_mangle_id("g", id);
}

static char *
ellc_mangle_param_id(struct ellc_id *id)
{
    return ellc_mangle_id("p", id);
}

static char *
ellc_mangle_env_id(struct ellc_id *id)
{
    return ellc_mangle_id("e", id);
}

static void
ellc_emit_glo_ref(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_id *id = ast->glo_ref.id;
    char *sid = ell_str_chars(ell_sym_name(id->sym));
    char *mid = ellc_mangle_glo_id(id);
    switch(id->ns) {
    case ELLC_NS_VAR:
        fprintf(st->f, "(%s != ell_unbound ? %s : ell_unbound_var(\"%s\"))", mid, mid, sid);
        break;
    case ELLC_NS_FUN:
        fprintf(st->f, "(%s != ell_unbound ? %s : ell_unbound_fun(\"%s\"))", mid, mid, sid);
        break;
    default:
        printf("unknown namespace\n");
        exit(EXIT_FAILURE);
    }
}

static void
ellc_emit_arg_ref_plain(struct ellc_st *st, struct ellc_ast *ast)
{
    fprintf(st->f, "%s", ellc_mangle_param_id(ast->arg_ref.param->id));
}

static void
ellc_emit_env_ref_plain(struct ellc_st *st, struct ellc_ast *ast)
{
    fprintf(st->f, "(__ell_env->%s)", ellc_mangle_env_id(ast->env_ref.param->id));
}

static void
ellc_emit_arg_ref(struct ellc_st *st, struct ellc_ast *ast)
{
    if (ellc_param_boxed(ast->arg_ref.param)) {
        fprintf(st->f, "ell_box_read(%s)", ellc_mangle_param_id(ast->arg_ref.param->id));
    } else {
        ellc_emit_arg_ref_plain(st, ast);
    }
}

static void
ellc_emit_env_ref(struct ellc_st *st, struct ellc_ast *ast)
{
    if (ellc_param_boxed(ast->env_ref.param)) {
        fprintf(st->f, "ell_box_read(__ell_env->%s)", ellc_mangle_env_id(ast->env_ref.param->id));
    } else {
        ellc_emit_env_ref_plain(st, ast);
    }
}

static void
ellc_emit_def(struct ellc_st *st, struct ellc_ast *ast)
{
    fprintf(st->f, "(%s = ", ellc_mangle_glo_id(ast->def.id));
    ellc_emit_ast(st, ast->def.val);
    fprintf(st->f, ")");
}

static void
ellc_emit_defp(struct ellc_st *st, struct ellc_ast *ast)
{
    fprintf(st->f, "(%s != ell_unbound ? ell_t : ell_f)", ellc_mangle_glo_id(ast->defp.id));
}

static void
ellc_emit_glo_set(struct ellc_st *st, struct ellc_ast *ast)
{
    char *mid = ellc_mangle_glo_id(ast->glo_set.id);
    fprintf(st->f, "({ if (%s == ell_unbound) ell_unbound_var(\"%s\"); %s = ", mid, mid, mid);
    ellc_emit_ast(st, ast->glo_set.val);
    fprintf(st->f, "; })");
}

static void
ellc_emit_arg_set(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_ast_arg_set *arg_set = &ast->arg_set;
    if (ellc_param_boxed(arg_set->param)) {
        fprintf(st->f, "(ell_box_write(%s, ", ellc_mangle_param_id(arg_set->param->id));
        ellc_emit_ast(st, arg_set->val);
        fprintf(st->f, "))");
    } else {
        fprintf(st->f, "(%s = ", ellc_mangle_param_id(arg_set->param->id));
        ellc_emit_ast(st, arg_set->val);
        fprintf(st->f, ")");
    }
}

static void
ellc_emit_env_set(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_ast_env_set *env_set = &ast->env_set;
    if (ellc_param_boxed(env_set->param)) {
        fprintf(st->f, "(ell_box_write(__ell_env->%s, ", ellc_mangle_env_id(env_set->param->id));
        ellc_emit_ast(st, env_set->val);
        fprintf(st->f, "))");
    } else {
        fprintf(st->f, "(__ell_env->%s = ", ellc_mangle_env_id(env_set->param->id));
        ellc_emit_ast(st, env_set->val);
        fprintf(st->f, ")");
    }
}

static void
ellc_emit_cond(struct ellc_st *st, struct ellc_ast *ast)
{
    fprintf(st->f, "(ell_is_true(");
    ellc_emit_ast(st, ast->cond.test);
    fprintf(st->f, ")) ? (");
    ellc_emit_ast(st, ast->cond.consequent);
    fprintf(st->f, ") : (");
    ellc_emit_ast(st, ast->cond.alternative);
    fprintf(st->f, ")");
}

static void
ellc_emit_seq(struct ellc_st *st, struct ellc_ast *ast)
{
    fprintf(st->f, "({");
    for (lnode_t *n = list_first(ast->seq.exprs); n; n = list_next(ast->seq.exprs, n)) {
        ellc_emit_ast(st, (struct ellc_ast *) lnode_get(n));
        fprintf(st->f, "; ");
    }
    fprintf(st->f, "})");
}

static void
ellc_emit_app(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_ast_app *app = &ast->app;
    listcount_t npos = list_count(&app->args->pos);
    dictcount_t nkey = dict_count(&app->args->key);
    fprintf(st->f, "({");
    if (npos || nkey) {
        fprintf(st->f, "struct ell_obj *__ell_args[] = {");
        for (lnode_t *n = list_first(&app->args->pos); n; n = list_next(&app->args->pos, n)) {
            struct ellc_ast *arg_ast = (struct ellc_ast *) lnode_get(n);
            ellc_emit_ast(st, arg_ast);
            fprintf(st->f, ", ");
        }
        for (dnode_t *n = dict_first(&app->args->key); n; n = dict_next(&app->args->key, n)) {
            struct ell_obj *arg_key_sym = (struct ell_obj *) dnode_getkey(n);
            struct ellc_ast *arg_ast = (struct ellc_ast *) dnode_get(n);
            fprintf(st->f, "ell_intern(ell_make_str(\"%s\"))", ell_str_chars(ell_sym_name(arg_key_sym)));
            fprintf(st->f, ", ");
            ellc_emit_ast(st, arg_ast);
            fprintf(st->f, ", ");
        }
        fprintf(st->f, "}; ");
    }
    fprintf(st->f, "ell_call(");
    ellc_emit_ast(st, app->op);
    fprintf(st->f, ", %lu, %lu, %s);", npos, nkey, ((npos || nkey) ? "__ell_args" : "NULL"));
    fprintf(st->f, "})");
}

static void
ellc_emit_lam(struct ellc_st *st, struct ellc_ast *ast)
{
    /* Inside a lambda, the enclosing hygiene context is not visible,
       because it's a C local variable.  Thus, setting it to off
       inside the lambda's body is needed so that the code does the
       right thing, namely, generate a new context when a new
       quasisyntax is encountered. */
    bool in_quasisyntax_tmp = st->in_quasisyntax;
    st->in_quasisyntax = 0;

    struct ellc_ast_lam *lam = &ast->lam;
    fprintf(st->f, "({");
    // populate env
    if (dict_count(lam->env) > 0) {
        fprintf(st->f, "struct __ell_env_%u *__lam_env = ell_alloc(sizeof(struct __ell_env_%u));",
               lam->code_id, lam->code_id);
        for (dnode_t *n = dict_first(lam->env); n; n = dict_next(lam->env, n)) {
            struct ellc_id *env_id = (struct ellc_id *) dnode_getkey(n);
            fprintf(st->f, "__lam_env->%s = ", ellc_mangle_env_id(env_id));
            struct ellc_ast *ref_ast = (struct ellc_ast *) dnode_get(n);
            /* Tricky: if a variable is boxed, the closure environment
               needs to contain the box, not the box's contents.  This
               means we need to emit references specially here, so
               that they always act as if the variable was unboxed,
               even for boxed ones. */
            switch(ref_ast->type) {
            case ELLC_AST_ENV_REF:
                ellc_emit_env_ref_plain(st, ref_ast); break;
            case ELLC_AST_ARG_REF:
                ellc_emit_arg_ref_plain(st, ref_ast); break;
            default:
                printf("bad closure environment reference\n");
                exit(EXIT_FAILURE);
            }
            fprintf(st->f, "; ");
        }
    }
    // create closure
    if (dict_count(lam->env) > 0) {
        fprintf(st->f, "ell_make_clo(&__ell_code_%u, __lam_env);", lam->code_id);
    } else {
        fprintf(st->f, "ell_make_clo(&__ell_code_%u, NULL);", lam->code_id);
    }
    fprintf(st->f, "})");

    st->in_quasisyntax = in_quasisyntax_tmp;
}

static void
ellc_emit_loop(struct ellc_st *st, struct ellc_ast *ast)
{
    fprintf(st->f, "({ for(;;) {");
    ellc_emit_ast(st, ast->loop.body);
    fprintf(st->f, "; } ell_unspecified; })");
}

static void
ellc_emit_lit_sym(struct ellc_st *st, struct ellc_ast *ast)
{
    fprintf(st->f, "ell_intern(ell_make_str(\"%s\"))", ell_str_chars(ell_sym_name(ast->lit_sym.sym)));
}

static void
ellc_emit_lit_str(struct ellc_st *st, struct ellc_ast *ast)
{
    fprintf(st->f, "ell_make_str(\"%s\")", ell_str_chars(ast->lit_str.str));
}

static void
ellc_emit_lit_stx(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ell_obj *stx = ast->lit_stx.stx;
    if (stx->brand == ELL_BRAND(stx_sym)) {
        fprintf(st->f, "ell_make_stx_sym_cx(ell_intern(ell_make_str(\"%s\")), __ell_cur_cx)",
                ell_str_chars(ell_sym_name(ell_stx_sym_sym(stx))));
    } else if (stx->brand == ELL_BRAND(stx_str)) {
        fprintf(st->f, "ell_make_stx_str(ell_make_str(\"%s\"))", 
                ell_str_chars(ell_stx_str_str(stx)));
    } else {
        printf("literal syntax error\n");
        exit(EXIT_FAILURE);
    }
}

static void
ellc_emit_cx(struct ellc_st *st, struct ellc_ast *ast)
{
    if (st->in_quasisyntax) {
        ellc_emit_ast(st, ast->cx.body);
    } else {
        /* Shadow the global current hygiene context, which is always
           NULL.  The trick here is that only syntax forms that are
           statically enclosed in this form will pick up this new
           context, that's shadowing the global context, since the new
           context is a C local variable. */
        st->in_quasisyntax = 1;
        fprintf(st->f, "({ struct ell_cx *__ell_cur_cx = ell_make_cx(); ");
        ellc_emit_ast(st, ast->cx.body);
        fprintf(st->f, "; })");
        st->in_quasisyntax = 0;
    }
}

static void
ellc_emit_ast(struct ellc_st *st, struct ellc_ast *ast)
{
    switch(ast->type) {
    case ELLC_AST_GLO_REF: ellc_emit_glo_ref(st, ast); break;
    case ELLC_AST_ARG_REF: ellc_emit_arg_ref(st, ast); break;
    case ELLC_AST_ENV_REF: ellc_emit_env_ref(st, ast); break;
    case ELLC_AST_DEF: ellc_emit_def(st, ast); break;
    case ELLC_AST_DEFP: ellc_emit_defp(st, ast); break;
    case ELLC_AST_GLO_SET: ellc_emit_glo_set(st, ast); break;
    case ELLC_AST_ARG_SET: ellc_emit_arg_set(st, ast); break;
    case ELLC_AST_ENV_SET: ellc_emit_env_set(st, ast); break;
    case ELLC_AST_COND: ellc_emit_cond(st, ast); break;
    case ELLC_AST_SEQ: ellc_emit_seq(st, ast); break;
    case ELLC_AST_APP: ellc_emit_app(st, ast); break;
    case ELLC_AST_LAM: ellc_emit_lam(st, ast); break;
    case ELLC_AST_LOOP: ellc_emit_loop(st, ast); break;
    case ELLC_AST_LIT_SYM: ellc_emit_lit_sym(st, ast); break;
    case ELLC_AST_LIT_STR: ellc_emit_lit_str(st, ast); break;
    case ELLC_AST_LIT_STX: ellc_emit_lit_stx(st, ast); break;
    case ELLC_AST_CX: ellc_emit_cx(st, ast); break;
    default:
        printf("emission error\n");
        exit(EXIT_FAILURE);
    }
}

static void
ellc_emit_globals_declarations(struct ellc_st *st)
{
    fprintf(st->f, "// GLOBALS\n");
    for (lnode_t *n = list_first(st->globals); n; n = list_next(st->globals, n)) {
        struct ellc_id *id = (struct ellc_id *) lnode_get(n);
        fprintf(st->f, "__attribute__((weak)) struct ell_obj *%s;\n", ellc_mangle_glo_id(id));
    }
    fprintf(st->f, "\n");
}

static void
ellc_emit_globals_initializations(struct ellc_st *st)
{
    for (lnode_t *n = list_first(st->globals); n; n = list_next(st->globals, n)) {
        struct ellc_id *id = (struct ellc_id *) lnode_get(n);
        char *mid = ellc_mangle_glo_id(id);
        fprintf(st->f, "if (%s == NULL) %s = ell_unbound;\n", mid, mid);
    }
    fprintf(st->f, "\n");
}

static void
ellc_emit_req_param_val(struct ellc_st *st, struct ellc_param *p, unsigned pos)
{
    if (ellc_param_boxed(p))
        fprintf(st->f, "ell_make_box(__ell_args[%u])", pos);
    else
        fprintf(st->f, "__ell_args[%u]", pos);
}

static void
ellc_emit_opt_param_val(struct ellc_st *st, struct ellc_param *p, unsigned i)
{
    if (ellc_param_boxed(p))
        fprintf(st->f, "__ell_npos > %u ? ell_make_box(__ell_args[%u]) : ", i, i);
    else
        fprintf(st->f, "__ell_npos > %u ? __ell_args[%u] : ", i, i);

    if (p->init)
        ellc_emit_ast(st, p->init);
    else
        fprintf(st->f, "ell_unbound");
}

static void
ellc_emit_key_param_val(struct ellc_st *st, struct ellc_param *p)
{
    // Construct a call to the lookup routine using the param's symbolic name
    fprintf(st->f, "({ struct ell_obj *__ell_key_val = ell_lookup_key(");
    fprintf(st->f, "ell_intern(ell_make_str(\"%s\"))", ell_str_chars(ell_sym_name(p->id->sym)));
    fprintf(st->f, ", __ell_npos, __ell_nkey, __ell_args);");

    if (ellc_param_boxed(p))
        fprintf(st->f, "__ell_key_val ? ell_make_box(__ell_key_val) : ");
    else
        fprintf(st->f, "__ell_key_val ? __ell_key_val : ");

    if (p->init)
        ellc_emit_ast(st, p->init);
    else
        fprintf(st->f, "ell_unbound");

    fprintf(st->f, "; })");
}

static void
ellc_emit_params(struct ellc_st *st, struct ellc_ast_lam *lam)
{
    listcount_t nreq = list_count(lam->params->req);
    listcount_t nopt = list_count(lam->params->opt);
    if (nreq > 0) {
        fprintf(st->f, "\tif (__ell_npos < %lu) { ell_arity_error(); }\n", nreq);
    }
    if (!lam->params->rest) {
        fprintf(st->f, "\tif (__ell_npos > %lu) { ell_arity_error(); }\n", nreq + nopt);
    }
    
    unsigned i = 0;

    // required
    for (lnode_t *n = list_first(lam->params->req); n; n = list_next(lam->params->req, n)) {
        struct ellc_param *p = (struct ellc_param *) lnode_get(n);
        fprintf(st->f, "\tvoid *%s = ", ellc_mangle_param_id(p->id));
        ellc_emit_req_param_val(st, p, i);
        fprintf(st->f, ";\n");
        i++;
    }

    // optional
    for (lnode_t *n = list_first(lam->params->opt); n; n = list_next(lam->params->opt, n)) {
        struct ellc_param *p = (struct ellc_param *) lnode_get(n);
        fprintf(st->f, "\tvoid *%s = ", ellc_mangle_param_id(p->id));
        ellc_emit_opt_param_val(st, p, i);
        fprintf(st->f, ";\n");
        i++;
    }

    // rest
    if (lam->params->rest) {
        struct ellc_param *rest = lam->params->rest;
        fprintf(st->f, "\tstruct ell_obj *__ell_rest_tmp = ell_make_lst();\n");;
        fprintf(st->f, "\tfor (int __ell_rest_i = %lu; __ell_rest_i < __ell_npos; __ell_rest_i++)\n",
                nreq + nopt);
        fprintf(st->f, "\t\tELL_SEND(__ell_rest_tmp, add, __ell_args[__ell_rest_i]);\n");
        char *mid = ellc_mangle_param_id(rest->id);
        if (ellc_param_boxed(rest)){
            fprintf(st->f, "\tvoid *%s = ell_make_box(__ell_rest_tmp);\n", mid);
        } else {
            fprintf(st->f, "\tvoid *%s = __ell_rest_tmp;\n", mid);
        }
    }

    // key
    for (lnode_t *n = list_first(lam->params->key); n; n = list_next(lam->params->key, n)) {
        struct ellc_param *p = (struct ellc_param *) lnode_get(n);
        fprintf(st->f, "\tvoid *%s = ", ellc_mangle_param_id(p->id));
        ellc_emit_key_param_val(st, p);
        fprintf(st->f, ";\n");
    }

    if (lam->params->all_keys) {
        printf("all-keys parameters not yet supported\n");
        exit(EXIT_FAILURE);
    }
}

static void
ellc_emit_codes(struct ellc_st *st)
{
    unsigned code_id = 0;
    for (lnode_t *n = list_first(st->lambdas); n; n = list_next(st->lambdas, n)) {
        struct ellc_ast_lam *lam = (struct ellc_ast_lam *) lnode_get(n);
        fprintf(st->f, "// CLOSURE %u\n", code_id);
        // env
        if (dict_count(lam->env) > 0) {
            fprintf(st->f, "struct __ell_env_%u {\n", code_id);
            for (dnode_t *en = dict_first(lam->env); en; en = dict_next(lam->env, en)) {
                struct ellc_id *env_id = (struct ellc_id *) dnode_getkey(en);
                fprintf(st->f, "\tvoid *%s;\n", ellc_mangle_env_id(env_id));
            }
            fprintf(st->f, "};\n");
        }
        // code
        fprintf(st->f, "static struct ell_obj *");
        fprintf(st->f, "__ell_code_%u(struct ell_obj *__ell_clo, unsigned __ell_npos, "
               "unsigned __ell_nkey, struct ell_obj **__ell_args) {\n", code_id);
        ellc_emit_params(st, lam);
        fprintf(st->f, "\tstruct __ell_env_%u *__ell_env = (struct __ell_env_%u *)"
               "((struct ell_clo_data *) __ell_clo->data)->env;\n", code_id, code_id);
        fprintf(st->f, "\treturn ");
        ellc_emit_ast(st, lam->body);
        fprintf(st->f, ";");
        fprintf(st->f, "\n}\n\n");

        code_id++;
    }
}

static void
ellc_emit(struct ellc_st *st, struct ellc_ast_seq *ast_seq)
{
    fprintf(st->f, "#include \"ellrt.h\"\n");
    ellc_emit_globals_declarations(st);
    ellc_emit_codes(st);
    fprintf(st->f, "// INIT\n");
    fprintf(st->f, "__attribute__((constructor(500))) static void ell_init() {\n");
    ellc_emit_globals_initializations(st);
    for (lnode_t *n = list_first(ast_seq->exprs); n; n = list_next(ast_seq->exprs, n)) {
        fprintf(st->f, "\tell_result = ");
        ellc_emit_ast(st, (struct ellc_ast *) lnode_get(n));
        fprintf(st->f, ";\n");
    }
    fprintf(st->f, "}\n");
}

/**** API ****/

static struct ellc_st *
ellc_make_st(FILE *f)
{
    struct ellc_st *st = (struct ellc_st *) ell_alloc(sizeof(*st));
    st->f = f;
    st->defined_globals = ell_util_make_list();
    st->defined_macros = ell_util_make_dict((dict_comp_t) &ell_sym_cmp);
    st->globals = ell_util_make_list();
    st->lambdas = ell_util_make_list();
    st->bottom_contour = NULL;
    return st;
}

/* Compiles a syntax list and returns the name of the (temporary) FASL
   file.  Also returns a pointer to the compiler state in the st_out
   parameter. */
static char *
ellc_compile(struct ell_obj *stx_lst, struct ellc_st **st_out)
{
    ell_assert_brand(stx_lst, ELL_BRAND(stx_lst));

    char *cnam = ell_alloc(L_tmpnam);
    if (!tmpnam(cnam)) {
        printf("cannot name temp file\n");
        exit(EXIT_FAILURE);
    }

    char *onam = ell_alloc(L_tmpnam);
    if (!tmpnam(onam)) {
        printf("cannot name temp file\n");
        exit(EXIT_FAILURE);
    }

    FILE *f = fopen(cnam, "w");
    if (!f) {
        printf("cannot open temp file\n");
        exit(EXIT_FAILURE);
    }
    
    struct ellc_st *st = ellc_make_st(f);
    struct ellc_ast_seq *ast_seq = ellc_norm(st, stx_lst);
    ellc_conv(st, ast_seq);
    ellc_emit(st, ast_seq);
    
    if (fclose(f) != 0) {
        printf("cannot close temp file\n");
        exit(EXIT_FAILURE);        
    }
    
    char cmdline[256];
    sprintf(cmdline, "gcc -pipe -std=c99 -shared -fPIC -I. -o %s -x c %s", onam, cnam);

    if (system(cmdline) == -1) {
        printf("error compiling file\n");
        exit(EXIT_FAILURE);
    }

    if (st_out)
        *st_out = st;
    return onam;
}

struct ell_obj *
ellc_eval(struct ell_obj *stx_lst)
{
    char *onam = ellc_compile(stx_lst, NULL);

    ell_result = NULL;

    if (!dlopen(onam, RTLD_NOW | RTLD_GLOBAL)) {
        printf("load error: %s\n", dlerror());
        exit(EXIT_FAILURE);
    }
    
    return ell_result;
}

int
ellc_compile_file(char *infile, char *faslfile, char *cfaslfile)
{
    freopen(infile, "r", stdin);

    struct ellc_st *st;
    struct ell_obj* stx_lst = ell_parse();
    char *tmp_fasl_name = ellc_compile(stx_lst, &st);

    // CROOK AHEAD
    struct ell_obj *macros_stx_lst = ell_make_stx_lst();
    for (dnode_t *n = dict_first(st->defined_macros); n; n = dict_next(st->defined_macros, n)) {
        struct ell_obj *name_sym = (struct ell_obj *) dnode_getkey(n);
        struct ell_obj *expander_stx = (struct ell_obj *) dnode_get(n);
        struct ell_obj *macro_stx = ell_make_stx_lst();
        ELL_SEND(macro_stx, add, ell_make_stx_sym(ell_intern(ell_make_str("compiler-put-expander"))));
        ELL_SEND(macro_stx, add, ell_make_stx_sym(name_sym));
        ELL_SEND(macro_stx, add, expander_stx);
        ELL_SEND(macros_stx_lst, add, macro_stx);
    }
    
    char *tmp_cfasl_name = ellc_compile(macros_stx_lst, NULL);
    
    rename(tmp_fasl_name, faslfile);
    rename(tmp_cfasl_name, cfaslfile);

    return 0;
}
