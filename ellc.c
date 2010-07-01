/***** Executable and Linkable Lisp Compiler *****/

#include "ellc.h"

/**** Parsing ****/

#include "grammar.c"

struct ellc_parser_stack {
    struct ellc_parser_stack *down;
    struct ell_obj *stx_lst;
};

static struct ellc_parser_stack *ellc_parser_stack_top;

static struct ell_obj *
ellc_parse()
{
    ellc_parser_stack_top = 
        (struct ellc_parser_stack *) ell_alloc(sizeof(*ellc_parser_stack_top));
    ellc_parser_stack_top->down = NULL;
    ellc_parser_stack_top->stx_lst = ell_make_stx_lst();
    while(yyparse())
        ;
    return ellc_parser_stack_top->stx_lst;
}

static void
ellc_parser_add_sym(char *chars)
{
    struct ell_obj *stx_sym = ell_make_stx_sym(ell_intern(ell_make_str(chars)));
    ELL_SEND(ellc_parser_stack_top->stx_lst, add, stx_sym);
}

static void
ellc_parser_push()
{
    struct ellc_parser_stack *new = 
        (struct ellc_parser_stack *) ell_alloc(sizeof(*new));
    struct ell_obj *new_stx_lst = ell_make_stx_lst();
    new->down = ellc_parser_stack_top;
    new->stx_lst = new_stx_lst;
    ELL_SEND(ellc_parser_stack_top->stx_lst, add, new_stx_lst);
    ellc_parser_stack_top = new;
}

static void
ellc_parser_pop()
{
    ellc_parser_stack_top = ellc_parser_stack_top->down;
}

/**** AST Utilities ****/

static struct ellc_id *
ellc_make_id(struct ell_obj *sym, enum ellc_ns ns)
{
    ell_assert_brand(sym, ELL_BRAND(sym));
    struct ellc_id *id = (struct ellc_id *) ell_alloc(sizeof(*id));
    id->sym = sym;
    id->ns = ns;
    return id;
}

static bool
ellc_id_equal(struct ellc_id *a, struct ellc_id *b)
{
    return (a->sym == b->sym) && (a->ns == b->ns);
}

static int
ellc_id_cmp(struct ellc_id *a, struct ellc_id *b)
{
    return ell_sym_cmp(a->sym, b->sym) || (a->ns - b->ns);
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

/**** Normalization: Syntax Objects -> AST ****/

static struct ellc_ast *
ellc_norm_stx(struct ell_obj *stx);

typedef struct ellc_ast *
(ellc_norm_fun)(struct ell_obj *stx_lst);

/* (Simple Forms) */

static struct ellc_ast *
ellc_make_ref(struct ell_obj *stx_id, enum ellc_ns ns)
{
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_REF);
    ast->ref.id = ellc_make_id(ell_stx_sym_sym(stx_id), ns);
    return ast;
}

static struct ellc_ast *
ellc_norm_ref(struct ell_obj *stx_sym)
{
    return ellc_make_ref(stx_sym, ELLC_NS_VAR);
}

static struct ellc_ast *
ellc_norm_fref(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 2);
    return ellc_make_ref(ELL_SEND(stx_lst, second), ELLC_NS_FUN);
}

static struct ellc_ast *
ellc_make_def(struct ell_obj *stx_lst, enum ellc_ns ns)
{
    ell_assert_stx_lst_len(stx_lst, 3);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_DEF);
    ast->def.id = ellc_make_id(ell_stx_sym_sym(ELL_SEND(stx_lst, second)), ns);
    ast->def.val = ellc_norm_stx(ELL_SEND(stx_lst, third));
    return ast;
}

static struct ellc_ast *
ellc_norm_def(struct ell_obj *stx_lst)
{
    return ellc_make_def(stx_lst, ELLC_NS_VAR);
}

static struct ellc_ast *
ellc_norm_fdef(struct ell_obj *stx_lst)
{
    return ellc_make_def(stx_lst, ELLC_NS_FUN);
}

static struct ellc_ast *
ellc_make_set(struct ell_obj *stx_lst, enum ellc_ns ns)
{
    ell_assert_stx_lst_len(stx_lst, 3);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_SET);
    ast->set.id = ellc_make_id(ell_stx_sym_sym(ELL_SEND(stx_lst, second)), ns);
    ast->set.val = ellc_norm_stx(ELL_SEND(stx_lst, third));
    return ast;    
}

static struct ellc_ast *
ellc_norm_set(struct ell_obj *stx_lst)
{
    return ellc_make_set(stx_lst, ELLC_NS_VAR);
}

static struct ellc_ast *
ellc_norm_fset(struct ell_obj *stx_lst)
{
    return ellc_make_set(stx_lst, ELLC_NS_FUN);
}

static struct ellc_ast *
ellc_norm_cond(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 4);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_COND);
    ast->cond.test = ellc_norm_stx(ELL_SEND(stx_lst, second));
    ast->cond.consequent = ellc_norm_stx(ELL_SEND(stx_lst, third));
    ast->cond.alternative = ellc_norm_stx(ELL_SEND(stx_lst, fourth));
    return ast;
}

static struct ellc_ast *
ellc_norm_seq(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len_min(stx_lst, 1);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_SEQ);
    ast->seq.exprs = ell_util_make_list();
    list_t *elts_stx = ell_util_sublist(ell_stx_lst_elts(stx_lst), 1);

    for (lnode_t *n = list_first(elts_stx); n; n = list_next(elts_stx, n)) {
        struct ell_obj *stx = (struct ell_obj *) lnode_get(n);
        ellc_ast_seq_add(&ast->seq, ellc_norm_stx(stx));
    }

    return ast;
}

/* (Application and Arguments Dissection) */

static bool
ellc_is_key_arg_sym(struct ell_obj *sym)
{
    ell_assert_brand(sym, ELL_BRAND(sym));
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
ellc_dissect_args(list_t *args_stx)
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
                ellc_norm_stx((struct ell_obj *) lnode_get(n));
            ell_util_dict_put(&args->key, key_arg_sym, key_arg_val_ast);
        } else {
            ell_util_list_add(&args->pos, ellc_norm_stx(arg_stx));
        }
    }
    return args;
}

static struct ellc_ast *
ellc_make_app(struct ellc_ast *op, list_t *arg_stx_lst)
{
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_APP);
    ast->app.op = op;
    ast->app.args = ellc_dissect_args(arg_stx_lst);
    return ast;
}

static struct ellc_ast *
ellc_norm_app(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len_min(stx_lst, 2);
    return ellc_make_app(ellc_norm_stx(ELL_SEND(stx_lst, second)),
                         ell_util_sublist(ell_stx_lst_elts(stx_lst), 2));
}

static struct ellc_ast *
ellc_norm_ordinary_app(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len_min(stx_lst, 1);
    struct ell_obj *op_sym_stx = ELL_SEND(stx_lst, first);
    ell_assert_brand(op_sym_stx, ELL_BRAND(stx_sym));
    struct ellc_ast *op_ast = ellc_make_ast(ELLC_AST_REF);
    op_ast->ref.id = ellc_make_id(ell_stx_sym_sym(op_sym_stx), ELLC_NS_FUN);
    return ellc_make_app(op_ast,
                         ell_util_sublist(ell_stx_lst_elts(stx_lst), 1));
}

/* (Abstraction and Parameters Dissection) */

static struct ellc_param *
ellc_dissect_param(struct ell_obj *p_stx)
{
    struct ellc_param *p = (struct ellc_param *) ell_alloc(sizeof(*p));
    if (p_stx->brand == ELL_BRAND(stx_sym)) {
        p->id = ellc_make_id(ell_stx_sym_sym(p_stx), ELLC_NS_VAR);
    } else if (p_stx->brand == ELL_BRAND(stx_lst)) {
        ell_assert_stx_lst_len(p_stx, 2);
        struct ell_obj *name_stx = ELL_SEND(p_stx, first);
        struct ell_obj *init_stx = ELL_SEND(p_stx, second);
        p->id = ellc_make_id(ell_stx_sym_sym(name_stx), ELLC_NS_VAR);
        p->init = ellc_norm_stx(init_stx);
    }
    return p;
}

static struct ellc_params *
ellc_dissect_params(list_t *params_stx)
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
        ell_util_list_add(cur, ellc_dissect_param(p_stx));
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
ellc_norm_lam(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 3);
    struct ell_obj *params_stx = ELL_SEND(stx_lst, second);
    ell_assert_brand(params_stx, ELL_BRAND(stx_lst));
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_LAM);
    ast->lam.params = ellc_dissect_params(ell_stx_lst_elts(params_stx));
    ast->lam.body = ellc_norm_stx(ELL_SEND(stx_lst, third));
    ast->lam.env = ell_util_make_dict((dict_comp_t) &ellc_id_cmp);
    return ast;
}

/* (Putting it All Together) */

static struct dict_t ellc_norm_tab;

__attribute__((constructor(301))) static void
ellc_init_norm_tab()
{
    dict_init(&ellc_norm_tab, DICTCOUNT_T_MAX, (dict_comp_t) &ell_sym_cmp);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_fref), &ellc_norm_fref);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_def), &ellc_norm_def);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_fdef), &ellc_norm_fdef);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_set), &ellc_norm_set);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_fset), &ellc_norm_fset);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_cond), &ellc_norm_cond);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_seq), &ellc_norm_seq);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_app), &ellc_norm_app);
    ell_util_dict_put(&ellc_norm_tab, ELL_SYM(core_lam), &ellc_norm_lam);
}

static struct ellc_ast *
ellc_norm_stx_lst(struct ell_obj *stx_lst)
{
    ell_assert_brand(stx_lst, ELL_BRAND(stx_lst));
    struct ell_obj *op_stx = ELL_SEND(stx_lst, first);
    ell_assert_brand(op_stx, ELL_BRAND(stx_sym));
    struct ell_obj *op_sym = ell_stx_sym_sym(op_stx);
    dnode_t *node = dict_lookup(&ellc_norm_tab, op_sym);
    if (node) {
        ellc_norm_fun *norm_fun = (ellc_norm_fun *) dnode_get(node);
        return norm_fun(stx_lst);
    } else {
        return ellc_norm_ordinary_app(stx_lst);
    }
}

static struct ellc_ast *
ellc_norm_stx(struct ell_obj *stx)
{
    if (stx->brand == ELL_BRAND(stx_sym)) {
        return ellc_norm_ref(stx);
    } else if (stx->brand == ELL_BRAND(stx_lst)) {
        return ellc_norm_stx_lst(stx);
    } else {
        printf("syntax normalization failure\n");
        exit(EXIT_FAILURE);
    }
}

static void
ellc_norm_process(list_t *list, lnode_t *node, void *ast_seq_arg)
{
    struct ellc_ast_seq *ast_seq = (struct ellc_ast_seq *) ast_seq_arg;
    struct ell_obj *stx = (struct ell_obj *) lnode_get(node);
    ellc_ast_seq_add(ast_seq, ellc_norm_stx(stx));
}

static struct ellc_ast_seq *
ellc_norm(struct ell_obj *stx_lst)
{
    ell_assert_brand(stx_lst, ELL_BRAND(stx_lst));
    struct ellc_ast_seq *ast_seq = ellc_make_ast_seq();
    list_process(ell_stx_lst_elts(stx_lst), ast_seq, &ellc_norm_process);
    return ast_seq;
}

/**** Explication: Closure Conversion ****/

static void
ellc_expl_ast(struct ellc_st *st, struct ellc_ast *ast);

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
        *out = p;
        return c;
    } else {
        return ellc_contour_lookup(c->up, id, out);
    }
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
ellc_expl_ref(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_param *p = NULL;
    struct ellc_contour *c = ellc_contour_lookup(st->bottom_contour, ast->ref.id, &p);
    if (!c) {
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
ellc_expl_def(struct ellc_st *st, struct ellc_ast *ast)
{
    ell_util_set_add(st->globals, ast->def.id, (dict_comp_t) &ellc_id_cmp);
    ellc_expl_ast(st, ast->def.val);
}

static void
ellc_expl_set(struct ellc_st *st, struct ellc_ast *ast)
{
    ellc_expl_ast(st, ast->set.val);
    struct ellc_param *p;
    struct ellc_contour *c = ellc_contour_lookup(st->bottom_contour, ast->set.id, &p);
    if (!c) {
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
ellc_expl_cond(struct ellc_st *st, struct ellc_ast *ast)
{
    ellc_expl_ast(st, ast->cond.test);
    ellc_expl_ast(st, ast->cond.consequent);
    ellc_expl_ast(st, ast->cond.alternative);
}

static void
ellc_expl_seq(struct ellc_st *st, struct ellc_ast *ast)
{
    for (lnode_t *n = list_first(ast->seq.exprs); n; n = list_next(ast->seq.exprs, n))
        ellc_expl_ast(st, (struct ellc_ast *) lnode_get(n));
}

static void
ellc_expl_args(struct ellc_st *st, struct ellc_args *args)
{
    for (lnode_t *n = list_first(&args->pos); n; n = list_next(&args->pos, n))
        ellc_expl_ast(st, (struct ellc_ast *) lnode_get(n));
    for (dnode_t *n = dict_first(&args->key); n; n = dict_next(&args->key, n))
        ellc_expl_ast(st, (struct ellc_ast *) dnode_get(n));
}

static void
ellc_expl_app(struct ellc_st *st, struct ellc_ast *ast)
{
    ellc_expl_ast(st, ast->app.op);
    ellc_expl_args(st, ast->app.args);
}

static void
ellc_expl_params_list_inits(struct ellc_st *st, list_t *params)
{
    for (lnode_t *n = list_first(params); n; n = list_next(params, n)) {
        struct ellc_param *p = (struct ellc_param *) lnode_get(n);
        if (p->init)
            ellc_expl_ast(st, p->init);
    }
}

static void
ellc_expl_param_inits(struct ellc_st *st, struct ellc_params *params)
{
    ellc_expl_params_list_inits(st, params->opt);
    ellc_expl_params_list_inits(st, params->key);
}

static void
ellc_expl_lam(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_contour *c = (struct ellc_contour *) ell_alloc(sizeof(*c));
    c->lam = &ast->lam;
    c->up = st->bottom_contour;
    st->bottom_contour = c;
    ellc_expl_param_inits(st, ast->lam.params);
    ellc_expl_ast(st, ast->lam.body);
    st->bottom_contour = c->up;
    for (dnode_t *n = dict_first(ast->lam.env); n; n = dict_next(ast->lam.env, n))
        ellc_expl_ast(st, (struct ellc_ast *) dnode_get(n));
    ast->lam.code_id = list_count(st->lambdas);
    ell_util_list_add(st->lambdas, &ast->lam);
}

static void
ellc_expl_ast(struct ellc_st *st, struct ellc_ast *ast)
{
    switch(ast->type) {
    case ELLC_AST_REF: ellc_expl_ref(st, ast); break;
    case ELLC_AST_DEF: ellc_expl_def(st, ast); break;
    case ELLC_AST_SET: ellc_expl_set(st, ast); break;
    case ELLC_AST_COND: ellc_expl_cond(st, ast); break;
    case ELLC_AST_SEQ: ellc_expl_seq(st, ast); break;
    case ELLC_AST_APP: ellc_expl_app(st, ast); break;
    case ELLC_AST_LAM: ellc_expl_lam(st, ast); break;
    default:
        printf("explication error: %d\n", ast->type);
        exit(EXIT_FAILURE);
    }
}

static void
ellc_expl(struct ellc_st *st, struct ellc_ast_seq *ast_seq)
{
    for (lnode_t *n = list_first(ast_seq->exprs); n; n = list_next(ast_seq->exprs, n))
        ellc_expl_ast(st, (struct ellc_ast *) lnode_get(n));
}

/**** Emission ****/

static void
ellc_emit_ast(struct ellc_st *st, struct ellc_ast *ast);

static char *
ellc_mangle_id(struct ellc_id *id)
{
    return ell_str_chars(ell_sym_name(id->sym));
}

static char *
ellc_mangle_glo_id(struct ellc_id *id)
{
    return ellc_mangle_id(id);
}

static char *
ellc_mangle_param_id(struct ellc_id *id)
{
    return ellc_mangle_id(id);
}

static char *
ellc_mangle_env_id(struct ellc_id *id)
{
    return ellc_mangle_id(id);
}

static char *
ellc_mangle_arg_id(struct ellc_id *id)
{
    return ellc_mangle_id(id);
}

static void
ellc_emit_glo_ref(struct ellc_st *st, struct ellc_ast *ast)
{
    printf("%s", ellc_mangle_glo_id(ast->glo_ref.id));
}

static void
ellc_emit_arg_ref(struct ellc_st *st, struct ellc_ast *ast)
{
    if (ellc_param_boxed(ast->arg_ref.param)) {
        printf("ell_box_read(%s)", ellc_mangle_arg_id(ast->arg_ref.param->id));
    } else {
        printf("%s", ellc_mangle_arg_id(ast->arg_ref.param->id));
    }
}

static void
ellc_emit_env_ref(struct ellc_st *st, struct ellc_ast *ast)
{
    if (ellc_param_boxed(ast->env_ref.param)) {
        printf("ell_box_read(env->%s)", ellc_mangle_env_id(ast->env_ref.param->id));
    } else {
        printf("(env->%s)", ellc_mangle_env_id(ast->env_ref.param->id));
    }
}

static void
ellc_emit_def(struct ellc_st *st, struct ellc_ast *ast)
{
    printf("(%s = ", ellc_mangle_glo_id(ast->def.id));
    ellc_emit_ast(st, ast->def.val);
    printf(")");
}

static void
ellc_emit_glo_set(struct ellc_st *st, struct ellc_ast *ast)
{
    printf("(%s = ", ellc_mangle_glo_id(ast->glo_set.id));
    ellc_emit_ast(st, ast->glo_set.val);
    printf(")");
}

static void
ellc_emit_arg_set(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_ast_arg_set *arg_set = &ast->arg_set;
    if (ellc_param_boxed(arg_set->param)) {
        printf("(ell_box_write(%s, ", ellc_mangle_arg_id(arg_set->param->id));
        ellc_emit_ast(st, arg_set->val);
        printf("))");
    } else {
        printf("(%s = ", ellc_mangle_arg_id(arg_set->param->id));
        ellc_emit_ast(st, arg_set->val);
        printf(")");
    }
}

static void
ellc_emit_env_set(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_ast_env_set *env_set = &ast->env_set;
    if (ellc_param_boxed(env_set->param)) {
        printf("(ell_box_write(env->%s, ", ellc_mangle_env_id(env_set->param->id));
        ellc_emit_ast(st, env_set->val);
        printf("))");
    } else {
        printf("(env->%s = ", ellc_mangle_env_id(env_set->param->id));
        ellc_emit_ast(st, env_set->val);
        printf(")");
    }
}

static void
ellc_emit_cond(struct ellc_st *st, struct ellc_ast *ast)
{
    printf("(if(");
    ellc_emit_ast(st, ast->cond.test);
    printf("){");
    ellc_emit_ast(st, ast->cond.consequent);
    printf("}else{");
    ellc_emit_ast(st, ast->cond.alternative);
    printf("})");
}

static void
ellc_emit_seq(struct ellc_st *st, struct ellc_ast *ast)
{
    printf("({");
    for (lnode_t *n = list_first(ast->seq.exprs); n; n = list_next(ast->seq.exprs, n)) {
        ellc_emit_ast(st, (struct ellc_ast *) lnode_get(n));
        printf("; ");
    }
    printf("})");
}

static void
ellc_emit_app(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_ast_app *app = &ast->app;
    listcount_t npos = list_count(&app->args->pos);
    printf("({");
    if (npos > 0) {
        printf("struct ell_obj *args[] = {");
        for (lnode_t *n = list_first(&app->args->pos); n; n = list_next(&app->args->pos, n)) {
            struct ellc_ast *arg_ast = (struct ellc_ast *) lnode_get(n);
            ellc_emit_ast(st, arg_ast);
            printf(", ");
        }
        printf("}; ");
    }
    printf("ell_call(");
    ellc_emit_ast(st, app->op);
    printf(", %u, 0, %s);", npos, (npos > 0 ? "args" : "NULL"));
    printf("})");
}

static void
ellc_emit_lam(struct ellc_st *st, struct ellc_ast *ast)
{
    struct ellc_ast_lam *lam = &ast->lam;
    printf("({");
    // populate env
    if (dict_count(lam->env) > 0) {
        printf("struct __ell_env_%u *__lam_env = ell_alloc(sizeof(struct __ell_env_%u));",
               lam->code_id, lam->code_id);
        for (dnode_t *n = dict_first(lam->env); n; n = dict_next(lam->env, n)) {
            struct ellc_id *env_id = (struct ellc_id *) dnode_getkey(n);
            printf("__lam_env->%s = ", ellc_mangle_env_id(env_id));
            ellc_emit_ast(st, (struct ellc_ast *) dnode_get(n));
            printf("; ");
        }
    }
    // create closure
    if (dict_count(lam->env) > 0) {
        printf("ell_make_clo(&__ell_code_%u, __lam_env);", lam->code_id);
    } else {
        printf("ell_make_clo(&__ell_code_%u, NULL);", lam->code_id);
    }
    printf("})");
}

static void
ellc_emit_ast(struct ellc_st *st, struct ellc_ast *ast)
{
    switch(ast->type) {
    case ELLC_AST_GLO_REF: ellc_emit_glo_ref(st, ast); break;
    case ELLC_AST_ARG_REF: ellc_emit_arg_ref(st, ast); break;
    case ELLC_AST_ENV_REF: ellc_emit_env_ref(st, ast); break;
    case ELLC_AST_DEF: ellc_emit_def(st, ast); break;
    case ELLC_AST_GLO_SET: ellc_emit_glo_set(st, ast); break;
    case ELLC_AST_ARG_SET: ellc_emit_arg_set(st, ast); break;
    case ELLC_AST_ENV_SET: ellc_emit_env_set(st, ast); break;
    case ELLC_AST_COND: ellc_emit_cond(st, ast); break;
    case ELLC_AST_SEQ: ellc_emit_seq(st, ast); break;
    case ELLC_AST_APP: ellc_emit_app(st, ast); break;
    case ELLC_AST_LAM: ellc_emit_lam(st, ast); break;
    default:
        printf("emission error\n");
        exit(EXIT_FAILURE);
    }
}

static void
ellc_emit_globals(list_t *globals)
{
    printf("// GLOBALS\n");
    for (lnode_t *n = list_first(globals); n; n = list_next(globals, n)) {
        struct ellc_id *id = (struct ellc_id *) lnode_get(n);
        printf("__attribute__((weak)) struct ell_obj *%s;\n", ellc_mangle_glo_id(id));
    }
    printf("\n");
}

static void
ellc_emit_req_param_val(struct ellc_param *p, unsigned pos)
{
    if (ellc_param_boxed(p))
        printf("ell_make_box(args[%u])", pos);
    else
        printf("args[%u]", pos);
}

static void
ellc_emit_opt_param_val(struct ellc_st *st, struct ellc_param *p, unsigned pos)
{
    if (ellc_param_boxed(p))
        printf("npos >= %u ? ell_make_box(args[%u]) : ", pos, pos);
    else
        printf("npos >= %u ? args[%u] : ", pos, pos);

    if (p->init)
        ellc_emit_ast(st, p->init);
    else
        printf("NULL");
}

static void
ellc_emit_params(struct ellc_st *st, struct ellc_ast_lam *lam)
{
    listcount_t nreq = list_count(lam->params->req);
    if (nreq > 0) printf("\tif (npos < %u) { ell_arity_error(); }\n", nreq);
    
    unsigned pos = 0;
    for (lnode_t *n = list_first(lam->params->req); n; n = list_next(lam->params->req, n)) {
        struct ellc_param *p = (struct ellc_param *) lnode_get(n);
        printf("\tvoid *%s = ", ellc_mangle_param_id(p->id));
        ellc_emit_req_param_val(p, pos);
        printf(";\n");
        pos++;
    }
    for (lnode_t *n = list_first(lam->params->opt); n; n = list_next(lam->params->opt, n)) {
        struct ellc_param *p = (struct ellc_param *) lnode_get(n);
        printf("\tvoid *%s = ", ellc_mangle_param_id(p->id));
        ellc_emit_opt_param_val(st, p, pos);
        printf(";\n");
        pos++;
    }

    if (list_count(lam->params->key) > 0) {
        printf("keyword parameters not yet supported\n");
        exit(EXIT_FAILURE);
    }

    if (lam->params->rest || lam->params->all_keys) {
        printf("rest and all-keys parameters not yet supported\n");
        exit(EXIT_FAILURE);
    }
}

static void
ellc_emit_codes(struct ellc_st *st, list_t *lambdas)
{
    unsigned code_id = 0;
    for (lnode_t *n = list_first(lambdas); n; n = list_next(lambdas, n)) {
        struct ellc_ast_lam *lam = (struct ellc_ast_lam *) lnode_get(n);
        printf("// CLOSURE %u\n", code_id);
        // env
        if (dict_count(lam->env) > 0) {
            printf("struct __ell_env_%u {\n", code_id);
            for (dnode_t *en = dict_first(lam->env); en; en = dict_next(lam->env, en)) {
                struct ellc_id *env_id = (struct ellc_id *) dnode_getkey(en);
                printf("\tvoid *%s;\n", ellc_mangle_env_id(env_id));
            }
            printf("};\n");
        }
        // code
        printf("static struct ell_obj *");
        printf("__ell_code_%u(struct ell_obj *clo, unsigned npos, "
               "unsigned nkey, struct ell_obj **args) {\n", code_id);
        ellc_emit_params(st, lam);
        printf("\tstruct __ell_env_%u *env = (struct __ell_env_%u *)"
               "((struct ell_clo_data *) clo->data)->env;\n", code_id, code_id);
        printf("\treturn ");
        ellc_emit_ast(st, lam->body);
        printf(";");
        printf("\n}\n\n");

        code_id++;
    }
}

static void
ellc_emit(struct ellc_st *st, struct ellc_ast_seq *ast_seq)
{
    printf("#include \"ellrt.h\"\n");
    ellc_emit_globals(st->globals);
    ellc_emit_codes(st, st->lambdas);
    printf("// INIT\n");
    printf("__attribute__((constructor(401))) static void init() {\n");
    // glo fun traps
    for (lnode_t *n = list_first(st->globals); n; n = list_next(st->globals, n)) {
        struct ellc_id *id = (struct ellc_id *) lnode_get(n);
        if (id->ns == ELLC_NS_FUN)
            printf("\t%s = ell_glo_fun_trap;\n", ellc_mangle_glo_id(id));
    }
    // body
    for (lnode_t *n = list_first(ast_seq->exprs); n; n = list_next(ast_seq->exprs, n)) {
        printf("\t");
        ellc_emit_ast(st, (struct ellc_ast *) lnode_get(n));
        printf(";\n");
    }
    printf("}\n");
}

/**** Main ****/

int main()
{
    struct ellc_st *st = (struct ellc_st *) ell_alloc(sizeof(*st));
    st->globals = ell_util_make_list();
    st->lambdas = ell_util_make_list();

    struct ellc_ast_seq *ast_seq = ellc_norm(ellc_parse());
    ellc_expl(st, ast_seq);
    ellc_emit(st, ast_seq);
}
