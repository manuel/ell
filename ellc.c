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
ellc_make_id(struct ell_obj *sym)
{
    ell_assert_brand(sym, ELL_BRAND(sym));
    struct ellc_id *id = (struct ellc_id *) ell_alloc(sizeof(*id));
    id->sym = sym;
    return id;
}

static struct ellc_ast_seq *
ellc_make_ast_seq()
{
    struct ellc_ast_seq *ast_seq = ell_alloc(sizeof(*ast_seq));
    ast_seq->exprs = (list_t *) ell_alloc(sizeof(list_t));
    list_init(ast_seq->exprs, LISTCOUNT_T_MAX);
    return ast_seq;
}

static void
ellc_ast_seq_add(struct ellc_ast_seq *ast_seq, struct ellc_ast *expr)
{
    lnode_t *node = (lnode_t *) ell_alloc(sizeof(*node));
    lnode_init(node, expr);
    list_append(ast_seq->exprs, node);
}

static struct ellc_ast *
ellc_make_ast(enum ellc_ast_type type)
{
    struct ellc_ast *ast = (struct ellc_ast *) ell_alloc(sizeof(*ast));
    ast->type = type;
    return ast;
}

/**** Normalization: Syntax Objects -> Normal Form AST ****/

static struct ellc_ast *
ellc_norm_stx(struct ell_obj *stx);

typedef struct ellc_ast *
(ellc_norm_fun)(struct ell_obj *stx_lst);

/* (Simple Forms) */

static struct ellc_ast *
ellc_norm_ref(struct ell_obj *stx_sym)
{
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_REF);
    ast->ref.id = ellc_make_id(ell_stx_sym_sym(stx_sym));
    return ast;
}

static struct ellc_ast *
ellc_norm_fref(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 2);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_FREF);
    ast->fref.id = ellc_make_id(ell_stx_sym_sym(ELL_SEND(stx_lst, second)));
    return ast;
}

static struct ellc_ast *
ellc_norm_def(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 3);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_DEF);
    ast->def.id = ellc_make_id(ell_stx_sym_sym(ELL_SEND(stx_lst, second)));
    ast->def.val = ellc_norm_stx(ELL_SEND(stx_lst, third));
    return ast;
}

static struct ellc_ast *
ellc_norm_fdef(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 3);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_FDEF);
    ast->fdef.id = ellc_make_id(ell_stx_sym_sym(ELL_SEND(stx_lst, second)));
    ast->fdef.val = ellc_norm_stx(ELL_SEND(stx_lst, third));
    return ast;
}

static struct ellc_ast *
ellc_norm_set(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 3);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_SET);
    ast->set.id = ellc_make_id(ell_stx_sym_sym(ELL_SEND(stx_lst, second)));
    ast->set.val = ellc_norm_stx(ELL_SEND(stx_lst, third));
    return ast;
}

static struct ellc_ast *
ellc_norm_fset(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len(stx_lst, 3);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_FSET);
    ast->fset.id = ellc_make_id(ell_stx_sym_sym(ELL_SEND(stx_lst, second)));
    ast->fset.val = ellc_norm_stx(ELL_SEND(stx_lst, third));
    return ast;
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
    struct ellc_ast_seq *ast_seq = ellc_make_ast_seq();
    list_t *elts_stx = ell_util_sublist(ell_stx_lst_elts(stx_lst), 1);
    listcount_t len = list_count(elts_stx);

    if (len > 0) {
        lnode_t *n = list_first(elts_stx);
        int i = 0;
        do {
            struct ell_obj *stx = (struct ell_obj *) lnode_get(n);
            ellc_ast_seq_add(ast_seq, ellc_norm_stx(stx));
            n = list_next(elts_stx, n);
            i++;
        } while(i < len);
    }

    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_SEQ);
    ast->seq.exprs = ast_seq->exprs;
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
    listcount_t ct = list_count(args_stx);
    if (ct == 0) return args;
    
    lnode_t *n = list_first(args_stx);
    int i = 0;
    do {
        struct ell_obj *arg_stx = lnode_get(n);
        if ((arg_stx->brand == ELL_BRAND(stx_sym))
                 && ellc_is_key_arg_sym(ell_stx_sym_sym(arg_stx))) {
            i++;
            if (i == ct) {
                printf("missing value for keyword argument\n");
                exit(EXIT_FAILURE);
            }
            struct ell_obj *key_arg_sym = 
                ellc_clean_key_arg_sym(ell_stx_sym_sym(arg_stx));
            n = list_next(args_stx, n);
            struct ellc_ast *key_arg_ast = 
                ellc_norm_stx((struct ell_obj *) lnode_get(n));
            
            dnode_t *dn = (dnode_t *) ell_alloc(sizeof(*dn));
            dnode_init(dn, key_arg_ast);
            dict_insert(&args->key, dn, key_arg_sym);
        } else {
            lnode_t *m = (lnode_t *) ell_alloc(sizeof(*m));
            lnode_init(m, ellc_norm_stx(arg_stx));
            list_append(&args->pos, m);
        }
        i++;
        n = list_next(args_stx, n);
    } while(i < ct);

    return args;
}

static struct ellc_ast *
ellc_norm_app(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len_min(stx_lst, 2);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_APP);
    ast->app.op = ellc_norm_stx(ELL_SEND(stx_lst, second));
    ast->app.args = ellc_dissect_args(ell_util_sublist(ell_stx_lst_elts(stx_lst), 2));
    return ast;
}

static struct ellc_ast *
ellc_norm_ordinary_app(struct ell_obj *stx_lst)
{
    ell_assert_stx_lst_len_min(stx_lst, 1);
    struct ellc_ast *ast = ellc_make_ast(ELLC_AST_APP);
    struct ell_obj *op_stx = ELL_SEND(stx_lst, first);
    ell_assert_brand(op_stx, ELL_BRAND(stx_sym));
    struct ellc_ast *op_ast = ellc_make_ast(ELLC_AST_FREF);
    op_ast->fref.id = ellc_make_id(ell_stx_sym_sym(op_stx));
    ast->app.op = op_ast;
    ast->app.args = ellc_dissect_args(ell_util_sublist(ell_stx_lst_elts(stx_lst), 1));
    return ast;
}

/* (Abstraction and Parameters Dissection) */

static struct ellc_param *
ellc_dissect_param(struct ell_obj *p_stx)
{
    struct ellc_param *p = (struct ellc_param *) ell_alloc(sizeof(*p));
    p->type = ELLC_PARAM_VAR;
    if (p_stx->brand == ELL_BRAND(stx_sym)) {
        p->id = ellc_make_id(ell_stx_sym_sym(p_stx));
    } else if (p_stx->brand == ELL_BRAND(stx_lst)) {
        ell_assert_stx_lst_len(p_stx, 2);
        struct ell_obj *name_stx = ELL_SEND(p_stx, first);
        struct ell_obj *init_stx = ELL_SEND(p_stx, second);
        p->id = ellc_make_id(ell_stx_sym_sym(name_stx));
        p->init = ellc_norm_stx(init_stx);
    }
    return p;
}

static struct ellc_params *
ellc_dissect_params(list_t *params_stx)
{
    struct ellc_params *params = (struct ellc_params *) ell_alloc(sizeof(*params));
    dictcount_t len = list_count(params_stx);

    if (len == 0) return params;

    list_t *req = (list_t *) ell_alloc(sizeof(list_t));
    list_init(req, LISTCOUNT_T_MAX);
    list_t *opt = (list_t *) ell_alloc(sizeof(list_t));
    list_init(opt, LISTCOUNT_T_MAX);
    list_t *key = (list_t *) ell_alloc(sizeof(list_t));
    list_init(key, LISTCOUNT_T_MAX);
    list_t *rest = (list_t *) ell_alloc(sizeof(list_t));
    list_init(rest, LISTCOUNT_T_MAX);
    list_t *all_keys = (list_t *) ell_alloc(sizeof(list_t));
    list_init(all_keys, LISTCOUNT_T_MAX);

    list_t *cur = req;

    int i = 0;
    lnode_t *n = list_first(params_stx);
    do {
        struct ell_obj *p_stx = lnode_get(n);
        if (p_stx->brand == ELL_BRAND(stx_sym)) {
            struct ell_obj *p_sym = ell_stx_sym_sym(p_stx);
            if (p_sym == ELL_SYM(param_optional)) {
                cur = opt;
                goto next;
            } else if (p_sym == ELL_SYM(param_key)) {
                cur = key;
                goto next;
            } else if (p_sym == ELL_SYM(param_rest)) {
                cur = rest;
                goto next;
            } else if (p_sym == ELL_SYM(param_all_keys)) {
                cur = all_keys;
                goto next;
            }
        }
        lnode_t *pn = (lnode_t *) ell_alloc(sizeof(*pn));
        lnode_init(pn, ellc_dissect_param(p_stx));
        list_append(cur, pn);
    next:
        i++;
        n = list_next(params_stx, n);
    } while(i < len);

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
    return ast;
}

/* (Putting it All Together) */

static struct dict_t ellc_norm_tab;

static void
ellc_norm_tab_put(struct ell_obj *sym, ellc_norm_fun *norm_fun)
{
    dnode_t *dn = (dnode_t *) ell_alloc(sizeof(*dn));
    dnode_init(dn, norm_fun);
    dict_insert(&ellc_norm_tab, dn, sym);
}

__attribute__((constructor(301))) static void
ellc_init_norm_tab()
{
    dict_init(&ellc_norm_tab, DICTCOUNT_T_MAX, (dict_comp_t) &ell_sym_cmp);
    ellc_norm_tab_put(ELL_SYM(core_fref), &ellc_norm_fref);
    ellc_norm_tab_put(ELL_SYM(core_def), &ellc_norm_def);
    ellc_norm_tab_put(ELL_SYM(core_fdef), &ellc_norm_fdef);
    ellc_norm_tab_put(ELL_SYM(core_set), &ellc_norm_set);
    ellc_norm_tab_put(ELL_SYM(core_fset), &ellc_norm_fset);
    ellc_norm_tab_put(ELL_SYM(core_cond), &ellc_norm_cond);
    ellc_norm_tab_put(ELL_SYM(core_seq), &ellc_norm_seq);
    ellc_norm_tab_put(ELL_SYM(core_app), &ellc_norm_app);
    ellc_norm_tab_put(ELL_SYM(core_lam), &ellc_norm_lam);
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

/**** Explication: Normal Form AST -> Explicit Form AST ****/

/**** Emission: Produces "C" ****/

/**** Main ****/

int main()
{
    struct ell_obj *stx_lst = ellc_parse();
    struct ellc_ast_seq *ast_seq = ellc_norm(stx_lst);
    ELL_SEND(stx_lst, print_object);
}
