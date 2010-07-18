/***** Executable and Linkable Lisp Runtime *****/

#include "ellrt.h"

/**** Parsing ****/

#include "grammar.c"

int yyparse();

struct ell_parser_stack {
    struct ell_parser_stack *down;
    struct ell_obj *stx_lst;
};

static struct ell_parser_stack *ell_parser_stack_top;

struct ell_obj *
ell_parse()
{
    ell_parser_stack_top = 
        (struct ell_parser_stack *) ell_alloc(sizeof(*ell_parser_stack_top));
    ell_parser_stack_top->down = NULL;
    ell_parser_stack_top->stx_lst = ell_make_stx_lst();
    if(!yyparse()) {
        printf("parsing error\n");
        exit(EXIT_FAILURE);
    }
    return ell_parser_stack_top->stx_lst;
}

void
ell_parser_add_sym(char *chars)
{
    struct ell_obj *stx_sym = ell_make_stx_sym(ell_intern(ell_make_str(chars)));
    ELL_SEND(ell_parser_stack_top->stx_lst, add, stx_sym);
}

void
ell_parser_add_str(char *chars)
{
    struct ell_obj *stx_str = ell_make_stx_str(ell_make_str(chars));
    ELL_SEND(ell_parser_stack_top->stx_lst, add, stx_str);
}

void
ell_parser_push()
{
    struct ell_parser_stack *new = 
        (struct ell_parser_stack *) ell_alloc(sizeof(*new));
    struct ell_obj *new_stx_lst = ell_make_stx_lst();
    new->down = ell_parser_stack_top;
    new->stx_lst = new_stx_lst;
    ELL_SEND(ell_parser_stack_top->stx_lst, add, new_stx_lst);
    ell_parser_stack_top = new;
}

void
ell_parser_pop()
{
    ell_parser_stack_top = ell_parser_stack_top->down;
}

void
ell_parser_push_special(struct ell_obj *sym)
{
    struct ell_parser_stack *new = 
        (struct ell_parser_stack *) ell_alloc(sizeof(*new));
    struct ell_obj *new_stx_lst = ell_make_stx_lst();
    ELL_SEND(new_stx_lst, add, ell_make_stx_sym(sym));
    new->down = ell_parser_stack_top;
    new->stx_lst = new_stx_lst;
    ELL_SEND(ell_parser_stack_top->stx_lst, add, new_stx_lst);
    ell_parser_stack_top = new;
}

void
ell_parser_push_quote()
{
    ell_parser_push_special(ELL_SYM(core_quote));
}

void
ell_parser_push_syntax()
{
    ell_parser_push_special(ELL_SYM(core_syntax));
}

void
ell_parser_push_quasisyntax()
{
    ell_parser_push_special(ELL_SYM(core_quasisyntax));
}

void
ell_parser_push_unsyntax()
{
    ell_parser_push_special(ELL_SYM(core_unsyntax));
}

void
ell_parser_push_unsyntax_splicing()
{
    ell_parser_push_special(ELL_SYM(core_unsyntax_splicing));
}

/**** Brands and Objects ****/

struct ell_obj *
ell_make_obj(struct ell_brand *brand, void *data)
{
    struct ell_obj *obj = (struct ell_obj *) ell_alloc(sizeof(*obj));
    obj->brand = brand;
    obj->data = data;
    return obj;
}

struct ell_brand *
ell_make_brand()
{
    struct ell_brand *brand = (struct ell_brand *) ell_alloc(sizeof(*brand));
    dict_init(&brand->methods, DICTCOUNT_T_MAX, (dict_comp_t) &ell_sym_cmp);
    return brand;
}

void
ell_assert_brand(struct ell_obj *obj, struct ell_brand *brand)
{
    if (obj->brand != brand) {
        printf("brand assertion failed\n");
        exit(EXIT_FAILURE);
    }
}

/**** Closures ****/

struct ell_obj *
ell_make_clo(ell_code *code, void *env)
{
    struct ell_clo_data *data = (struct ell_clo_data *) ell_alloc(sizeof(*data));
    data->code = code;
    data->env = env;
    return ell_make_obj(ELL_BRAND(clo), data);
}

struct ell_obj *
ell_call_unchecked(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args)
{
    return ((struct ell_clo_data *) clo->data)->code(clo, npos, nkey, args);
}

struct ell_obj *
ell_call(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args)
{
    ell_assert_brand(clo, ELL_BRAND(clo));
    return ell_call_unchecked(clo, npos, nkey, args);
}

void
ell_check_npos(unsigned formal_npos, unsigned actual_npos)
{
    if (formal_npos != actual_npos) {
        printf("wrong number of arguments");
        exit(EXIT_FAILURE);
    }
}

/**** Methods ****/

void
ell_put_method(struct ell_brand *brand, struct ell_obj *msg_sym, struct ell_obj *clo)
{
    ell_assert_brand(clo, ELL_BRAND(clo));
    ell_assert_brand(msg_sym, ELL_BRAND(sym));
    ell_util_dict_put(&brand->methods, msg_sym, clo);
}

struct ell_obj *
ell_find_method(struct ell_obj *rcv, struct ell_obj *msg_sym)
{
    dnode_t *node = dict_lookup(&rcv->brand->methods, msg_sym);
    if (node) {
        return (struct ell_obj *) dnode_get(node);
    } else {
        return NULL;
    }
}

struct ell_obj *
ell_send(struct ell_obj *rcv, struct ell_obj *msg_sym, 
         unsigned npos, unsigned nkey, struct ell_obj **args)
{
    struct ell_obj *clo = ell_find_method(rcv, msg_sym);
    if (clo) {
        return ell_call_unchecked(clo, npos, nkey, args);
    } else {
        printf("message not understood: %s\n", ell_str_chars(ell_sym_name(msg_sym)));
        exit(EXIT_FAILURE);
    }
}

/**** Control Flow ****/

struct ell_obj *
ell_return_from_code(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args)
{
    struct ell_clo_data *clo_data = (struct ell_clo_data *) clo->data;
    struct ell_block *block = (struct ell_block *) clo_data->env; // See comment in ell_block
    struct ell_obj *val = args[0];
    while(ell_current_unwind_protect != block->parent) {
        struct ell_unwind_protect *unwind_protect = ell_current_unwind_protect;
        ell_current_unwind_protect = ell_current_unwind_protect->parent;
        ELL_CALL(unwind_protect->cleanup);
    }
    block->val = val;
    longjmp(block->dest, 1);
    return NULL;
}

struct ell_obj *
ell_block(struct ell_obj *fun)
{
    struct ell_block block;
    block.parent = ell_current_unwind_protect;
    if (!setjmp(block.dest)) {
        // Faked closure with block as "environment"
        struct ell_obj *escape = ell_make_clo(&ell_return_from_code, &block);
        return ELL_CALL(fun, escape);
    } else {
        return block.val;
    }
}

struct ell_obj *
ell_unwind_protect(struct ell_obj *protected, struct ell_obj *cleanup)
{
    struct ell_unwind_protect unwind_protect;
    unwind_protect.parent = ell_current_unwind_protect;
    unwind_protect.cleanup = cleanup;
    
    ell_current_unwind_protect = &unwind_protect;
    struct ell_obj *val = ELL_CALL(protected);
    ell_current_unwind_protect = ell_current_unwind_protect->parent;
    
    ELL_CALL(cleanup);
    return val;
}

struct ell_obj *
ell_blockLf_code(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args)
{
    return ell_block(args[0]);
}

struct ell_obj *
ell_unwind_protectLf_code(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args)
{
    return ell_unwind_protect(args[0], args[1]);
}

struct ell_obj *__ell_g_blockLf_;
struct ell_obj *__ell_g_unwindDprotectLf_;

/**** Strings ****/

struct ell_obj *
ell_make_strn(char *chars, size_t len)
{
    char *copy = (char *) ell_alloc(len + 1);
    strncpy(copy, chars, len);
    copy[len] = '\0';
    struct ell_str_data *data = (struct ell_str_data *) ell_alloc(sizeof(*data));
    data->chars = copy;
    return ell_make_obj(ELL_BRAND(str), data);
}

struct ell_obj *
ell_make_str(char *chars)
{
    return ell_make_strn(chars, strlen(chars));
}

char *
ell_str_chars(struct ell_obj *str)
{
    ell_assert_brand(str, ELL_BRAND(str));
    return ((struct ell_str_data *) str->data)->chars;
}

size_t
ell_str_len(struct ell_obj *str)
{
    ell_assert_brand(str, ELL_BRAND(str));
    return strlen(ell_str_chars(str));
}

char
ell_str_char_at(struct ell_obj *str, size_t i)
{
    if (i < ell_str_len(str)) {
        return ell_str_chars(str)[i];
    } else {
        printf("string index out of range\n");
        exit(EXIT_FAILURE);
    }
}

struct ell_obj *
ell_str_poplast(struct ell_obj *str)
{
    char *chars = ell_str_chars(str);
    size_t len = strlen(chars);
    if (len <= 1)
        return ell_make_str("");
    else 
        return ell_make_strn(chars, len - 1);
}

/**** Symbols ****/

static struct ell_obj *
ell_make_sym(struct ell_obj *str)
{
    ell_assert_brand(str, ELL_BRAND(str));
    struct ell_sym_data *data = (struct ell_sym_data *) ell_alloc(sizeof(*data));
    data->name = str;
    return ell_make_obj(ELL_BRAND(sym), data);
}

struct ell_obj *
ell_intern(struct ell_obj *str)
{
    char *chars = ell_str_chars(str);
    dnode_t *node = dict_lookup(&ell_sym_tab, chars);
    if (node == NULL) {
        struct ell_obj *sym = ell_make_sym(str);
        node = (dnode_t *) ell_alloc(sizeof(*node));
        dnode_init(node, sym);
        dict_insert(&ell_sym_tab, node, chars);
        return sym;
    } else {
        return (struct ell_obj *) dnode_get(node);
    }    
}

struct ell_obj *
ell_sym_name(struct ell_obj *sym)
{
    ell_assert_brand(sym, ELL_BRAND(sym));
    return ((struct ell_sym_data *) sym->data)->name;
}

int
ell_sym_cmp(struct ell_obj *sym_a, struct ell_obj *sym_b)
{
    ell_assert_brand(sym_a, ELL_BRAND(sym));
    ell_assert_brand(sym_b, ELL_BRAND(sym));
    return sym_a - sym_b;
}

/**** Syntax Objects ****/

struct ell_obj *
ell_make_stx_sym_cx(struct ell_obj *sym, struct ell_cx *cx)
{
    ell_assert_brand(sym, ELL_BRAND(sym));
    struct ell_stx_sym_data *data = (struct ell_stx_sym_data *) ell_alloc(sizeof(*data));
    data->sym = sym;
    data->cx = cx;
    return ell_make_obj(ELL_BRAND(stx_sym), data);    
}

struct ell_obj *
ell_make_stx_sym(struct ell_obj *sym)
{
    return ell_make_stx_sym_cx(sym, NULL);
}

struct ell_obj *
ell_make_stx_str(struct ell_obj *str)
{
    ell_assert_brand(str, ELL_BRAND(str));
    struct ell_stx_str_data *data = (struct ell_stx_str_data *) ell_alloc(sizeof(*data));
    data->str = str;
    return ell_make_obj(ELL_BRAND(stx_str), data);
}

struct ell_obj *
ell_make_stx_lst()
{
    struct ell_stx_lst_data *data = (struct ell_stx_lst_data *) ell_alloc(sizeof(*data));
    list_init(&data->elts, LISTCOUNT_T_MAX);
    return ell_make_obj(ELL_BRAND(stx_lst), data);
}

struct ell_obj *
ell_stx_sym_sym(struct ell_obj *stx_sym)
{
    ell_assert_brand(stx_sym, ELL_BRAND(stx_sym));
    return ((struct ell_stx_sym_data *) stx_sym->data)->sym;
}

struct ell_cx *
ell_stx_sym_cx(struct ell_obj *stx_sym)
{
    ell_assert_brand(stx_sym, ELL_BRAND(stx_sym));
    return ((struct ell_stx_sym_data *) stx_sym->data)->cx;    
}

struct ell_obj *
ell_stx_str_str(struct ell_obj *stx_str)
{
    ell_assert_brand(stx_str, ELL_BRAND(stx_str));
    return ((struct ell_stx_str_data *) stx_str->data)->str;
}

list_t *
ell_stx_lst_elts(struct ell_obj *stx_lst)
{
    ell_assert_brand(stx_lst, ELL_BRAND(stx_lst));
    return &((struct ell_stx_lst_data *) stx_lst->data)->elts;
}

listcount_t
ell_stx_lst_len(struct ell_obj *stx_lst)
{
    return list_count(ell_stx_lst_elts(stx_lst));
}

struct ell_cx *
ell_make_cx()
{
    struct ell_cx *cx = (struct ell_cx *) ell_alloc(sizeof(*cx));
    uuid_generate(cx->uuid);
    return cx;
}

bool
ell_cx_equal(struct ell_cx *cxa, struct ell_cx *cxb)
{
    return (ell_cx_cmp(cxa, cxb) == 0);
}

int
ell_cx_cmp(struct ell_cx *cxa, struct ell_cx *cxb)
{
    if (cxa == NULL) {
        if (cxb == NULL) {
            return 0;
        } else {
            return -1;
        }
    } else {
        if (cxb == NULL) {
            return 1;
        } else {
            return uuid_compare(cxa->uuid, cxb->uuid);
        }
    }
}

void
ell_assert_stx_lst_len(struct ell_obj *stx_lst, listcount_t len)
{
    ell_assert_brand(stx_lst, ELL_BRAND(stx_lst));
    ell_util_assert_list_len(ell_stx_lst_elts(stx_lst), len);
}

void
ell_assert_stx_lst_len_min(struct ell_obj *stx_lst, listcount_t len)
{
    ell_assert_brand(stx_lst, ELL_BRAND(stx_lst));
    ell_util_assert_list_len_min(ell_stx_lst_elts(stx_lst), len);
}

/**** Booleans ****/

bool
ell_is_true(struct ell_obj *obj)
{
    ell_assert_brand(obj, ELL_BRAND(boolean));
    return obj == ell_t;
}

/**** Ranges ****/

struct ell_obj *
ell_make_range_from_list(list_t *elts)
{
    struct ell_list_range_data *data = (struct ell_list_range_data *) ell_alloc(sizeof(*data));
    data->elts = elts;
    data->cur = list_first(elts);
    return ell_make_obj(ELL_BRAND(list_range), data);
}

list_t *
ell_list_range_elts(struct ell_obj *range)
{
    ell_assert_brand(range, ELL_BRAND(list_range));
    return ((struct ell_list_range_data *) range->data)->elts;
}

lnode_t *
ell_list_range_cur(struct ell_obj *range)
{
    ell_assert_brand(range, ELL_BRAND(list_range));
    return ((struct ell_list_range_data *) range->data)->cur;
}

void
ell_list_range_set_cur(struct ell_obj *range, lnode_t *new_cur)
{
    ell_assert_brand(range, ELL_BRAND(list_range));
    ((struct ell_list_range_data *) range->data)->cur = new_cur;
}

ELL_DEFMETHOD(list_range, emptyp, 1)
ELL_PARAM(range, 0)
return (ell_list_range_cur(range) ? ell_f : ell_t);
ELL_END

ELL_DEFMETHOD(list_range, front, 1)
ELL_PARAM(range, 0)
return (struct ell_obj *) lnode_get(ell_list_range_cur(range));
ELL_END

ELL_DEFMETHOD(list_range, pop_front, 1)
ELL_PARAM(range, 0)
ell_list_range_set_cur(range, list_next(ell_list_range_elts(range), ell_list_range_cur(range)));
return ell_unspecified;
ELL_END

/**** Lists ****/

struct ell_obj *
ell_make_lst()
{
    struct ell_lst_data *data = (struct ell_lst_data *) ell_alloc(sizeof(*data));
    list_init(&data->elts, LISTCOUNT_T_MAX);
    return ell_make_obj(ELL_BRAND(lst), data);
}

list_t *
ell_lst_elts(struct ell_obj *lst)
{
    ell_assert_brand(lst, ELL_BRAND(lst));
    return &((struct ell_lst_data *) lst->data)->elts;
}

ELL_DEFMETHOD(lst, add, 2)
ELL_PARAM(lst, 0)
ELL_PARAM(elt, 1)
ell_util_list_add(ell_lst_elts(lst), elt);
return lst;
ELL_END

static void
ell_lst_print_process(list_t *list, lnode_t *node, void *unused)
{
    ELL_SEND((struct ell_obj *) lnode_get(node), print_object);
    printf(" ");
}

ELL_DEFMETHOD(lst, print_object, 1)
ELL_PARAM(lst, 0)
printf("(");
list_process(ell_lst_elts(lst), NULL, &ell_lst_print_process);
printf(")");
return ell_unspecified;
ELL_END

ELL_DEFMETHOD(lst, all, 1)
ELL_PARAM(lst, 0)
return ell_make_range_from_list(ell_lst_elts(lst));
ELL_END

/**** Library ****/

ELL_DEFMETHOD(boolean, print_object, 1)
ELL_PARAM(boolean, 0)
if (ell_is_true(boolean)) {
    printf("#t");
} else {
    printf("#f");
}
return ell_unspecified;
ELL_END

ELL_DEFMETHOD(unspecified, print_object, 1)
return ell_unspecified;
ELL_END

ELL_DEFMETHOD(sym, print_object, 1)
ELL_PARAM(sym, 0)
printf("%s", ell_str_chars(ell_sym_name(sym)));
return ell_unspecified;
ELL_END

ELL_DEFMETHOD(str, print_object, 1)
ELL_PARAM(str, 0)
printf("\"%s\"", ell_str_chars(str));
return ell_unspecified;
ELL_END

ELL_DEFMETHOD(clo, print_object, 1)
ELL_PARAM(self, 0)
printf("%s", "#<function>");
return ell_unspecified;
ELL_END

ELL_DEFMETHOD(stx_lst, add, 2)
ELL_PARAM(stx_lst, 0)
ELL_PARAM(elt, 1)
ell_util_list_add(ell_stx_lst_elts(stx_lst), elt);
return stx_lst;
ELL_END

static void
ell_stx_lst_print_process(list_t *list, lnode_t *node, void *unused)
{
    ELL_SEND((struct ell_obj *) lnode_get(node), print_object);
    printf(" ");
}

ELL_DEFMETHOD(stx_lst, print_object, 1)
ELL_PARAM(stx_lst, 0)
printf("(");
list_process(ell_stx_lst_elts(stx_lst), NULL, &ell_stx_lst_print_process);
printf(")");
return ell_unspecified;
ELL_END

ELL_DEFMETHOD(stx_sym, print_object, 1)
ELL_PARAM(stx_sym, 0)
printf("%s", ell_str_chars(ell_sym_name(ell_stx_sym_sym(stx_sym))));
return ell_unspecified;
ELL_END

ELL_DEFMETHOD(stx_str, print_object, 1)
ELL_PARAM(stx_str, 0)
printf("%s", ell_str_chars(ell_stx_str_str(stx_str)));
return ell_unspecified;
ELL_END

ELL_DEFMETHOD(stx_lst, first, 1)
ELL_PARAM(stx_lst, 0)
ell_assert_stx_lst_len_min(stx_lst, 1);
lnode_t *node = list_first(ell_stx_lst_elts(stx_lst));
return (struct ell_obj *) lnode_get(node);
ELL_END

ELL_DEFMETHOD(stx_lst, second, 1)
ELL_PARAM(stx_lst, 0)
ell_assert_stx_lst_len_min(stx_lst, 2);
list_t *elts = ell_stx_lst_elts(stx_lst);
lnode_t *node = list_next(elts, list_first(elts));
return (struct ell_obj *) lnode_get(node);
ELL_END

ELL_DEFMETHOD(stx_lst, third, 1)
ELL_PARAM(stx_lst, 0)
ell_assert_stx_lst_len_min(stx_lst, 3);
list_t *elts = ell_stx_lst_elts(stx_lst);
lnode_t *node = list_next(elts, list_next(elts, list_first(elts)));
return (struct ell_obj *) lnode_get(node);
ELL_END

ELL_DEFMETHOD(stx_lst, fourth, 1)
ELL_PARAM(stx_lst, 0)
ell_assert_stx_lst_len_min(stx_lst, 4);
list_t *elts = ell_stx_lst_elts(stx_lst);
lnode_t *node = list_next(elts, list_next(elts, list_next(elts, list_first(elts))));
return (struct ell_obj *) lnode_get(node);
ELL_END

ELL_DEFMETHOD(stx_lst, all, 1)
ELL_PARAM(stx_lst, 0)
return ell_make_range_from_list(ell_stx_lst_elts(stx_lst));
ELL_END

/**** Utilities used by Generated Code ****/

void
ell_arity_error()
{
    printf("arity error\n");
    exit(EXIT_FAILURE);
}

struct ell_obj *
ell_unbound_arg()
{
    printf("unbound argument\n");
    exit(EXIT_FAILURE);
    return ell_unspecified;
}

struct ell_obj *
ell_unbound_var(char *name)
{
    printf("unbound variable: %s\n", name);
    exit(EXIT_FAILURE);
    return ell_unspecified;
}

struct ell_obj *
ell_unbound_fun(char *name)
{
    printf("unbound function: %s\n", name);
    exit(EXIT_FAILURE);
    return ell_unspecified;
}

struct ell_obj **
ell_make_box(struct ell_obj *value)
{
    struct ell_obj **box = ell_alloc(sizeof(struct ell_obj *));
    *box = value;
    return box;
}

struct ell_obj *
ell_box_read(struct ell_obj **box)
{
    return *box;
}

void
ell_box_write(struct ell_obj **box, struct ell_obj *value)
{
    *box = value;
}

/**** Data Structure Utilities ****/

list_t *
ell_util_make_list()
{
    list_t *list = (list_t *) ell_alloc(sizeof(*list));
    list_init(list, LISTCOUNT_T_MAX);
    return list;
}

void
ell_util_list_add(list_t *list, void *elt)
{
    lnode_t *new = (lnode_t *) ell_alloc(sizeof(*new));
    lnode_init(new, elt);
    list_append(list, new);
}

list_t *
ell_util_sublist(list_t *list, listcount_t start)
{
    list_t *res = ell_util_make_list();
    if (start >= list_count(list))
        return res;

    lnode_t *n = list_first(list);
    for (int i = 0; i < start; i++) {
        n = list_next(list, n);
    }

    do {
        ell_util_list_add(res, lnode_get(n));
    } while((n = list_next(list, n)));

    return res;
}

bool
ell_util_list_contains(list_t *list, void *elt, dict_comp_t compare)
{
    return (list_find(list, elt, compare) != NULL);
}

void
ell_util_assert_list_len(list_t *list, listcount_t len)
{
    if (len != list_count(list)) {
        printf("list length assertion failed\n");
        exit(EXIT_FAILURE);
    }
}

void
ell_util_assert_list_len_min(list_t *list, listcount_t len)
{
    if (len > list_count(list)) {
        printf("list length assertion failed\n");
        exit(EXIT_FAILURE);
    }
}

dict_t *
ell_util_make_dict(dict_comp_t comp)
{
    dict_t *dict = (dict_t *) ell_alloc(sizeof(*dict));
    dict_init(dict, DICTCOUNT_T_MAX, comp);
    return dict;
}

void
ell_util_dict_put(dict_t *dict, void *key, void *val)
{
    dnode_t *node = dict_lookup(dict, key);
    if (node) {
        dnode_put(node, val);
    } else {
        node = (dnode_t *) ell_alloc(sizeof(*node));
        dnode_init(node, val);
        dict_insert(dict, node, key);
    }
}

void
ell_util_set_add(list_t *set, void *elt, dict_comp_t compare)
{
    for (lnode_t *n = list_first(set); n; n = list_next(set, n))
        if (compare(elt, lnode_get(n)) == 0)
            return;
    ell_util_list_add(set, elt);
}

int
ell_ptr_cmp(void *a, void *b)
{
    return a - b;
}

/**** Built-in Functions ****/

/* (send rcv msg &rest args) -> result */

struct ell_obj *__ell_g_send_;

struct ell_obj *
ell_send_code(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args)
{
    ell_check_npos(2, npos);
    struct ell_obj *rcv = args[0];
    struct ell_obj *msg = args[1];
    ell_assert_brand(msg, ELL_BRAND(sym));
    /* Klever: */
    args++;
    args[0] = rcv;
    return ell_send(rcv, msg, npos - 1, 0, args);
}

/* (syntax-list &rest syntax-objects) -> syntax-list */

struct ell_obj *__ell_g_syntaxDlist_;

struct ell_obj *
ell_syntax_list_code(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args)
{
    struct ell_obj *res = ell_make_stx_lst();
    for (int i = 0; i < npos; i++) {
        ELL_SEND(res, add, args[i]);
    }
    return res;
}

/* (syntax-list-rest syntax-list) -> syntax-list */

struct ell_obj *__ell_g_syntaxDlistDrest_;

struct ell_obj *
ell_syntax_list_rest_code(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args)
{
    ell_check_npos(1, npos);
    struct ell_obj *stx_lst = args[0];
    ell_assert_brand(stx_lst, ELL_BRAND(stx_lst));
    struct ell_obj *res = ell_make_stx_lst();
    list_t *elts = ell_stx_lst_elts(stx_lst);
    for (lnode_t *n = list_next(elts, list_first(elts)); n; n = list_next(elts, n)) {
        ELL_SEND(res, add, (struct ell_obj *) lnode_get(n));
    }
    return res;
}

/* (append-syntax-lists &rest syntax-lists) -> syntax-list */

struct ell_obj *__ell_g_appendDsyntaxDlists_;

struct ell_obj *
ell_append_syntax_lists_code(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args)
{
    struct ell_obj *res = ell_make_stx_lst();
    for (int i = 0; i < npos; i++) {
        struct ell_obj *lst = args[i];
        struct ell_obj *range = ELL_SEND(args[i], all);
        while (!ell_is_true(ELL_SEND(range, emptyp))) {
            struct ell_obj *elt = ELL_SEND(range, front);
            ELL_SEND(res, add, elt);
            ELL_SEND(range, pop_front);
        }
    }
    return res;
}

/* (apply-syntax-list function syntax-list) -> result */

struct ell_obj *__ell_g_applyDsyntaxDlist_;

struct ell_obj *
ell_apply_syntax_list_code(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args)
{
    ell_check_npos(2, npos);
    struct ell_obj *fun = args[0];
    struct ell_obj *stx_lst = args[1];
    ell_assert_brand(fun, ELL_BRAND(clo));
    ell_assert_brand(stx_lst, ELL_BRAND(stx_lst));
    list_t *elts = ell_stx_lst_elts(stx_lst);
    listcount_t len = list_count(elts);
    struct ell_obj *the_args[len];
    int i = 0;
    for (lnode_t *n = list_first(elts); n; n = list_next(elts, n)) {
        the_args[i++] = (struct ell_obj *) lnode_get(n);
    }
    return ell_call(fun, len, 0, the_args);
}

struct ell_obj *__ell_g_exit_;

struct ell_obj *
ell_exit_code(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args)
{
    exit(EXIT_SUCCESS);
    return NULL;
}

/**** Initialization ****/

__attribute__((constructor(200))) static void
ell_init()
{
#define ELL_DEFBRAND(name) ELL_BRAND(name) = ell_make_brand();
#include "brands.h"
#undef ELL_DEFBRAND

    dict_init(&ell_sym_tab, DICTCOUNT_T_MAX, (dict_comp_t) &strcmp);

#define ELL_DEFSYM(name, lisp_name) \
    if (!ELL_SYM(name)) ELL_SYM(name) = ell_intern(ell_make_str(lisp_name));
#include "syms.h"
#undef ELL_DEFSYM

    __ell_g_Ot_ = ell_make_obj(ELL_BRAND(boolean), NULL);
    ell_t = __ell_g_Ot_;
    __ell_g_Of_ = ell_make_obj(ELL_BRAND(boolean), NULL);
    ell_f = __ell_g_Of_;

    __ell_g_unspecified_ = ell_make_obj(ELL_BRAND(unspecified), NULL);
    ell_unspecified = __ell_g_unspecified_;

    ell_unbound = ell_make_obj(ELL_BRAND(unbound), NULL);

    __ell_g_blockLf_ = ell_make_clo(&ell_blockLf_code, NULL);
    __ell_g_unwindDprotectLf_ = ell_make_clo(&ell_unwind_protectLf_code, NULL);

    __ell_g_send_ = ell_make_clo(&ell_send_code, NULL);
    __ell_g_syntaxDlist_ = ell_make_clo(&ell_syntax_list_code, NULL);
    __ell_g_syntaxDlistDrest_ = ell_make_clo(&ell_syntax_list_rest_code, NULL);
    __ell_g_appendDsyntaxDlists_ = ell_make_clo(&ell_append_syntax_lists_code, NULL);
    __ell_g_applyDsyntaxDlist_ = ell_make_clo(&ell_apply_syntax_list_code, NULL);
    __ell_g_exit_ = ell_make_clo(&ell_exit_code, NULL);
}
