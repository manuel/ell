/***** Executable and Linkable Lisp Runtime *****/

#include <gc/gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dict.h"
#include "list.h"

/**** Allocation ****/

#define ell_alloc GC_MALLOC

/**** Brands and Objects ****/

struct ell_brand {
    dict_t methods;
};

struct ell_obj {
    struct ell_brand *brand;
    void *data;
};

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
    dict_init(&brand->methods, DICTCOUNT_T_MAX, (dict_comp_t) &strcmp);
    return brand;
}

#define ELL_BRAND(name) __ell_brand_##name

#define ELL_DEFBRAND(name)                                 \
    struct ell_brand *ELL_BRAND(name);                     \
                                                           \
    __attribute__((constructor(202))) static void          \
    __ell_init_brand_##name()                              \
    {                                                      \
        ELL_BRAND(name) = ell_make_brand();                \
    }

void
ell_assert_brand(struct ell_obj *obj, struct ell_brand *brand)
{
    if (obj->brand != brand) {
        printf("brand assertion failed\n");
        exit(EXIT_FAILURE);
    }
}

/**** Strings ****/

struct ell_str_data {
    char *chars;
};

ELL_DEFBRAND(str)

struct ell_obj *
ell_make_str(char *chars)
{
    struct ell_str_data *data = (struct ell_str_data *) ell_alloc(sizeof(*data));
    data->chars = chars;
    return ell_make_obj(ELL_BRAND(str), data);
}

char *
ell_str_chars(struct ell_obj *str)
{
    ell_assert_brand(str, ELL_BRAND(str));
    return ((struct ell_str_data *) str->data)->chars;
}

/**** Symbols ****/

struct dict_t ell_sym_tab;

__attribute__((constructor(201))) static void
ell_init_sym_tab()
{
    dict_init(&ell_sym_tab, DICTCOUNT_T_MAX, (dict_comp_t) &strcmp);
}

struct ell_sym_data {
    struct ell_obj *name;
};

ELL_DEFBRAND(sym)

struct ell_obj *
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

char *
ell_sym_name_chars(struct ell_obj *sym)
{
    ell_assert_brand(sym, ELL_BRAND(sym));
    return ell_str_chars(((struct ell_sym_data *) sym->data)->name);
}

#define ELL_SYM(name) __ell_sym_##name

/**** Closures ****/

typedef struct ell_obj *
ell_code(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args);

struct ell_clo_data {
    ell_code *code;
    void *env;
};

ELL_DEFBRAND(clo)

struct ell_obj *
ell_make_clo(ell_code *code, void *env)
{
    struct ell_clo_data *data = (struct ell_clo_data *) ell_alloc(sizeof(*data));
    data->code = code;
    data->env = env;
    return ell_make_obj(ELL_BRAND(clo), data);
}

struct ell_obj *
ell_call_unchecked(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args) {
    return ((struct ell_clo_data *) clo->data)->code(clo, npos, nkey, args);
}

struct ell_obj *
ell_call(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args) {
    ell_assert_brand(clo, ELL_BRAND(clo));
    return ell_call_unchecked(clo, npos, nkey, args);
}

#define ELL_CALL(clo, ...)                                              \
    ({                                                                  \
        struct ell_obj *__ell_args[] = { __VA_ARGS__ };                 \
        unsigned npos = sizeof(__ell_args) / sizeof(struct ell_obj *);  \
        return ell_call(clo, npos, 0, __ell_args);                      \
    })

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
ell_put_method(struct ell_brand *brand, struct ell_obj *msg, struct ell_obj *clo)
{
    ell_assert_brand(clo, ELL_BRAND(clo));
    char *msg_chars = ell_sym_name_chars(msg);
    dnode_t *node = dict_lookup(&brand->methods, msg_chars);
    if (node) {
        dnode_put(node, clo);
    } else {
        node = (dnode_t *) ell_alloc(sizeof(*node));
        dnode_init(node, clo);
        dict_insert(&brand->methods, node, msg_chars);
    }
}

struct ell_obj *
ell_find_method(struct ell_obj *rcv, struct ell_obj *msg)
{
    dnode_t *node = dict_lookup(&rcv->brand->methods, ell_sym_name_chars(msg));
    if (node) {
        return (struct ell_obj *) dnode_get(node);
    } else {
        return NULL;
    }
}

struct ell_obj *
ell_send(struct ell_obj *rcv, struct ell_obj *msg, 
         unsigned npos, unsigned nkey, struct ell_obj **args)
{
    struct ell_obj *clo = ell_find_method(rcv, msg);
    if (clo) {
        return ell_call_unchecked(clo, npos, nkey, args);
    } else {
        printf("message not understood\n");
        exit(EXIT_FAILURE);
    }
}

#define ELL_SEND(rcv, msg, ...)                                         \
    ({                                                                  \
        struct ell_obj *__ell_rcv = rcv;                                \
        struct ell_obj *__ell_args[] = { __ell_rcv, __VA_ARGS__ };      \
        unsigned npos = sizeof(__ell_args) / sizeof(struct ell_obj *);  \
        ell_send(__ell_rcv, ELL_SYM(msg), npos, 0, __ell_args);         \
    })

#define ELL_METHOD_CODE(brand, msg) __ell_method_code_##brand##_##msg

#define ELL_DEFMETHOD(brand, msg, lisp_msg, formal_npos)                \
    __attribute__((weak)) struct ell_obj *ELL_SYM(msg);                 \
                                                                        \
    ell_code ELL_METHOD_CODE(brand, msg);                               \
                                                                        \
    __attribute__((constructor(203))) static void                       \
    __ell_init_method_##brand##_##msg()                                 \
    {                                                                   \
        if (!ELL_SYM(msg))                                              \
            ELL_SYM(msg) = ell_intern(ell_make_str(lisp_msg));          \
        struct ell_obj *clo =                                           \
            ell_make_clo(&ELL_METHOD_CODE(brand, msg), NULL);           \
        ell_put_method(ELL_BRAND(brand), ELL_SYM(msg), clo);            \
    }                                                                   \
                                                                        \
    struct ell_obj *                                                    \
    ELL_METHOD_CODE(brand, msg)(struct ell_obj *clo, unsigned npos,     \
                                unsigned nkey, struct ell_obj **args)   \
    {                                                                   \
        ell_check_npos(formal_npos, npos);

#define ELL_PARAM(name, i) \
    struct ell_obj *name = args[i];

#define ELL_END \
    }

/**** Syntax Objects ****/

struct ell_stx_sym_data {
    struct ell_obj *sym;
};

struct ell_stx_str_data {
    struct ell_obj *str;
};

struct ell_stx_lst_data {
    list_t elts;
};

ELL_DEFBRAND(stx_sym)
ELL_DEFBRAND(stx_str)
ELL_DEFBRAND(stx_lst)

struct ell_obj *
ell_make_stx_sym(struct ell_obj *sym)
{
    ell_assert_brand(sym, ELL_BRAND(sym));
    struct ell_stx_sym_data *data = (struct ell_stx_sym_data *) ell_alloc(sizeof(*data));
    data->sym = sym;
    return ell_make_obj(ELL_BRAND(stx_sym), data);    
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

ELL_DEFMETHOD(stx_lst, add, "add", 2)
ELL_PARAM(stx_lst, 0)
ELL_PARAM(elt, 1)
lnode_t *node = (lnode_t *) ell_alloc(sizeof(*node));
lnode_init(node, elt);
list_append(&((struct ell_stx_lst_data *) stx_lst->data)->elts, node);
return stx_lst;
ELL_END

ELL_DEFMETHOD(stx_lst, print, "print", 1)
//ELL_PARAM(stx_lst, 0)
printf("foo\n");
return NULL;
ELL_END

