/***** Executable and Linkable Lisp Runtime *****/

#ifndef ELL_H
#define ELL_H

#include <gc/gc.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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
ell_make_obj(struct ell_brand *brand, void *data);
struct ell_brand *
ell_make_brand();
void
ell_assert_brand(struct ell_obj *obj, struct ell_brand *brand);

#define ELL_BRAND(name) __ell_brand_##name

#define ELL_DEFBRAND(name) struct ell_brand *ELL_BRAND(name);
#include "brands.h"
#undef ELL_DEFBRAND

/**** Closures ****/

typedef struct ell_obj *
ell_code(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args);

struct ell_clo_data {
    ell_code *code;
    void *env;
};

struct ell_obj *
ell_make_clo(ell_code *code, void *env);
struct ell_obj *
ell_call_unchecked(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args);
struct ell_obj *
ell_call(struct ell_obj *clo, unsigned npos, unsigned nkey, struct ell_obj **args);
void
ell_check_npos(unsigned formal_npos, unsigned actual_npos);

#define ELL_CALL(clo, ...)                                              \
    ({                                                                  \
        struct ell_obj *__ell_args[] = { __VA_ARGS__ };                 \
        unsigned npos = sizeof(__ell_args) / sizeof(struct ell_obj *);  \
        return ell_call(clo, npos, 0, __ell_args);                      \
    })

/**** Methods ****/

void
ell_put_method(struct ell_brand *brand, struct ell_obj *msg_sym, struct ell_obj *clo);
struct ell_obj *
ell_find_method(struct ell_obj *rcv, struct ell_obj *msg_sym);
struct ell_obj *
ell_send(struct ell_obj *rcv, struct ell_obj *msg_sym,
         unsigned npos, unsigned nkey, struct ell_obj **args);

#define ELL_SEND(rcv, msg, ...)                                         \
    ({                                                                  \
        struct ell_obj *__ell_rcv = rcv;                                \
        struct ell_obj *__ell_args[] = { __ell_rcv, __VA_ARGS__ };      \
        unsigned npos = sizeof(__ell_args) / sizeof(struct ell_obj *);  \
        ell_send(__ell_rcv, ELL_SYM(msg), npos, 0, __ell_args);         \
    })

#define ELL_METHOD_CODE(brand, msg) __ell_method_code_##brand##_##msg

#define ELL_DEFMETHOD(brand, msg, formal_npos)                          \
    ell_code ELL_METHOD_CODE(brand, msg);                               \
                                                                        \
    __attribute__((constructor(201))) static void                       \
    __ell_init_method_##brand##_##msg()                                 \
    {                                                                   \
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

/**** Strings ****/

struct ell_str_data {
    char *chars;
};

struct ell_obj *
ell_make_strn(char *chars, size_t len);
struct ell_obj *
ell_make_str(char *chars);
char *
ell_str_chars(struct ell_obj *str);
size_t
ell_str_len(struct ell_obj *str);
char
ell_str_char_at(struct ell_obj *str, size_t i);
struct ell_obj *
ell_str_poplast(struct ell_obj *str);

/**** Symbols ****/

struct dict_t ell_sym_tab;

struct ell_sym_data {
    struct ell_obj *name;
};

#define ELL_SYM(name) __ell_sym_##name

#define ELL_DEFSYM(name, lisp_name) __attribute__((weak)) struct ell_obj *ELL_SYM(name);
#include "syms.h"
#undef ELL_DEFSYM

struct ell_obj *
ell_intern(struct ell_obj *str);
struct ell_obj *
ell_sym_name(struct ell_obj *sym);
int
ell_sym_cmp(struct ell_obj *sym_a, struct ell_obj *sym_b);

/**** Syntax Objects ****/

struct ell_obj *
ell_parse();

struct ell_stx_sym_data {
    struct ell_obj *sym;
};

struct ell_stx_str_data {
    struct ell_obj *str;
};

struct ell_stx_lst_data {
    list_t elts;
};

struct ell_obj *
ell_make_stx_sym(struct ell_obj *sym);
struct ell_obj *
ell_make_stx_str(struct ell_obj *str);
struct ell_obj *
ell_make_stx_lst();
struct ell_obj *
ell_stx_sym_sym(struct ell_obj *stx_sym);
struct ell_obj *
ell_stx_str_str(struct ell_obj *stx_str);
list_t *
ell_stx_lst_elts(struct ell_obj *stx_lst);
listcount_t
ell_stx_lst_len(struct ell_obj *stx_lst);
void
ell_assert_stx_lst_len(struct ell_obj *stx_lst, listcount_t len);
void
ell_assert_stx_lst_len_min(struct ell_obj *stx_lst, listcount_t len);

/**** Utilities ****/

list_t *
ell_util_make_list();
void
ell_util_list_add(list_t *list, void *elt);
list_t *
ell_util_sublist(list_t *list, listcount_t start);
void
ell_util_assert_list_len(list_t *list, listcount_t len);
void
ell_util_assert_list_len_min(list_t *list, listcount_t len);
dict_t *
ell_util_make_dict(dict_comp_t comp);
void
ell_util_dict_put(dict_t *dict, void *key, void *val);
void
ell_util_set_add(list_t *set, void *elt, dict_comp_t compare);

/**** Utilities for Generated Code ****/

__attribute__((weak)) struct ell_obj *ell_result;

void
ell_arity_error();
struct ell_obj *
ell_unbound_var(char *name);
struct ell_obj *
ell_unbound_fun(char *name);
struct ell_obj **
ell_make_box(struct ell_obj *value);
struct ell_obj *
ell_box_read(struct ell_obj **box);
void
ell_box_write(struct ell_obj **box, struct ell_obj *value);

#endif
