/***** Executable and Linkable Lisp Type System *****/

#include "list.h"

struct ellts_type;

// X
struct ellts_ctor_var {
    char *ctor_var_name;
};

// C
struct ellts_ctor_cls {
    char *ctor_cls_name;
};

// (P* => T)
struct ellts_ctor_fun {
    list_t *ctor_fun_params; // param
    struct ellts_type *ctor_fun_result;
};

// K ::= X | C | (P* => T)
struct ellts_ctor {
    enum { ELLTS_CTOR_VAR, ELLTS_CTOR_CLS, ELLTS_CTOR_FUN } ctor_ilk;
    __extension__ union {
        struct ellts_ctor_var *ctor_var;
        struct ellts_ctor_cls *ctor_cls;
        struct ellts_ctor_fun *ctor_fun;
    };
};

// (C K*)
struct ellts_bound {
    struct ellts_ctor_cls *bound_cls;
    list_t *bound_args; // ctor
};

// P ::= (<: (X P*) (C K*))
struct ellts_param {
    struct ellts_ctor_var *param_op;
    list_t *param_params; // param
    struct ellts_bound *bound; // may be NULL
};

// T ::= (K K*)
struct ellts_type {
    struct ellts_ctor *type_op;
    list_t *type_args; // ctor
};