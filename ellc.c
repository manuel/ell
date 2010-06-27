/***** Executable and Linkable Lisp Compiler *****/

#include "ellc.h"

/**** Parsing ****/

#include "grammar.c"

struct ellc_parser_stack {
    struct ellc_parser_stack *down;
    struct ell_obj *stx_lst;
};

struct ellc_parser_stack *ellc_parser_stack_top;

struct ell_obj *
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

void
ellc_parser_add_sym(char *chars)
{
    size_t len = strlen(chars) + 1;
    char *copy = (char *) ell_alloc(len);
    strncpy(copy, chars, len);
    struct ell_obj *stx_sym = ell_make_stx_sym(ell_intern(ell_make_str(copy)));
    ELL_SEND(ellc_parser_stack_top->stx_lst, add, stx_sym);
}

void
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

void
ellc_parser_pop()
{
    ellc_parser_stack_top = ellc_parser_stack_top->down;
}

/**** Normalization ****/



/**** Explication ****/

/**** Main ****/

int main()
{
    struct ell_obj *stx_lst = ellc_parse();
    ELL_SEND(stx_lst, print_object);
}
