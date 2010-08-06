/* This x-header is included from `ellrt.c' and `ellrt.h' with
   different definitions of `ELL_DEFCLASS'. */

ELL_DEFCLASS(num_int, "<integer>")
ELL_DEFCLASS(sym, "<symbol>")
ELL_DEFCLASS(str, "<string>")
ELL_DEFCLASS(clo, "<function>")
ELL_DEFCLASS(lst, "<linked-list>")
ELL_DEFCLASS(boolean, "<boolean>")
ELL_DEFCLASS(unspecified, "<void>")
ELL_DEFCLASS(unbound, "<unbound>")

ELL_DEFCLASS(list_range, "<linked-list-range>")

ELL_DEFCLASS(stx_sym, "<syntax-symbol>")
ELL_DEFCLASS(stx_str, "<syntax-string>")
ELL_DEFCLASS(stx_num, "<syntax-number>")
ELL_DEFCLASS(stx_lst, "<syntax-list>")
