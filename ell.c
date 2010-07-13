/***** Executable and Linkable Lisp *****/

#include <getopt.h>
#include <stdio.h>
#include <readline/readline.h>
#include <unistd.h>

#include "ellc.h"

#include "grammar.c"

/*
static char *ell_line;
static size_t ell_len;
static size_t ell_off;

#define YY_INPUT(buf, result, max_size)                          \
    if (ell_off == ell_len) {                                    \
        result = 0;                                              \
    } else {                                                     \
        *(buf) = ell_line[ell_off++];                            \
        result = 1;                                              \
    }

static void
ell_repl()
{
    for(unsigned i = 0; ; i++) {
        ell_line = readline("> ");
        ell_len = strlen(ell_line);
        ell_off = 0;

        struct ell_obj *result = ellc_eval(ell_parse());
        if (result != ell_unspecified) {
            ELL_SEND(result, print_object);
            printf("\n");
        }

        free(ell_line);
    }
}
*/

int
main(int argc, char *argv[])
{
    int opt;
    struct ell_obj *name_str = NULL;
    while ((opt = getopt(argc, argv, "c:")) != -1) {
        switch (opt) {
        case 'c':
            name_str = ell_make_str(optarg);
            break;
        default: /* '?' */
            exit(EXIT_FAILURE);
        }
    }

    if (name_str)
        ellc_compile_file(name_str);
    else
        fprintf(stderr, "Usage: %s -c package.lisp\n", argv[0]);
}
