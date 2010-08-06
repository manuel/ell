/***** Executable and Linkable Lisp REPL *****/

/*
  ell-repl [-x compile-time.fasl]* [-l file]* [-e expression]* [-q]
  
  Compiles and loads Lisp source files, and provides a read-eval-print loop.

  Arguments:

  -l file---A Lisp source file to compile and load, or a FASL to load.
  -x compile-time.fasl---A file to load at compile-time.
  -e expression--A Lisp expression to evaluate.
  -q---Whether to quit after processing instead of entering the REPL.
*/

#include <getopt.h>
#include <stdio.h>
#include <readline/readline.h>
#include <unistd.h>

#include "ellcm.h"

static void
ell_repl(struct ellcm *cm)
{
    printf("Welcome to ell!\nExit with (exit).\n");
    for(;;) {
        char *line = readline("> ");
        struct ell_obj *result = ellcm_eval(cm, line);
        if (result != ell_unspecified) {
            ELL_SEND(result, print_object);
            printf("\n");
        }
        free(line);
    }
}

int
main(int argc, char *argv[])
{
    struct ellcm *cm = ellcm_init();
    bool quit = false;
    opterr = 0;
    int c;
    char *faslfile, *cfaslfile;
    while ((c = getopt (argc, argv, "x:l:e:q")) != -1) {
        switch (c) {
        case 'x':
            ellcm_compiletime_load_file(cm, optarg);
            break;
        case 'l':
            ellcm_load_file(cm, optarg);
            break;
        case 'e':
            ellcm_eval(cm, optarg);
            break;
        case 'q':
            quit = true;
            break;
        }
    }
    if (!quit)
        ell_repl(cm);
    return 0;
}
