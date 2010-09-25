/***** Executable and Linkable Lisp Load Tool *****/

/*
  ell-load [(-x compile-time.fasl) | (-l file) | (-e expression) | -q]*
  
  Compiles and loads Lisp source files, and provides a read-eval-print loop.

  Arguments:

  -l file---A Lisp source file to compile and load, or a FASL to load.
  -x compile-time.fasl---A file to load at compile-time.
  -e expression---A Lisp expression to evaluate.
  -q---Quit right now.  Without -q, enters REPL after options processing.
*/

#include <getopt.h>
#include <stdio.h>
#include <readline/readline.h>
#include <unistd.h>

#include "ellcm.h"

static void
ell_repl(struct ellcm *cm);

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

static void
ell_repl(struct ellcm *cm)
{
    printf("Welcome to ell!\nExit with (exit).\n");
    for(;;) {
        char *line = readline("> ");
        if (line == NULL)
            break;
        struct ell_obj *result = ellcm_eval(cm, line);
        if (result == ell_unspecified) {
            printf("; no value\n");
        } else {
            ELL_SEND(result, printDobject);
            printf("\n");
        }
        free(line);
    }
}
