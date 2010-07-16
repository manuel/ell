/***** Executable and Linkable Lisp Compilation Manager *****/

#ifndef ELLCM_H
#define ELLCM_H

#include "ellrt.h"
#include "ellc.h"

#define ELLCM_PATH_LEN 256
#define ELLCM_MSG_LEN 256

struct ellcm_tx {
    char infile[ELLCM_PATH_LEN];
    char faslfile[ELLCM_PATH_LEN];
    char cfaslfile[ELLCM_PATH_LEN];
};

struct ellcm_rx {
    int status;
    char msg[ELLCM_MSG_LEN];
};

struct ellcm {
    int sd;
};

struct ellcm *
ellcm_init();

int
ellcm_compile_str(struct ellcm *cm, char *s, char *faslfile, char *cfaslfile);

int
ellcm_compile_file(struct ellcm *cm, char *infile, char *faslfile, char *cfaslfile);

struct ell_obj *
ellcm_eval(struct ellcm *cm, char *s);

void
ellcm_load_file(struct ellcm *cm, char *infile);

#endif
