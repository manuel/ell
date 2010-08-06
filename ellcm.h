/***** Executable and Linkable Lisp Compilation Manager *****/

#ifndef ELLCM_H
#define ELLCM_H

#include "ellrt.h"
#include "ellc.h"

#define ELLCM_PATH_LEN 256
#define ELLCM_MSG_LEN 256

struct ellcm_tx {
    enum { ELLCM_COMPILE, ELLCM_LOAD } type;
    union {
        struct {
            char infile[ELLCM_PATH_LEN];
            char faslfile[ELLCM_PATH_LEN];
            char cfaslfile[ELLCM_PATH_LEN];
        } compile;
        struct {
            char file[ELLCM_PATH_LEN];
        } load;
    } data;
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

int
ellcm_compiletime_load_file(struct ellcm *cm, char *file);

void
ellcm_load_file(struct ellcm *cm, char *infile);

bool
ellcm_is_source_file(struct ellcm *cm, char *file);

bool
ellcm_is_fasl_file(struct ellcm *cm, char *file);

#endif
