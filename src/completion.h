#ifndef COMPLETION_H__
#define COMPLETION_H__

#include <R.h>
#include <Rinternals.h>

/* Extract row indexes needed to build document completion indexes. */
SEXP completion_parse_index_c(
    SEXP id,
    SEXP parent,
    SEXP token,
    SEXP line1,
    SEXP col1,
    SEXP line2,
    SEXP col2
);

#endif /* end of include guard: COMPLETION_H__ */
