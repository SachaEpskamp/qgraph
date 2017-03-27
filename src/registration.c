#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void qgraph_layout_fruchtermanreingold_R(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
  {"qgraph_layout_fruchtermanreingold_R", (DL_FUNC) &qgraph_layout_fruchtermanreingold_R, 14},
  {NULL, NULL, 0}
};

void R_init_qgraph(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}