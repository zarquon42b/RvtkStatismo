#ifndef _HELPERS_H__
#define _HELPERS_H__
#include "VTKTypes.h"
#include "Rcpp.h"
using namespace Rcpp;
using namespace statismo;

vtkPoint NumVec2vtkPoint(NumericVector vec);

vtkPoint SEXP2vtkPoint(SEXP vec);

#endif
