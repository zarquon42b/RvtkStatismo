#ifndef _BUILD_MODEL_H__
#define _BUILD_MODEL_H__

#include "VTKTypes.h"
#include <memory>
#include <RcppEigen.h>
#include "pPCA2statismo.h"


using namespace Rcpp;
using namespace statismo;

RcppExport SEXP BuildModelExport(SEXP myshapelist_,SEXP myreference_,SEXP sigma_);
auto_ptr<StatisticalModelType> BuildModel(SEXP myshapelist_,SEXP myreference_,SEXP sigma_);


#endif //#ifndef _BUILD_MODEL_H__
