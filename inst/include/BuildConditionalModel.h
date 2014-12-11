#ifndef _BUILD_MODEL_H__
#define _BUILD_MODEL_H__

#include "VTKTypes.h"
#include <memory>
#include <RcppEigen.h>
#include "pPCA2statismo.h"

using namespace Eigen;
using namespace Rcpp;
using namespace statismo;

RcppExport SEXP BuildConditionalModelExport(SEXP myshapelist_,SEXP myreference_,SEXP sigma_,SEXP trainingData_, SEXP condData_,SEXP surrogateInfo_);
shared_ptr<vtkMeshModel> BuildConditionalModel(SEXP myshapelist_,SEXP myreference_,SEXP sigma_,SEXP trainingData_, SEXP condData_,SEXP surrogateInfo_);


#endif //#ifndef _BUILD_MODEL_H__
