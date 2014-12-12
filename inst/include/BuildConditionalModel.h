#ifndef _BUILD_MODEL_H__
#define _BUILD_MODEL_H__

#include "VTKTypes.h"
#include <memory>
#include <RcppEigen.h>
#include "pPCA2statismo.h"
#include "DataManagerWithSurrogateVector.h"

using namespace Eigen;
using namespace Rcpp;
using namespace statismo;

RcppExport SEXP BuildConditionalModelExport(SEXP myshapelist_,SEXP myreference_,SEXP sigma_,SEXP trainingData_, SEXP condData_,SEXP surrogateInfo_, SEXP exVar_);
shared_ptr<vtkMeshModel> BuildConditionalModel(SEXP myshapelist_,SEXP myreference_,SEXP sigma_,SEXP trainingData_, SEXP condData_,SEXP surrogateInfo_, SEXP exVar_);


#endif //#ifndef _BUILD_MODEL_H__
