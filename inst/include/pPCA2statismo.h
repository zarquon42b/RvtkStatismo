#ifndef _STATISMO2PPCA_H__
#define _STATISMO2PPCA_H__

#include "VTKTypes.h"
#include "ModelInfo.h"
#include "PCAModelBuilder.h"
#include "StatisticalModel.h"
#include "DataManager.h"
#include <memory>
#include <RcppEigen.h>
#include "R2polyData.h"


using namespace Rcpp;

S4 statismo2pPCA(XPtr<vtkMeshModel> model, bool pointer = false);
XPtr<vtkMeshModel> pPCA2statismo(SEXP pPCA_);
#endif // _STATISMO2PPCA_H__

