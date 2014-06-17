#include "VTKTypes.h"
#include "statismo/PCAModelBuilder.h"
#include "statismo/StatisticalModel.h"
#include "statismo/DataManager.h"
#include <memory>
#include <RcppEigen.h>
#include "R2vtk.h"

using namespace Rcpp;

List statismo2pPCA(auto_ptr<StatisticalModelType> model);
auto_ptr<StatisticalModelType> pPCA2statismo(SEXP pPCA_);
