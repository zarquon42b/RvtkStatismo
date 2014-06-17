#include "VTKTypes.h"
#include <string.h>
#include <RcppEigen.h>
#include "pPCA2statismo.h"

using namespace Rcpp;

RcppExport SEXP SaveModel(SEXP pPCA_, SEXP filename_) {
  std::string str = Rcpp::as<std::string>(filename_);
   const char *filename = str.c_str();
   
   auto_ptr<StatisticalModelType>model = pPCA2statismo(pPCA_);
   model->Save(filename);
   return wrap(1);
}
