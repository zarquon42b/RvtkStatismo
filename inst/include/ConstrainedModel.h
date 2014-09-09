#ifndef _CONSTRAINED_MODEL_H__
#define _CONSTRAINED_MODEL_H__
#include "VTKTypes.h"
#include "statismo/PosteriorModelBuilder.h"
#include <RcppEigen.h>
#include "pPCA2statismo.h"
#include "Helpers.h"

double mahadist(const StatisticalModelType* model, vtkPoint targetPt, vtkPoint meanPt);

RcppExport SEXP PosteriorModel(SEXP pPCA_,SEXP sample_, SEXP mean_, SEXP ptValueNoise_);

RcppExport SEXP PosteriorModelSafe(SEXP pPCA_,SEXP sample_, SEXP mean_, SEXP ptValueNoise_,SEXP maha_);

#endif 
