#include "VTKTypes.h"
#include "pPCA2statismo.h"
#include "polyData2R.h"

#ifndef _MODEL_MEMBERS_H__
#define _MODEL_MEMBERS_H__

RcppExport SEXP DrawMean(SEXP pPCA_);

RcppExport SEXP LoadModel(SEXP modelname_);

RcppExport SEXP ComputeLogProbabilityOfDataset(SEXP pPCA_, SEXP dataset_, SEXP getlog_= wrap(false));

RcppExport SEXP ComputeCoefficientsForDataset(SEXP pPCA_, SEXP dataset_);

RcppExport SEXP ComputeCoefficientsForDataset(SEXP pPCA_, SEXP dataset_);

#endif// _MODEL_MEMBERS_H__
