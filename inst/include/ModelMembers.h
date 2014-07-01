#ifndef _MODEL_MEMBERS_H__
#define _MODEL_MEMBERS_H__


#include "VTKTypes.h"
#include "pPCA2statismo.h"
#include "polyData2R.h"
#include "Rcpp.h"



RcppExport SEXP DrawMean(SEXP pPCA_);

RcppExport SEXP DrawSample(SEXP pPCA_, SEXP coeffs_, SEXP addNoise_= Rcpp::wrap(false));

RcppExport SEXP LoadModel(SEXP modelname_);

RcppExport SEXP ComputeLogProbabilityOfDataset(SEXP pPCA_, SEXP dataset_, SEXP getlog_= wrap(false));

RcppExport SEXP ComputeCoefficientsForDataset(SEXP pPCA_, SEXP dataset_);

RcppExport SEXP ComputeCoefficientsForDataset(SEXP pPCA_, SEXP dataset_);

RcppExport SEXP GetDomainPoints(SEXP pPCA_);

RcppExport SEXP GetCovarianceAtPointId(SEXP pPCA_, SEXP pt1_, SEXP pt2_);

RcppExport SEXP GetCovarianceAtPointPt(SEXP pPCA_, SEXP pt1_, SEXP pt2_);

RcppExport SEXP GetCovarianceMatrix(SEXP pPCA_);

RcppExport SEXP GetJacobian(SEXP pPCA_, SEXP pt_);

RcppExport SEXP ComputeCoefficientsForPointValues(SEXP pPCA_, SEXP sample_, SEXP mean_, SEXP noise_);

RcppExport SEXP EvaluateSampleAtPoint(SEXP pPCA_, SEXP dataset_, SEXP meanpt_);

#endif// _MODEL_MEMBERS_H__
