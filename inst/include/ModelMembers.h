#ifndef _MODEL_MEMBERS_H__
#define _MODEL_MEMBERS_H__


#include "VTKTypes.h"
#include "pPCA2statismo.h"
#include "StatismoIO.h"
#include "polyData2R.h"
#include "RcppEigen.h"



RcppExport SEXP DrawMeanCpp(SEXP pPCA_);

RcppExport SEXP DrawMeanCppAtPoint(SEXP pPCA_, SEXP meanpt_);

RcppExport SEXP DrawSampleCpp(SEXP pPCA_, SEXP coeffs_, SEXP addNoise_= Rcpp::wrap(false));

RcppExport SEXP DrawSampleCppVector(SEXP pPCA_, SEXP coeffs_, SEXP addNoise_);

RcppExport SEXP DrawSampleCppAtPoint(SEXP pPCA_, SEXP coeffs_, SEXP meanpt_, SEXP addNoise_);

RcppExport SEXP LoadModel(SEXP modelname_, SEXP pointer_);

RcppExport SEXP ComputeLogProbabilityCpp(SEXP pPCA_, SEXP dataset_, SEXP getlog_= wrap(false));

RcppExport SEXP ComputeMahalanobisDistanceCpp(SEXP pPCA_, SEXP dataset_);

RcppExport SEXP ComputeCoefficientsCpp(SEXP pPCA_, SEXP dataset_);

RcppExport SEXP ComputeCoefficientsCpp(SEXP pPCA_, SEXP dataset_);

// RcppExport SEXP RobustlyComputeCoefficients(SEXP pPCA_, SEXP dataset_, SEXP niterations_, SEXP nu_, SEXP sigma2_);

RcppExport SEXP GetDomainPointsCpp(SEXP pPCA_);

RcppExport SEXP GetCovarianceAtPointId(SEXP pPCA_, SEXP pt1_, SEXP pt2_);

RcppExport SEXP GetCovarianceAtPointPt(SEXP pPCA_, SEXP pt1_, SEXP pt2_);

RcppExport SEXP GetCovarianceMatrixCpp(SEXP pPCA_);

RcppExport SEXP GetJacobianCpp(SEXP pPCA_, SEXP pt_);

RcppExport SEXP ComputeCoefficientsCppForPointValues(SEXP pPCA_, SEXP sample_, SEXP mean_, SEXP noise_);

RcppExport SEXP ComputeCoefficientsCppForPointValuesWithCovariance(SEXP pPCA_, SEXP sample_, SEXP mean_, SEXP noise_);

RcppExport SEXP EvaluateSampleAtPointCpp(SEXP pPCA_, SEXP dataset_, SEXP meanpt_);

RcppExport SEXP GetPCABasisMatrixCpp(SEXP pPCA_);

RcppExport SEXP GetOrthonormalPCABasisMatrixCpp(SEXP pPCA_);

RcppExport SEXP GetNumberOfPrincipalComponentsCpp(SEXP pPCA_);

RcppExport SEXP GetMeanVectorCpp(SEXP pPCA_);

RcppExport SEXP GetNoiseVarianceCpp(SEXP pPCA_);

RcppExport SEXP GetPCAVarianceVectorCpp(SEXP pPCA_);

RcppExport SEXP ComputeLogProbabilityCppOfCoefficients(SEXP pPCA_, SEXP coeffs_);

  RcppExport SEXP ComputeProbabilityOfCoefficientsCpp(SEXP pPCA_, SEXP coeffs_);
#endif// _MODEL_MEMBERS_H__
