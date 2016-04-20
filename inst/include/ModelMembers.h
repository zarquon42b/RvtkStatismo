#ifndef _MODEL_MEMBERS_H__
#define _MODEL_MEMBERS_H__


#include "VTKTypes.h"
#include "pPCA2statismo.h"
#include "StatismoIO.h"
#include "polyData2R.h"
#include "RcppEigen.h"



RcppExport SEXP DrawMean(SEXP pPCA_);

RcppExport SEXP DrawMeanAtPoint(SEXP pPCA_, SEXP meanpt_);

RcppExport SEXP DrawSample(SEXP pPCA_, SEXP coeffs_, SEXP addNoise_= Rcpp::wrap(false));

RcppExport SEXP DrawSampleVector(SEXP pPCA_, SEXP coeffs_, SEXP addNoise_);

RcppExport SEXP DrawSampleAtPoint(SEXP pPCA_, SEXP coeffs_, SEXP meanpt_, SEXP addNoise_);

RcppExport SEXP LoadModel(SEXP modelname_, SEXP pointer_);

RcppExport SEXP ComputeLogProbabilityOfDataset(SEXP pPCA_, SEXP dataset_, SEXP getlog_= wrap(false));

RcppExport SEXP ComputeMahalanobisDistanceForDataset(SEXP pPCA_, SEXP dataset_);

RcppExport SEXP ComputeCoefficientsForDataset(SEXP pPCA_, SEXP dataset_);

RcppExport SEXP ComputeCoefficientsForDataset(SEXP pPCA_, SEXP dataset_);

RcppExport SEXP RobustlyComputeCoefficientsForDataset(SEXP pPCA_, SEXP dataset_, SEXP niterations_, SEXP nu_, SEXP sigma2_);

RcppExport SEXP GetDomainPoints(SEXP pPCA_);

RcppExport SEXP GetCovarianceAtPointId(SEXP pPCA_, SEXP pt1_, SEXP pt2_);

RcppExport SEXP GetCovarianceAtPointPt(SEXP pPCA_, SEXP pt1_, SEXP pt2_);

RcppExport SEXP GetCovarianceMatrix(SEXP pPCA_);

RcppExport SEXP GetJacobian(SEXP pPCA_, SEXP pt_);

RcppExport SEXP ComputeCoefficientsForPointValues(SEXP pPCA_, SEXP sample_, SEXP mean_, SEXP noise_);

RcppExport SEXP ComputeCoefficientsForPointValuesWithCovariance(SEXP pPCA_, SEXP sample_, SEXP mean_, SEXP noise_);

RcppExport SEXP EvaluateSampleAtPoint(SEXP pPCA_, SEXP dataset_, SEXP meanpt_);

RcppExport SEXP GetPCABasisMatrix(SEXP pPCA_);

RcppExport SEXP GetOrthonormalPCABasisMatrix(SEXP pPCA_);

RcppExport SEXP GetNumberOfPrincipalComponents(SEXP pPCA_);

RcppExport SEXP GetMeanVector(SEXP pPCA_);

RcppExport SEXP GetNoiseVariance(SEXP pPCA_);

RcppExport SEXP GetPCAVarianceVector(SEXP pPCA_);

RcppExport SEXP ComputeLogProbabilityOfCoefficients(SEXP pPCA_, SEXP coeffs_);

  RcppExport SEXP ComputeProbabilityOfCoefficients(SEXP pPCA_, SEXP coeffs_);
#endif// _MODEL_MEMBERS_H__
