extern SEXP BuildConditionalModelExport(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP BuildGPModelExport(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP BuildModelExport(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP competingPointsCpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP ComputeCoefficientsCpp(SEXP, SEXP);
extern SEXP ComputeCoefficientsCppForPointValues(SEXP, SEXP, SEXP, SEXP);
extern SEXP ComputeCoefficientsCppForPointValuesWithCovariance(SEXP, SEXP, SEXP, SEXP);
extern SEXP ComputeLogProbabilityCpp(SEXP, SEXP, SEXP);
extern SEXP ComputeLogProbabilityCppOfCoefficients(SEXP, SEXP);
extern SEXP ComputeMahalanobisDistanceCpp(SEXP, SEXP, SEXP);
extern SEXP ComputeProbabilityOfCoefficientsCpp(SEXP, SEXP);
extern SEXP DrawMeanCpp(SEXP);
extern SEXP DrawMeanCppAtPoint(SEXP, SEXP);
extern SEXP DrawSampleCpp(SEXP, SEXP, SEXP);
extern SEXP DrawSampleCppAtPoint(SEXP, SEXP, SEXP, SEXP);
extern SEXP DrawSampleCppVector(SEXP, SEXP, SEXP);
extern SEXP EvaluateSampleAtPointCpp(SEXP, SEXP, SEXP);
extern SEXP GetCovarianceAtPointId(SEXP, SEXP, SEXP);
extern SEXP GetCovarianceAtPointPt(SEXP, SEXP, SEXP);
extern SEXP GetCovarianceMatrixCpp(SEXP);
extern SEXP GetDomainPointsCpp(SEXP);
extern SEXP GetFullJacobianCpp(SEXP, SEXP);
extern SEXP GetJacobianCpp(SEXP, SEXP);
extern SEXP GetMeanVectorCpp(SEXP);
extern SEXP GetNoiseVarianceCpp(SEXP);
extern SEXP GetNumberOfPrincipalComponentsCpp(SEXP);
extern SEXP GetOrthonormalPCABasisMatrixCpp(SEXP);
extern SEXP GetPCABasisMatrixCpp(SEXP);
extern SEXP GetPCAVarianceVectorCpp(SEXP);
extern SEXP LoadModel(SEXP, SEXP);
extern SEXP pointer2pPCACpp(SEXP);
extern SEXP PosteriorModel(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP PosteriorModelSafe(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP pPCA2pointerCpp(SEXP);
extern SEXP ReducedModel(SEXP, SEXP, SEXP, SEXP);
extern SEXP SaveModel(SEXP, SEXP);
extern SEXP SetScale(SEXP, SEXP);
extern SEXP vtkBooleanOpCpp(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vtkExtractOutsideMeshCpp(SEXP, SEXP);
extern SEXP vtkFillHole(SEXP, SEXP);
extern SEXP vtkGeodesicPath(SEXP, SEXP, SEXP);
extern SEXP vtkICPCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vtkImageBlender(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vtkImageReSize(SEXP, SEXP, SEXP, SEXP);
extern SEXP vtkImageTransformCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vtkMeshInfoCpp(SEXP);
extern SEXP vtkPolyToImageData(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vtkRead(SEXP, SEXP);
extern SEXP vtkSegment2PolyData(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP vtkSurfaceRekoCpp(SEXP, SEXP);
extern SEXP vtkVisualize(SEXP, SEXP, SEXP);
extern SEXP vtkWrite(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"BuildConditionalModelExport",                        (DL_FUNC) &BuildConditionalModelExport,                        8},
    {"BuildGPModelExport",                                 (DL_FUNC) &BuildGPModelExport,                                 5},
    {"BuildModelExport",                                   (DL_FUNC) &BuildModelExport,                                   6},
    {"competingPointsCpp",                                 (DL_FUNC) &competingPointsCpp,                                 4},
    {"ComputeCoefficientsCpp",                             (DL_FUNC) &ComputeCoefficientsCpp,                             2},
    {"ComputeCoefficientsCppForPointValues",               (DL_FUNC) &ComputeCoefficientsCppForPointValues,               4},
    {"ComputeCoefficientsCppForPointValuesWithCovariance", (DL_FUNC) &ComputeCoefficientsCppForPointValuesWithCovariance, 4},
    {"ComputeLogProbabilityCpp",                           (DL_FUNC) &ComputeLogProbabilityCpp,                           3},
    {"ComputeLogProbabilityCppOfCoefficients",             (DL_FUNC) &ComputeLogProbabilityCppOfCoefficients,             2},
    {"ComputeMahalanobisDistanceCpp",                      (DL_FUNC) &ComputeMahalanobisDistanceCpp,                      3},
    {"ComputeProbabilityOfCoefficientsCpp",                (DL_FUNC) &ComputeProbabilityOfCoefficientsCpp,                2},
    {"DrawMeanCpp",                                        (DL_FUNC) &DrawMeanCpp,                                        1},
    {"DrawMeanCppAtPoint",                                 (DL_FUNC) &DrawMeanCppAtPoint,                                 2},
    {"DrawSampleCpp",                                      (DL_FUNC) &DrawSampleCpp,                                      3},
    {"DrawSampleCppAtPoint",                               (DL_FUNC) &DrawSampleCppAtPoint,                               4},
    {"DrawSampleCppVector",                                (DL_FUNC) &DrawSampleCppVector,                                3},
    {"EvaluateSampleAtPointCpp",                           (DL_FUNC) &EvaluateSampleAtPointCpp,                           3},
    {"GetCovarianceAtPointId",                             (DL_FUNC) &GetCovarianceAtPointId,                             3},
    {"GetCovarianceAtPointPt",                             (DL_FUNC) &GetCovarianceAtPointPt,                             3},
    {"GetCovarianceMatrixCpp",                             (DL_FUNC) &GetCovarianceMatrixCpp,                             1},
    {"GetDomainPointsCpp",                                 (DL_FUNC) &GetDomainPointsCpp,                                 1},
    {"GetFullJacobianCpp",                                 (DL_FUNC) &GetFullJacobianCpp,                                 2},
    {"GetJacobianCpp",                                     (DL_FUNC) &GetJacobianCpp,                                     2},
    {"GetMeanVectorCpp",                                   (DL_FUNC) &GetMeanVectorCpp,                                   1},
    {"GetNoiseVarianceCpp",                                (DL_FUNC) &GetNoiseVarianceCpp,                                1},
    {"GetNumberOfPrincipalComponentsCpp",                  (DL_FUNC) &GetNumberOfPrincipalComponentsCpp,                  1},
    {"GetOrthonormalPCABasisMatrixCpp",                    (DL_FUNC) &GetOrthonormalPCABasisMatrixCpp,                    1},
    {"GetPCABasisMatrixCpp",                               (DL_FUNC) &GetPCABasisMatrixCpp,                               1},
    {"GetPCAVarianceVectorCpp",                            (DL_FUNC) &GetPCAVarianceVectorCpp,                            1},
    {"LoadModel",                                          (DL_FUNC) &LoadModel,                                          2},
    {"pointer2pPCACpp",                                    (DL_FUNC) &pointer2pPCACpp,                                    1},
    {"PosteriorModel",                                     (DL_FUNC) &PosteriorModel,                                     6},
    {"PosteriorModelSafe",                                 (DL_FUNC) &PosteriorModelSafe,                                 7},
    {"pPCA2pointerCpp",                                    (DL_FUNC) &pPCA2pointerCpp,                                    1},
    {"ReducedModel",                                       (DL_FUNC) &ReducedModel,                                       4},
    {"SaveModel",                                          (DL_FUNC) &SaveModel,                                          2},
    {"SetScale",                                           (DL_FUNC) &SetScale,                                           2},
    {"vtkBooleanOpCpp",                                    (DL_FUNC) &vtkBooleanOpCpp,                                    5},
    {"vtkExtractOutsideMeshCpp",                           (DL_FUNC) &vtkExtractOutsideMeshCpp,                           2},
    {"vtkFillHole",                                        (DL_FUNC) &vtkFillHole,                                        2},
    {"vtkGeodesicPath",                                    (DL_FUNC) &vtkGeodesicPath,                                    3},
    {"vtkICPCpp",                                          (DL_FUNC) &vtkICPCpp,                                          9},
    {"vtkImageBlender",                                    (DL_FUNC) &vtkImageBlender,                                    5},
    {"vtkImageReSize",                                     (DL_FUNC) &vtkImageReSize,                                     4},
    {"vtkImageTransformCpp",                               (DL_FUNC) &vtkImageTransformCpp,                               6},
    {"vtkMeshInfoCpp",                                     (DL_FUNC) &vtkMeshInfoCpp,                                     1},
    {"vtkPolyToImageData",                                 (DL_FUNC) &vtkPolyToImageData,                                 6},
    {"vtkRead",                                            (DL_FUNC) &vtkRead,                                            2},
    {"vtkSegment2PolyData",                                (DL_FUNC) &vtkSegment2PolyData,                                6},
    {"vtkSurfaceRekoCpp",                                  (DL_FUNC) &vtkSurfaceRekoCpp,                                  2},
    {"vtkVisualize",                                       (DL_FUNC) &vtkVisualize,                                       3},
    {"vtkWrite",                                           (DL_FUNC) &vtkWrite,                                           5},
    {NULL, NULL, 0}
};

void R_init_RvtkStatismo(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
