#include "BuildGaussProcessModel.h"

auto_ptr<StatisticalModelType> BuildGPModel(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_) {
  unsigned int nystroem = as<unsigned int>(nystroem_);
  unsigned int numberOfComponents = as<unsigned int>(ncomp_);
  List kernels(kernels_);
  try {
    auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    const MatrixValuedKernelType& statModelKernel = StatisticalModelKernel<vtkPolyData>(model.get());
    //for (unsigned int i = 0; i < kernels.size();i++) {
    NumericVector kerntmp = kernels[0];
    double gaussianKernelSigma = kerntmp[0];
    double gaussianKernelScale = kerntmp[1];
    const GaussianKernel gk = GaussianKernel(gaussianKernelSigma);
    const MatrixValuedKernelType& mvGk = UncorrelatedMatrixValuedKernel<vtkPoint>(&gk, model->GetRepresenter()->GetDimensions());
    const MatrixValuedKernelType& scaledGk = ScaledKernel<vtkPoint>(&mvGk, gaussianKernelScale);
      
    const MatrixValuedKernelType& combinedModelAndGaussKernel = SumKernel<vtkPoint>(&statModelKernel, &scaledGk);

    auto_ptr<ModelBuilderType> modelBuilder(ModelBuilderType::Create(model->GetRepresenter()));
    auto_ptr<StatisticalModelType> combinedModel(modelBuilder->BuildNewModel(model->DrawMean(), combinedModelAndGaussKernel, numberOfComponents,nystroem));
    //combinedModel->Save("test.h5");
    return combinedModel;
  }
  catch (StatisticalModelException& e) {
    Rprintf("Exception occured while building the shape model\n");
    Rprintf("%s\n",  e.what());
    auto_ptr<StatisticalModelType> model;
    return model;
  }
}
RcppExport SEXP BuildGPModelExport(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_){
  
  auto_ptr<StatisticalModelType> model = BuildGPModel(pPCA_,kernels_,ncomp_,nystroem_);
//return statismo2pPCA(model);
  return statismo2pPCA(model);
  
}
RcppExport SEXP DrawMean(SEXP pPCA_){
  auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
  vtkSmartPointer<vtkPolyData> reference = model->DrawMean();
  List out = polyData2R(reference);
  return out;
}

RcppExport SEXP LoadModel(SEXP modelname_){
  CharacterVector modelname(modelname_);
  vtkStandardMeshRepresenter* representer = vtkStandardMeshRepresenter::Create();
  std::string modelFilename = as<std::string>(modelname);
  

  auto_ptr<StatisticalModelType> model(StatisticalModelType::Load(representer, modelFilename));
  List out = statismo2pPCA(model);
  return out;
}
