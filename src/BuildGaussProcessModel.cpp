#include "BuildGaussProcessModel.h"

auto_ptr<StatisticalModelType> BuildGPModel(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_) {
  unsigned int nystroem = as<unsigned int>(nystroem_);
  unsigned int numberOfComponents = as<unsigned int>(ncomp_);
  List kernels(kernels_);
  try {
    auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    // get the empiric kernel
    auto_ptr<MatrixValuedKernelType> statModelKernel(new StatisticalModelKernel<vtkPolyData>(model.get()));
    // set up the gaussian kernel to be incremented over a list of parameters
    NumericVector params = kernels[0];
    auto_ptr<GaussianKernel> gk(new GaussianKernel(params[0]));
    auto_ptr<MatrixValuedKernelType> mvKernel(new UncorrelatedMatrixValuedKernel<vtkPoint>(gk.get(), model->GetRepresenter()->GetDimensions()));
    
    auto_ptr<MatrixValuedKernelType> sumKernel(new ScaledKernel<vtkPoint>(mvKernel.get(), params[1]));
    //iterate over the remaining kernel parameters
    for (unsigned int i = 1; i < kernels.size();i++) {
      params = kernels[i];
      auto_ptr<GaussianKernel> gkNew(new GaussianKernel(params[0]));
      auto_ptr<MatrixValuedKernelType> mvGk(new UncorrelatedMatrixValuedKernel<vtkPoint>(gkNew.get(), model->GetRepresenter()->GetDimensions()));
      auto_ptr<MatrixValuedKernelType> scaledGk(new ScaledKernel<vtkPoint>(mvGk.get(), params[1]));
      auto_ptr<MatrixValuedKernelType> dump = sumKernel;
      sumKernel.reset(new SumKernel<vtkPoint>(dump.get(), scaledGk.get()));
    }
    // add the empiric kernel on top
    auto_ptr<MatrixValuedKernelType> dump = sumKernel;
    sumKernel.reset(new SumKernel<vtkPoint>(dump.get(), statModelKernel.get()));
    
    //build new model
    auto_ptr<ModelBuilderType> modelBuilder(ModelBuilderType::Create(model->GetRepresenter()));
    auto_ptr<StatisticalModelType> combinedModel(modelBuilder->BuildNewModel(model->DrawMean(), *sumKernel, numberOfComponents,nystroem));
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
