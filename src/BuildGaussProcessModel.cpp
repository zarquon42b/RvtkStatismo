#include "BuildGaussProcessModel.h"

auto_ptr<StatisticalModelType> BuildGPModel(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_,SEXP useEmp_) {
  try { 
    bool useEmp = as<bool>(useEmp_);
    unsigned int nystroem = as<unsigned int>(nystroem_);
    unsigned int numberOfComponents = as<unsigned int>(ncomp_);
    std::list<MatrixValuedKernelType*> mKerns;
    std::list<GaussianKernel*> gKerns;

    List kernels(kernels_);
  
    auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
   
    // set up the gaussian kernel to be incremented over a list of parameters
    NumericVector params = kernels[0];
    GaussianKernel* gk = new GaussianKernel(params[0]);
    gKerns.push_back(gk);
    MatrixValuedKernelType* mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
    
    MatrixValuedKernelType* sumKernel = new ScaledKernel<vtkPoint>(mvKernel, params[1]);
    //iterate over the remaining kernel parameters
    for (unsigned int i = 1; i < kernels.size();i++) {
      params = kernels[i];
      GaussianKernel* gkNew = new GaussianKernel(params[0]);
      MatrixValuedKernelType* mvGk = new UncorrelatedMatrixValuedKernel<vtkPoint>(gkNew, model->GetRepresenter()->GetDimensions());
      MatrixValuedKernelType* scaledGk = new ScaledKernel<vtkPoint>(mvGk, params[1]);
      //keep track of allocated objects
      gKerns.push_back(gkNew);
      mKerns.push_back(mvGk);
      mKerns.push_back(scaledGk);
      sumKernel = new SumKernel<vtkPoint>(sumKernel, scaledGk);
      
    }
    if (useEmp) {
      // get the empiric kernel
      MatrixValuedKernelType* statModelKernel = new StatisticalModelKernel<vtkPolyData>(model.get());
      mKerns.push_back(statModelKernel);
      // add the empiric kernel on top
      sumKernel = new SumKernel<vtkPoint>(sumKernel, statModelKernel);
    }
    mKerns.push_back(sumKernel);
    //build new model
    auto_ptr<ModelBuilderType> modelBuilder(ModelBuilderType::Create(model->GetRepresenter()));
    auto_ptr<StatisticalModelType> combinedModel(modelBuilder->BuildNewModel(model->DrawMean(), *sumKernel, numberOfComponents,nystroem));
    //tidy up
    for (std::list<MatrixValuedKernelType*>::iterator it = mKerns.begin(); it != mKerns.end(); it++) {
      if (*it != NULL) {
	delete *it;
      }
    }
    for (std::list<GaussianKernel*>::iterator it = gKerns.begin(); it != gKerns.end(); it++) {
      if (*it != NULL) {
	delete *it;
      }
    }
    return combinedModel;
  
  } catch (StatisticalModelException& e) {
    ::Rf_error("Exception occured while building the shape model\n");
    ::Rf_error("%s\n",  e.what());
    auto_ptr<StatisticalModelType> model;
    return model;
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    auto_ptr<StatisticalModelType> model;
    return model;
  } catch (...) {
    ::Rf_error("unknown exception");
    auto_ptr<StatisticalModelType> model;
    return model;
  }
}

RcppExport SEXP BuildGPModelExport(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_,SEXP useEmp_){
  
  auto_ptr<StatisticalModelType> model = BuildGPModel(pPCA_,kernels_,ncomp_,nystroem_,useEmp_);
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
