#include "BuildGaussProcessModel.h"
#include "Helpers.h"

shared_ptr<vtkMeshModel> BuildGPModel(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_,SEXP useEmp_, SEXP combine_,SEXP combineEmp_, SEXP isoScale_, SEXP centroid_) {
  try { 
    bool useEmp = as<bool>(useEmp_);
    int combine = as<int>(combine_);
    int combineEmp = as<int>(combineEmp_);
    double isoScale = as<double>(isoScale_);
    unsigned int nystroem = as<unsigned int>(nystroem_);
    unsigned int numberOfComponents = as<unsigned int>(ncomp_);
    std::list<MatrixValuedKernelType*> mKerns;
    std::list<GaussianKernel*> gKerns;
    std::list<MultiscaleKernel*> bsKerns;
    vtkPoint centroid = SEXP2vtkPoint(centroid_);
    List kernels(kernels_);
  
    shared_ptr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    MatrixValuedKernelType* mvKernel;
    // set up the gaussian kernel to be incremented over a list of parameters
    NumericVector params = kernels[0];
    //if params[0] == 0 Gaussian Kernel
    //else Multiscale kernel
    if (params[0] == 0) {
      GaussianKernel* gk = new GaussianKernel(params[1]);
      gKerns.push_back(gk);
      mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
      
    } else {
      MultiscaleKernel* gk1 = new MultiscaleKernel(params[1],params[3]);
      bsKerns.push_back(gk1);
      mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk1, model->GetRepresenter()->GetDimensions());
      
    }
      MatrixValuedKernelType* sumKernel = new ScaledKernel<vtkPoint>(mvKernel, params[2]);
    //iterate over the remaining kernel parameters
    for (unsigned int i = 1; i < kernels.size();i++) {
      params = kernels[i];
      GaussianKernel* gkNew = new GaussianKernel(params[1]);
      MatrixValuedKernelType* mvGk = new UncorrelatedMatrixValuedKernel<vtkPoint>(gkNew, model->GetRepresenter()->GetDimensions());
      MatrixValuedKernelType* scaledGk = new ScaledKernel<vtkPoint>(mvGk, params[2]);
      //keep track of allocated objects
      gKerns.push_back(gkNew);
      mKerns.push_back(mvGk);
      mKerns.push_back(scaledGk);
      if (combine == 0)
	sumKernel = new SumKernel<vtkPoint>(sumKernel, scaledGk);
      else
	sumKernel = new ProductKernel<vtkPoint>(sumKernel, scaledGk);
    }
    if (useEmp) {
      // get the empiric kernel
      MatrixValuedKernelType* statModelKernel = new StatisticalModelKernel<vtkPolyData>(model.get());
      mKerns.push_back(statModelKernel);
      // add the empiric kernel on top
      if (combineEmp == 0)
	sumKernel = new SumKernel<vtkPoint>(sumKernel, statModelKernel);
      else
	sumKernel = new ProductKernel<vtkPoint>(sumKernel, statModelKernel);
    }
    if (isoScale > 0) {
      MatrixValuedKernelType* isoKernel = new IsoKernel(3,isoScale,centroid);
      mKerns.push_back(isoKernel);
      sumKernel = new SumKernel<vtkPoint>(sumKernel, isoKernel);
    }
    mKerns.push_back(sumKernel);
    //build new model
    shared_ptr<ModelBuilderType> modelBuilder(ModelBuilderType::Create(model->GetRepresenter()));
    shared_ptr<vtkMeshModel> combinedModel(modelBuilder->BuildNewModel(model->DrawMean(), *sumKernel, numberOfComponents,nystroem));
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
    for (std::list<MultiscaleKernel*>::iterator it = bsKerns.begin(); it != bsKerns.end(); it++) {
      if (*it != NULL) {
	delete *it;
      }
    }
    return combinedModel;
  
  } catch (StatisticalModelException& e) {
    ::Rf_error("Exception occured while building the shape model\n");
    ::Rf_error("%s\n",  e.what());
    //shared_ptr<vtkMeshModel> model(NULL);
    //return model;
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    //shared_ptr<vtkMeshModel> model(NULL);    
    //return model;
  } catch (...) {
    ::Rf_error("unknown exception");
    //shared_ptr<vtkMeshModel> model(NULL);
    //return model;
  }
}

RcppExport SEXP BuildGPModelExport(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_,SEXP useEmp_, SEXP combine_, SEXP combineEmp_,SEXP isoScale_,SEXP centroid_){
  
  shared_ptr<vtkMeshModel> model = BuildGPModel(pPCA_,kernels_,ncomp_,nystroem_,useEmp_,combine_,combineEmp_,isoScale_,centroid_);
  //return statismo2pPCA(model);
  return statismo2pPCA(model);
  
}

