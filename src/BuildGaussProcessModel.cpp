#include "BuildGaussProcessModel.h"
#include "Helpers.h"

XPtr<vtkMeshModel> BuildGPModel(XPtr<vtkMeshModel> model, SEXP mykernel_, SEXP ncomp_,SEXP nystroem_) {
  try {
    unsigned int nystroem = as<unsigned int>(nystroem_);
    unsigned int numberOfComponents = as<unsigned int>(ncomp_);
    //setup lists for keeping track of allocated pointers
    std::list<MatrixValuedKernelType*> mKerns;
    std::list<ScalarValuedKernel<vtkPoint>*> gKerns;

    std::string classname = getClassname(mykernel_);
    if (classname != "CombinedKernel")
      ::Rf_error("pleas provid a valid kernel");
    MatrixValuedKernelType* mvKernel;
    // create a multiplicative neutral matrixkernel
    MatrixValuedKernelType* productKernel = new NeutralProductKernel();
    
    MatrixValuedKernelType* tmpKernel;
    S4 CombinedKernelR(mykernel_);
    List kernellist(CombinedKernelR.slot("kernels"));
    int listsize = kernellist.size();
    List additiveKernels;
    // create an additive neutral matrixkernel
    MatrixValuedKernelType* addKernel = new NeutralSumKernel();
    // iterate over a list containing lists of kernels
    for (unsigned int i = 0; i < listsize;i++) {
      MatrixValuedKernelType* addKernel = new NeutralSumKernel();
      additiveKernels = kernellist[i];
      for (unsigned int j = 0; j < additiveKernels.size();j++){
	classname = getClassname(additiveKernels[j]);
	if (classname == "GaussianKernel") {
	  S4 GaussianKernelR(additiveKernels[j]);
	  GaussianKernel* gk = new GaussianKernel(as<double>(GaussianKernelR.slot("sigma")));
	  mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	  tmpKernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(GaussianKernelR.slot("scale")));
	  gKerns.push_back(gk);
	  mKerns.push_back(mvKernel);
	} else if (classname == "BSplineKernel") {
	  S4 BSplineKernelR(additiveKernels[j]);
	  BSplineKernel* gk = new BSplineKernel(as<double>(BSplineKernelR.slot("support")));
	  mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	  tmpKernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(BSplineKernelR.slot("scale")));
	  gKerns.push_back(gk);
	  mKerns.push_back(mvKernel);
	} else if (classname == "MultiscaleBSplineKernel") {
	  S4 MultiscaleBSplineKernelR(additiveKernels[j]);
	  MultiscaleKernel* gk = new MultiscaleKernel(as<double>(MultiscaleBSplineKernelR.slot("support")),as<int>(MultiscaleBSplineKernelR.slot("levels")));
	  mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	  tmpKernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(MultiscaleBSplineKernelR.slot("scale")));
	  gKerns.push_back(gk);
	  mKerns.push_back(mvKernel);
	} else if (classname == "IsoKernel") {
	  S4 IsoKernelR(additiveKernels[j]);
	  vtkPoint centroid = SEXP2vtkPoint(IsoKernelR.slot("centroid"));
	  tmpKernel = new IsoKernel(3,as<double>(IsoKernelR.slot("scale")), centroid);
	} else if (classname == "StatisticalModelKernel") {
	  tmpKernel = new StatisticalModelKernel<vtkPolyData>(model.get());
	}
	addKernel = new SumKernel<vtkPoint>(addKernel, tmpKernel);
      }
      productKernel = new ProductKernel<vtkPoint>(productKernel,addKernel);
      mKerns.push_back(addKernel);
      mKerns.push_back(tmpKernel);
    }
    
    mKerns.push_back(productKernel);
    // build the model
    XPtr<ModelBuilderType> modelBuilder(ModelBuilderType::Create(model->GetRepresenter()));
    XPtr<vtkMeshModel> combinedModel(modelBuilder->BuildNewModel(model->DrawMean(), *productKernel, numberOfComponents,nystroem));    
    // house cleaning
    for (std::list<ScalarValuedKernel<vtkPoint>*>::iterator it = gKerns.begin(); it != gKerns.end(); it++) {
      if (*it != NULL) {
	delete *it;
      }
    }
    for (std::list<MatrixValuedKernelType*>::iterator it = mKerns.begin(); it != mKerns.end(); it++) {
      if (*it != NULL) {
	delete *it;
      }
    }
    // end house cleaning
    
    return combinedModel;
  
  } catch (StatisticalModelException& e) {
    ::Rf_error("Exception occured while building the shape model\n");
    ::Rf_error("%s\n",  e.what());
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

SEXP BuildGPModelExport(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_, SEXP pointer_) {
  try {
    bool pointer = as<bool>(pointer_);
    XPtr<vtkMeshModel> modelin = pPCA2statismo(pPCA_);
    XPtr<vtkMeshModel> model = BuildGPModel(modelin,kernels_,ncomp_,nystroem_);
    return statismo2pPCA(model,pointer);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

