#include "BuildGaussProcessModel.h"
#include "Helpers.h"

XPtr<vtkMeshModel> BuildGPModel(XPtr<vtkMeshModel> model, SEXP mykernel_, SEXP ncomp_,SEXP nystroem_, SEXP combine_) {
  try {
    unsigned int nystroem = as<unsigned int>(nystroem_);
    unsigned int numberOfComponents = as<unsigned int>(ncomp_);
    int combine = as<int>(combine_);

    std::string classname = getClassname(mykernel_);
    MatrixValuedKernelType* mvKernel;
    MatrixValuedKernelType* sumKernel;
    if (classname != "combinedKernel") {
      if (classname == "GaussianKernel") {
	S4 GaussianKernelR(mykernel_);
	GaussianKernel* gk = new GaussianKernel(as<double>(GaussianKernelR.slot("sigma")));
	mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	sumKernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(GaussianKernelR.slot("scale")));
      } else if (classname == "BsplineKernel") {
	S4 BsplineKernelR(mykernel_);
	MultiscaleKernel* gk = new MultiscaleKernel(as<double>(BsplineKernelR.slot("support")),as<int>(BsplineKernelR.slot("levels")));
	mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	sumKernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(BsplineKernelR.slot("scale")));
      } else if (classname == "BsplineKernel") {
	S4 IsoKernelR(mykernel_);
	vtkPoint centroid = SEXP2vtkPoint(IsoKernelR.slot("centroid"));
	sumKernel = new IsoKernel(3,as<double>(IsoKernelR.slot("scale")), centroid);
      }
    } else {//deal with a list of kernels
      MatrixValuedKernelType* tmpKernel;
      S4 combinedKernelR(mykernel_);
      List kernellist(combinedKernelR.slot("kernels"));
      int listsize = kernellist.size();
      // get the first kernel 
      List additiveKernels = kernellist[0];
      std::string classnametmp = getClassname(additiveKernels[0]);
      if (classnametmp == "GaussianKernel") {
	S4 GaussianKernelR(additiveKernels[0]);
	GaussianKernel* gk = new GaussianKernel(as<double>(GaussianKernelR.slot("sigma")));
	mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	sumKernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(GaussianKernelR.slot("scale")));
      } else if (classnametmp == "BsplineKernel") {
	S4 BsplineKernelR(additiveKernels[0]);
	MultiscaleKernel* gk = new MultiscaleKernel(as<double>(BsplineKernelR.slot("support")),as<int>(BsplineKernelR.slot("levels")));
	mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	sumKernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(BsplineKernelR.slot("scale")));
      } else if (classname == "IsoKernel") {
	S4 IsoKernelR(additiveKernels[0]);
	vtkPoint centroid = SEXP2vtkPoint(IsoKernelR.slot("centroid"));
	sumKernel = new IsoKernel(3,as<double>(IsoKernelR.slot("scale")), centroid);
      } // now process the first list
      for (unsigned int j = 1; j < additiveKernels.size();j++) {
	MatrixValuedKernelType* tmpKernel;
	std::string classnametmp1 = getClassname(additiveKernels[j]);
	if (classnametmp1 == "GaussianKernel") {
	  S4 GaussianKernelR(additiveKernels[j]);
	  GaussianKernel* gk = new GaussianKernel(as<double>(GaussianKernelR.slot("sigma")));
	  mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	  tmpKernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(GaussianKernelR.slot("scale")));
	} else if (classnametmp1 == "BsplineKernel") {
	  S4 BsplineKernelR(additiveKernels[j]);
	  MultiscaleKernel* gk = new MultiscaleKernel(as<double>(BsplineKernelR.slot("support")),as<int>(BsplineKernelR.slot("levels")));
	  mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	  tmpKernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(BsplineKernelR.slot("scale")));
	} else if (classname == "IsoKernel") {
	  S4 IsoKernelR(additiveKernels[j]);
	  vtkPoint centroid = SEXP2vtkPoint(IsoKernelR.slot("centroid"));
	  tmpKernel = new IsoKernel(3,as<double>(IsoKernelR.slot("scale")), centroid);
	}
	sumKernel = new SumKernel<vtkPoint>(sumKernel, tmpKernel);
      }
	//
      for (unsigned int i = 1; i < listsize;i++) {
	MatrixValuedKernelType* sum1Kernel;
	
	additiveKernels = kernellist[i];
	classname = getClassname(additiveKernels[0]);
	if (classname == "GaussianKernel") {
	  S4 GaussianKernelR(additiveKernels[0]);
	  GaussianKernel* gk = new GaussianKernel(as<double>(GaussianKernelR.slot("sigma")));
	  mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	  sum1Kernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(GaussianKernelR.slot("scale")));
	} else if (classname == "BsplineKernel") {
	  S4 BsplineKernelR(additiveKernels[0]);
	  MultiscaleKernel* gk = new MultiscaleKernel(as<double>(BsplineKernelR.slot("support")),as<int>(BsplineKernelR.slot("levels")));
	  mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	  sum1Kernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(BsplineKernelR.slot("scale")));
	} else if (classname == "IsoKernel") {
	  S4 IsoKernelR(additiveKernels[0]);
	  vtkPoint centroid = SEXP2vtkPoint(IsoKernelR.slot("centroid"));
	  sum1Kernel = new IsoKernel(3,as<double>(IsoKernelR.slot("scale")), centroid);
	}
	
	for (unsigned int j = 1; j < additiveKernels.size();j++){
	  classname = getClassname(additiveKernels[j]);
	  if (classname == "GaussianKernel") {
	    S4 GaussianKernelR(additiveKernels[j]);
	    GaussianKernel* gk = new GaussianKernel(as<double>(GaussianKernelR.slot("sigma")));
	    mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	    tmpKernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(GaussianKernelR.slot("scale")));
	  } else if (classname == "BsplineKernel") {
	    S4 BsplineKernelR(additiveKernels[j]);
	    MultiscaleKernel* gk = new MultiscaleKernel(as<double>(BsplineKernelR.slot("support")),as<int>(BsplineKernelR.slot("levels")));
	    mvKernel = new UncorrelatedMatrixValuedKernel<vtkPoint>(gk, model->GetRepresenter()->GetDimensions());
	    tmpKernel = new ScaledKernel<vtkPoint>(mvKernel,as<double>(BsplineKernelR.slot("scale")));
	  } else if (classname == "IsoKernel") {
	    S4 IsoKernelR(additiveKernels[j]);
	    vtkPoint centroid = SEXP2vtkPoint(IsoKernelR.slot("centroid"));
	    tmpKernel = new IsoKernel(3,as<double>(IsoKernelR.slot("scale")), centroid);
	  }
	  sum1Kernel = new SumKernel<vtkPoint>(sum1Kernel, tmpKernel);
	}
	sumKernel = new ProductKernel<vtkPoint>(sumKernel,sum1Kernel);
	}
    }
  
  
    
    MatrixValuedKernelType* statModelKernel = new StatisticalModelKernel<vtkPolyData>(model.get());
    if (combine == 1 )
      sumKernel = new SumKernel<vtkPoint>(sumKernel, statModelKernel);
    else if (combine == 2)
      sumKernel = new ProductKernel<vtkPoint>(sumKernel, statModelKernel);
  
    XPtr<ModelBuilderType> modelBuilder(ModelBuilderType::Create(model->GetRepresenter()));
    XPtr<vtkMeshModel> combinedModel(modelBuilder->BuildNewModel(model->DrawMean(), *sumKernel, numberOfComponents,nystroem));
    
    delete statModelKernel;
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

SEXP BuildGPModelExport(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_, SEXP combine_, SEXP pointer_) {
  try {
    bool pointer = as<bool>(pointer_);
    XPtr<vtkMeshModel> modelin = pPCA2statismo(pPCA_);
    XPtr<vtkMeshModel> model = BuildGPModel(modelin,kernels_,ncomp_,nystroem_,combine_);
    return statismo2pPCA(model,pointer);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

