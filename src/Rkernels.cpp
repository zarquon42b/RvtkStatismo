#include "BuildGaussProcessModel.h"
#include "Helpers.h"


RcppExport SEXP RscalarValuedKernel(SEXP kerneltype_, SEXP sigma_, SEXP levels_) {
  try {
    int kerneltype = as<int>(kerneltype_);
    int sigma= as<int>(sigma_);
    int levels = as<int>(levels_);
    
    if (kerneltype == 0) { // case Gaussian kernel
      XPtr< ScalarValuedKernel<vtkPoint> > scalarKernel(new GaussianKernel(sigma));
      return scalarKernel;
    } else if (kerneltype == 1) { //bspline kerneltype
      XPtr< ScalarValuedKernel<vtkPoint> > scalarKernel(new MultiscaleKernel(sigma,levels));
      return scalarKernel;
    } else {
      ::Rf_error("unsupported kerneltype");
    }
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}
    
RcppExport SEXP RMatrixValuedKernel(SEXP scalarKernel_, SEXP scale_) {
  try {
    XPtr<ScalarValuedKernel<vtkPoint> > scalarKernel(scalarKernel_);
    double scale = as<double>(scale_);
    XPtr<MatrixValuedKernelType> matrixKernel(new UncorrelatedMatrixValuedKernel<vtkPoint>(scalarKernel,3));
    XPtr<MatrixValuedKernelType> matrixKernelScaled(new ScaledKernel<vtkPoint>(matrixKernel,scale));
    Language matrixKernelCall("new", "matrixKernel");
    S4 matrixKernelOut( matrixKernelCall.eval() );
    matrixKernelOut.slot("pointer") = matrixKernelScaled;
    //return matrixKernelScaled;
    return matrixKernelOut;
       
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

RcppExport SEXP RisoKernel(SEXP scale_, SEXP centroid_) {
  try {
    double scale = as<double>(scale_);
    vtkPoint centroid = SEXP2vtkPoint(centroid_);
    XPtr<MatrixValuedKernelType> matrixKernelScaled(new IsoKernel(3,scale,centroid));
    Language matrixKernelCall("new", "matrixKernel");
    S4 matrixKernel( matrixKernelCall.eval() );
    matrixKernel.slot("pointer") = matrixKernelScaled;
    //return matrixKernelScaled;
    return matrixKernel;
      
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

RcppExport SEXP RgetEmpiricalKernel(SEXP pPCA_) {
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    XPtr<MatrixValuedKernelType> statModelKernel(new StatisticalModelKernel<vtkPolyData>(model.get()));
    return statModelKernel;
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }						

}


RcppExport SEXP RcombineKernels(SEXP mkernel1_, SEXP mkernel2_, SEXP add_) {
  try {
    XPtr<MatrixValuedKernelType> mkernel1(mkernel1_);
    XPtr<MatrixValuedKernelType> mkernel2(mkernel2_);
    double add = as<double>(add_);
    Language matrixKernelCall("new", "matrixKernel");
    S4 combinedKernelR( matrixKernelCall.eval() );
    if (add) {
      XPtr<MatrixValuedKernelType> combinedKernel(new SumKernel<vtkPoint>(mkernel1,mkernel2));
      combinedKernelR.slot("pointer") = combinedKernel;
      return combinedKernelR;
    //return matrixKernelScaled;
    return combinedKernelR;
    } else {
      XPtr<MatrixValuedKernelType> combinedKernel(new ProductKernel<vtkPoint>(mkernel1,mkernel2));
      combinedKernelR.slot("pointer") = combinedKernel;
      return combinedKernelR;
    }
    
   
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}
