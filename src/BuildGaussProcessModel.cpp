#include "BuildGaussProcessModel.h"
#include "Helpers.h"

XPtr<vtkMeshModel> BuildGPModel(XPtr<vtkMeshModel> model, SEXP mykernel_, SEXP ncomp_,SEXP nystroem_, SEXP combine_) {
  try {
    unsigned int nystroem = as<unsigned int>(nystroem_);
    unsigned int numberOfComponents = as<unsigned int>(ncomp_);
    XPtr<MatrixValuedKernelType > mykernel(mykernel_);
    int combine = as<int>(combine_);
    MatrixValuedKernelType* sumKernel = (&*mykernel);
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

