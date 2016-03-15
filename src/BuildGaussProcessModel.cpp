#include "BuildGaussProcessModel.h"
#include "Helpers.h"

XPtr<vtkMeshModel> BuildGPModel(shared_ptr<vtkMeshModel> model, SEXP mykernel_, SEXP ncomp_,SEXP nystroem_) {
  try {
    // shared_ptr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    unsigned int nystroem = as<unsigned int>(nystroem_);
    unsigned int numberOfComponents = as<unsigned int>(ncomp_);
    XPtr<MatrixValuedKernelType > mykernel(mykernel_);
    //MatrixValuedKernelType* sumKernel = (&*mykernel);
    MatrixValuedKernelType* sumKernel = new SumKernel<vtkPoint>(mykernel,mykernel);
    
    //MatrixValuedKernelType* testKernel = *sumKernel;
    shared_ptr<ModelBuilderType> modelBuilder(ModelBuilderType::Create(model->GetRepresenter()));
    vtkPolyData* newmean = model->DrawMean();
    Rprintf("2\n");
    XPtr<vtkMeshModel> combinedModel(modelBuilder->BuildNewModel(newmean, *mykernel, numberOfComponents,nystroem));
   
    newmean->Delete();
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

SEXP BuildGPModelExport(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_){
 
  std::string classname = getClassname(pPCA_);
  Rprintf("%s\n",classname.c_str());
  if (("class") == "pPCA") {
	  shared_ptr<vtkMeshModel> modelin = pPCA2statismo(pPCA_);
	  shared_ptr<vtkMeshModel> model = BuildGPModel(modelin,kernels_,ncomp_,nystroem_);
	  return statismo2pPCA(model);
  } else {
    XPtr<vtkMeshModel> modelin = pPCA2statismo(pPCA_);
    // }
  //return statismo2pPCA(model);
  
  
}

