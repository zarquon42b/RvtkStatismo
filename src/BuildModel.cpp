#include "BuildModel.h"
typedef PCAModelBuilder<vtkPolyData> ModelBuilderType;


SEXP BuildModelExport(SEXP myshapelist_,SEXP myreference_,SEXP sigma_, SEXP computeScores_, SEXP SelfAdjointSolve_,  SEXP pointer_) {
  try {
    bool pointer = as<bool>(pointer_);
    XPtr<vtkMeshModel> model = BuildModel(myshapelist_,myreference_, sigma_, computeScores_, SelfAdjointSolve_);
    return statismo2pPCA(model,pointer);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

  
XPtr<vtkMeshModel> BuildModel(SEXP myshapelist_,SEXP myreference_,SEXP sigma_, SEXP computeScores_, SEXP SelfAdjointSolve_){

  try {
    List myshapelist(myshapelist_);
    List myreference(myreference_);
    double sigma = as<double>(sigma_);
    bool computeScores = as<bool>(computeScores_);
    bool SelfAdjointSolve = as<bool>(SelfAdjointSolve_);
    unsigned int ndata = myshapelist.size();
    std::vector<std::string> nam = myshapelist.names();
    SEXP vbref = myreference["vb"];
    SEXP itref = myreference["it"];
    vtkSmartPointer<vtkPolyData> reference = R2polyData(vbref,itref);
    XPtr<vtkMeshRepresenter> representer(vtkMeshRepresenter::Create(reference));
    //enum method { JacobiSVD, SelfAdjointEigenSolver } ;
    ModelBuilderType::EigenValueMethod method = ModelBuilderType::JacobiSVD;
    if (SelfAdjointSolve)
      method =  ModelBuilderType::SelfAdjointEigenSolver;
    
    XPtr<vtkMeshDataManager> dataManager(vtkMeshDataManager::Create(representer.get()));
    //#pragma omp parallel for schedule(static)
    for (unsigned int i = 0; i < ndata; i++) {
      List tmplist = myshapelist[i];
      //IntegerMatrix
      SEXP vb = tmplist["vb"];
      SEXP it = tmplist["it"];
      vtkSmartPointer<vtkPolyData> dataset = R2polyData(vb,it);
      std::string myname = nam[i];
      dataManager->AddDataset(dataset,myname);
      dataset = NULL;
    }
    XPtr<ModelBuilderType> modelBuilder(ModelBuilderType::Create());
    
    XPtr<vtkMeshModel> model(modelBuilder->BuildNewModel(dataManager->GetData(), sigma,computeScores, method));
    return model;

  } catch (StatisticalModelException& e) {
    ::Rf_error("Exception occured while building the shape model\n");
    ::Rf_error("%s\n",  e.what());
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}
