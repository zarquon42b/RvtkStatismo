#include "BuildModel.h"
typedef PCAModelBuilder<vtkPolyData> ModelBuilderType;


SEXP BuildModelExport(SEXP myshapelist_,SEXP myreference_,SEXP sigma_) {
  
  shared_ptr<vtkMeshModel> model = BuildModel(myshapelist_,myreference_, sigma_);
  return statismo2pPCA(model);
  
}

  
shared_ptr<vtkMeshModel> BuildModel(SEXP myshapelist_,SEXP myreference_,SEXP sigma_){

  try {
    List myshapelist(myshapelist_);
    List myreference(myreference_);
    double sigma = as<double>(sigma_);
    unsigned int ndata = myshapelist.size();
    std::vector<std::string> nam = myshapelist.names();
    SEXP vbref = myreference["vb"];
    SEXP itref = myreference["it"];
    vtkSmartPointer<vtkPolyData> reference = R2polyData(vbref,itref);
    shared_ptr<vtkMeshRepresenter> representer(vtkMeshRepresenter::Create(reference));
  
  
    shared_ptr<vtkMeshDataManager> dataManager(vtkMeshDataManager::Create(representer.get()));
    for (unsigned int i = 0; i < ndata; i++) {
      List tmplist = myshapelist[i];
      //IntegerMatrix
      SEXP vb = tmplist["vb"];
      SEXP it = tmplist["it"];
      vtkSmartPointer<vtkPolyData> dataset = R2polyData(vb,it);
      std::string myname = nam[i];
      dataManager->AddDataset(dataset,myname);
    
    }
    shared_ptr<ModelBuilderType> modelBuilder(ModelBuilderType::Create());
    shared_ptr<vtkMeshModel> model(modelBuilder->BuildNewModel(dataManager->GetData(), sigma));
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
