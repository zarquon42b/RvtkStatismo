#include "BuildModel.h"
typedef PCAModelBuilder<vtkPolyData> ModelBuilderType;


SEXP BuildModelExport(SEXP myshapelist_,SEXP myreference_,SEXP sigma_) {
  
  auto_ptr<StatisticalModelType> model = BuildModel(myshapelist_,myreference_, sigma_);
  return statismo2pPCA(model);
  
}

  
auto_ptr<StatisticalModelType> BuildModel(SEXP myshapelist_,SEXP myreference_,SEXP sigma_){
  List myshapelist(myshapelist_);
  List myreference(myreference_);
  double sigma = as<double>(sigma_);
  unsigned int ndata = myshapelist.size();
  std::vector<std::string> nam = myshapelist.names();
  SEXP vbref = myreference["vb"];
  SEXP itref = myreference["it"];
  vtkSmartPointer<vtkPolyData> reference = R2vtk(vbref,itref);
  auto_ptr<RepresenterType> representer(RepresenterType::Create(reference));
  try{
  
  auto_ptr<DataManagerType> dataManager(DataManagerType::Create(representer.get()));
  for (unsigned int i = 0; i < ndata; i++) {
    List tmplist = myshapelist[i];
    //IntegerMatrix
    SEXP vb = tmplist["vb"];
    SEXP it = tmplist["it"];
    vtkSmartPointer<vtkPolyData> dataset = R2vtk(vb,it);
    std::string myname = nam[i];
    dataManager->AddDataset(dataset,myname);
    
  }
  auto_ptr<ModelBuilderType> modelBuilder(ModelBuilderType::Create());
  auto_ptr<StatisticalModelType> model(modelBuilder->BuildNewModel(dataManager->GetData(), sigma));
  return model;

  }
  catch (StatisticalModelException& e) {
    Rprintf("Exception occured while building the shape model\n");
    Rprintf("%s\n",  e.what());
    auto_ptr<StatisticalModelType> model;
    return model;
  }
}
