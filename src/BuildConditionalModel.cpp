#include "BuildConditionalModel.h"
#include "ConditionalModelBuilderCustom.h"

typedef ConditionalModelBuilderCustom<vtkPolyData> ModelBuilderType;
typedef DataManagerWithSurrogateVector<vtkPolyData> vtkMeshDataManagerWithSurrogates;


SEXP BuildConditionalModelExport(SEXP myshapelist_,SEXP myreference_,SEXP sigma_,SEXP trainingData_, SEXP condData_,SEXP surrogateInfo_, SEXP exVar_) {
  
  XPtr<vtkMeshModel> model = BuildConditionalModel(myshapelist_,myreference_, sigma_, trainingData_,condData_,surrogateInfo_,exVar_);
  return statismo2pPCA(model);
  
}

  
XPtr<vtkMeshModel> BuildConditionalModel(SEXP myshapelist_,SEXP myreference_,SEXP sigma_,SEXP trainingData_, SEXP condData_,SEXP surrogateInfo_,SEXP exVar_) {

  try {
    List myshapelist(myshapelist_);
    List myreference(myreference_);
    NumericMatrix trainingData(trainingData_);
    if (myshapelist.size() != trainingData.nrow())
      ::Rf_error("each input shape must have corresponding conditioning variables");
    
    Map<VectorXd> surrogateInfo1(as<Map<VectorXd> >(surrogateInfo_)); 
    VectorXf surrogateInfo = surrogateInfo1.cast<float>();
    NumericMatrix condData(condData_); 
    if (condData.nrow() != trainingData.ncol())
      ::Rf_error("conditioning data must have same amount of variables as training data");
    double sigma = as<double>(sigma_);
    double exVar = as<double>(exVar_);
    unsigned int ndata = myshapelist.size();
    std::vector<std::string> nam = myshapelist.names();
    SEXP vbref = myreference["vb"];
    SEXP itref = myreference["it"];
    vtkSmartPointer<vtkPolyData> reference = R2polyData(vbref,itref);
    XPtr<vtkMeshRepresenter> representer(vtkMeshRepresenter::Create(reference));
    ModelBuilderType::CondVariableValueVectorType conditioningInfo;
    for (unsigned int i = 0; i < condData.nrow(); i++) {
      bool use = condData(i,0);
      float value = condData(i,1);
      conditioningInfo.push_back(ModelBuilderType::CondVariableValuePair(true, value));
    }
  
    XPtr<vtkMeshDataManagerWithSurrogates> dataManager(vtkMeshDataManagerWithSurrogates::Create(representer.get(),surrogateInfo));
    for (unsigned int i = 0; i < ndata; i++) {
      List tmplist = myshapelist[i];
      //IntegerMatrix
      SEXP vb = tmplist["vb"];
      SEXP it = tmplist["it"];
      vtkSmartPointer<vtkPolyData> dataset = R2polyData(vb,it);
      std::string myname = nam[i];
      NumericVector tmp0 = trainingData(i,_);
      Map<VectorXd> tmpVector0 =  as<Map<VectorXd> >(tmp0);
      VectorXf tmpVector= tmpVector0.cast<float>();
      dataManager->AddDatasetWithSurrogates(dataset,myname,tmpVector);
    
      }
    XPtr<ModelBuilderType> modelBuilder(ModelBuilderType::Create());
    XPtr<vtkMeshModel> model(modelBuilder->BuildNewModel(dataManager->GetData(), dataManager->GetSurrogateTypeInfo(), conditioningInfo, sigma,exVar));
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
