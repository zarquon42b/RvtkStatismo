#include "VTKTypes.h"
#include "pPCA2statismo.h"
#include "polyData2R.h"

RcppExport SEXP DrawMean(SEXP pPCA_){
  try {
  auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
  vtkSmartPointer<vtkPolyData> reference = model->DrawMean();
  List out = polyData2R(reference);
  return out;
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}

RcppExport SEXP LoadModel(SEXP modelname_){
  try {
    CharacterVector modelname(modelname_);
    vtkStandardMeshRepresenter* representer = vtkStandardMeshRepresenter::Create();
    std::string modelFilename = as<std::string>(modelname);
  
    auto_ptr<StatisticalModelType> model(StatisticalModelType::Load(representer, modelFilename));
    List out = statismo2pPCA(model);
    return out;
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}

RcppExport SEXP ComputeLogProbabilityOfDataset(SEXP pPCA_, SEXP dataset_, SEXP getlog_){
  try {
    bool getlog = as<bool>(getlog_);
    List dataset(dataset_);
    double prob;
    const vtkSmartPointer<vtkPolyData> datasetRef = R2vtk(dataset["vb"],dataset["it"]);
    auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    if (getlog)
      prob = model->ComputeLogProbabilityOfDataset(datasetRef);
    else
      prob = model->ComputeProbabilityOfDataset(datasetRef);
    //return List::create(Named
    return wrap(prob);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }

}

RcppExport SEXP ComputeCoefficientsForDataset(SEXP pPCA_, SEXP dataset_){
  try {
    List dataset(dataset_);
    double prob;
    const vtkSmartPointer<vtkPolyData> datasetRef = R2vtk(dataset["vb"],dataset["it"]);
    auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    Eigen::VectorXf out = model->ComputeCoefficientsForDataset(datasetRef);	
    //return List::create(Named
    return wrap(out);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }

}
