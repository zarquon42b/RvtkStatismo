#include "statismoReducedModel.h"


typedef ReducedVarianceModelBuilder<vtkPolyData> ModelBuilderType;

SEXP ReducedModel(SEXP pPCA_,SEXP npc_,SEXP exVar_, SEXP scores_) {
  try {
    bool computeScores = as<bool>(scores_);
    unsigned int npc = as<unsigned int>(npc_);
    double exVar = as<double>(exVar_);
    auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    //auto_ptr<StatisticalModelType> reducedModel;
    auto_ptr<ModelBuilderType> modelBuilder(ModelBuilderType::Create());
    if (npc > 0) {
      auto_ptr<StatisticalModelType> reducedModel(modelBuilder->BuildNewModelWithLeadingComponents(model.get(), npc,computeScores));
      return statismo2pPCA(reducedModel);
    
      
    } else {
      auto_ptr<StatisticalModelType> reducedModel(modelBuilder->BuildNewModelWithVariance(model.get(), exVar,computeScores));
      return statismo2pPCA(reducedModel);

    }
    //return wrap(1);
  
  } catch (StatisticalModelException& e) {
    ::Rf_error("Exception occured while building the shape model\n");
    ::Rf_error("%s\n",  e.what());
    return wrap(1);
    
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);   
  }
  
}
