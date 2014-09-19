#include "ModelMembers.h"
#include "Helpers.h"

using namespace Rcpp;
using namespace Eigen;

SEXP DrawMean(SEXP pPCA_){
  try {
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
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

SEXP DrawMeanAtPoint(SEXP pPCA_, SEXP meanpt_){
  try {
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    vtkPoint pt = SEXP2vtkPoint(meanpt_);
    vtkPoint samplept = model->DrawMeanAtPoint(pt);
    
    return NumericVector(&samplept[0],&samplept[3]);

  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}

SEXP DrawSample(SEXP pPCA_, SEXP coeffs_, SEXP addNoise_){
  try {
    bool addNoise = as<bool>(addNoise_);
    vtkSmartPointer<vtkPolyData> reference;
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    if (!Rf_isNull(coeffs_)) {
      
      Map<VectorXd> coeffs0(as<Map<VectorXd> >(coeffs_));
      const VectorXf coeffs = coeffs0.cast<float>();
      reference = model->DrawSample(coeffs,addNoise);
    } else {
      reference = model->DrawSample(addNoise);
    }
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

SEXP DrawSampleVector(SEXP pPCA_, SEXP coeffs_, SEXP addNoise_){
  try {
    bool addNoise = as<bool>(addNoise_);
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    Map<VectorXd> coeffs0(as<Map<VectorXd> >(coeffs_));
    const VectorXf coeffs = coeffs0.cast<float>();
    VectorXf out = model->DrawSampleVector(coeffs,addNoise);
   
    return wrap(out);

  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}
SEXP DrawSampleAtPoint(SEXP pPCA_, SEXP coeffs_, SEXP meanpt_, SEXP addNoise_){
  try {
    bool addNoise = as<bool>(addNoise_);
    vtkSmartPointer<vtkPolyData> reference;
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    
    Map<VectorXd> coeffs0(as<Map<VectorXd> >(coeffs_));
    const VectorXf coeffs = coeffs0.cast<float>();
    vtkPoint pt = SEXP2vtkPoint(meanpt_);
    vtkPoint samplept = model->DrawSampleAtPoint(coeffs,pt,addNoise);
    return NumericVector(&samplept[0],&samplept[3]);
    
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}



SEXP LoadModel(SEXP modelname_){
  try {
    CharacterVector modelname(modelname_);
    vtkStandardMeshRepresenter* representer = vtkStandardMeshRepresenter::Create();
    std::string modelFilename = as<std::string>(modelname);
  
    shared_ptr<StatisticalModelType> model(StatisticalModelType::Load(representer, modelFilename));
    S4 out = statismo2pPCA(model);
    return out;
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}

SEXP ComputeLogProbabilityOfDataset(SEXP pPCA_, SEXP dataset_, SEXP getlog_){
  try {
    bool getlog = as<bool>(getlog_);
    List dataset(dataset_);
    double prob;
    const vtkSmartPointer<vtkPolyData> datasetRef = R2polyData(dataset["vb"],dataset["it"]);
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
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

SEXP ComputeCoefficientsForDataset(SEXP pPCA_, SEXP dataset_){
  try {
    List dataset(dataset_);
    double prob;
    const vtkSmartPointer<vtkPolyData> datasetRef = R2polyData(dataset["vb"],dataset["it"]);
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
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

typedef std::vector<vtkPoint> DomainPointsListType;
SEXP GetDomainPoints(SEXP pPCA_) {
  try {
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    const DomainPointsListType domainPoints = model->GetDomain().GetDomainPoints();
    unsigned int siz = model->GetDomain().GetNumberOfPoints();
    NumericMatrix out(3,siz);
    NumericVector test1;
    
    for (unsigned int i = 0; i < siz; i++) {
      const double *a = domainPoints[i].data();
      out(_,i) = NumericVector(&a[0],&a[3]);
    }
    return out;
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}

  
SEXP GetCovarianceAtPointId(SEXP pPCA_, SEXP pt1_, SEXP pt2_) {
  try {
    unsigned int ptId1 = as<unsigned int>(pt1_);
    unsigned int ptId2 = as<unsigned int>(pt2_);
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    MatrixXf cov = model->GetCovarianceAtPoint(ptId1,ptId2);
  
    return wrap(cov);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }

}
SEXP GetCovarianceAtPointPt(SEXP pPCA_, SEXP pt1_, SEXP pt2_) {
  try {
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    vtkPoint pt1 = SEXP2vtkPoint(pt1_);
    vtkPoint pt2 = SEXP2vtkPoint(pt2_);
    MatrixXf cov =model->GetCovarianceAtPoint(pt1,pt2);
  
    return wrap(cov);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }

}
SEXP GetCovarianceMatrix(SEXP pPCA_) {
  try {
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    MatrixXf cov = model->GetCovarianceMatrix();
  
    return wrap(cov);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}

SEXP GetJacobian(SEXP pPCA_, SEXP pt_) {
  try {
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    vtkPoint pt = SEXP2vtkPoint(pt_);
    MatrixXf cov = model->GetJacobian(pt);
  
    return wrap(cov);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}

typedef std::pair<vtkPoint, vtkPoint> PointValuePairType;
typedef std::list<PointValuePairType> PointValueListType;
SEXP ComputeCoefficientsForPointValues(SEXP pPCA_, SEXP sample_, SEXP mean_, SEXP noise_) {
  try {
    double noise = as<double>(noise_);
    NumericMatrix sample(sample_);
    NumericMatrix mean(mean_);
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    PointValueListType ptValueList;
    for (int i = 0; i < mean.ncol();i++) {
      
      vtkPoint tmp0 = SEXP2vtkPoint(wrap(sample(_,i)));
      vtkPoint tmp1 = SEXP2vtkPoint(wrap(mean(_,i)));
      
      ptValueList.push_back(PointValuePairType(tmp1,tmp0));
    }
    
    VectorXf coeff = model->ComputeCoefficientsForPointValues(ptValueList,noise);
    
  
    return wrap(coeff);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}

SEXP EvaluateSampleAtPoint(SEXP pPCA_, SEXP dataset_, SEXP meanpt_) {
  try {
    List dataset(dataset_);
    const vtkSmartPointer<vtkPolyData> sample = R2polyData(dataset["vb"],dataset["it"]);
    shared_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    vtkPoint pt = SEXP2vtkPoint(meanpt_);
    vtkPoint samplept = model->EvaluateSampleAtPoint(sample,pt);
    return NumericVector(&samplept[0],&samplept[3]);
    
  
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}
