#include "ModelMembers.h"
#include "Helpers.h"

using namespace Rcpp;
using namespace Eigen;

SEXP DrawMean(SEXP pPCA_){
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    vtkPolyData* reference = model->DrawMean();
    List out = polyData2R(reference);
    reference->Delete();
    return out;
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

SEXP DrawMeanAtPoint(SEXP pPCA_, SEXP meanpt_){
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    vtkPoint pt = SEXP2vtkPoint(meanpt_);
    vtkPoint samplept = model->DrawMeanAtPoint(pt);
    
    return NumericVector(&samplept[0],&samplept[3]);

  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

SEXP DrawSample(SEXP pPCA_, SEXP coeffs_, SEXP addNoise_){
  try {
    bool addNoise = as<bool>(addNoise_);
    vtkPolyData* reference;
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    if (!Rf_isNull(coeffs_)) {
      
      Map<VectorXd> coeffs0(as<Map<VectorXd> >(coeffs_));
      const VectorXf coeffs = coeffs0.cast<float>();
      reference = model->DrawSample(coeffs,addNoise);
    } else {
      reference = model->DrawSample(addNoise);
    }
    List out = polyData2R(reference);
    reference->Delete();
    return out;
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

SEXP DrawSampleVector(SEXP pPCA_, SEXP coeffs_, SEXP addNoise_){
  try {
    bool addNoise = as<bool>(addNoise_);
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    Map<VectorXd> coeffs0(as<Map<VectorXd> >(coeffs_));
    const VectorXf coeffs = coeffs0.cast<float>();
    VectorXf out = model->DrawSampleVector(coeffs,addNoise);
   
    return wrap(out);

  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}
SEXP DrawSampleAtPoint(SEXP pPCA_, SEXP coeffs_, SEXP meanpt_, SEXP addNoise_){
  try {
    bool addNoise = as<bool>(addNoise_);
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    
    Map<VectorXd> coeffs0(as<Map<VectorXd> >(coeffs_));
    const VectorXf coeffs = coeffs0.cast<float>();
    vtkPoint pt = SEXP2vtkPoint(meanpt_);
    vtkPoint samplept = model->DrawSampleAtPoint(coeffs,pt,addNoise);
    return NumericVector(&samplept[0],&samplept[3]);
    
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}



SEXP LoadModel(SEXP modelname_, SEXP pointer_){
  try {
    bool pointer = as<bool>(pointer_);
    CharacterVector modelname(modelname_);
    XPtr<vtkStandardMeshRepresenter> representer(vtkStandardMeshRepresenter::Create());
    std::string modelFilename = as<std::string>(modelname);
  
    XPtr<vtkMeshModel> model(vtkMeshModel::Load(representer.get(), modelFilename));
    return statismo2pPCA(model,pointer);
    
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

SEXP ComputeLogProbabilityOfDataset(SEXP pPCA_, SEXP dataset_, SEXP getlog_){
  try {
    bool getlog = as<bool>(getlog_);
    List dataset(dataset_);
    double prob;
    const vtkSmartPointer<vtkPolyData> datasetRef = R2polyData(dataset["vb"],dataset["it"]);
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    if (getlog)
      prob = model->ComputeLogProbabilityOfDataset(datasetRef);
    else
      prob = model->ComputeProbabilityOfDataset(datasetRef);
    //return List::create(Named
    return wrap(prob);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }

}
SEXP ComputeMahalanobisDistanceForDataset(SEXP pPCA_, SEXP dataset_) {
  try {
    List dataset(dataset_);
    double mahadist;
    const vtkSmartPointer<vtkPolyData> datasetRef = R2polyData(dataset["vb"],dataset["it"]);
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    mahadist = model->ComputeMahalanobisDistanceForDataset(datasetRef);
    //return List::create(Named
    return wrap(mahadist);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }

}


SEXP ComputeCoefficientsForDataset(SEXP pPCA_, SEXP dataset_){
  try {
    List dataset(dataset_);
    double prob;
    const vtkSmartPointer<vtkPolyData> datasetRef = R2polyData(dataset["vb"],dataset["it"]);
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    Eigen::VectorXf out = model->ComputeCoefficientsForDataset(datasetRef);	
    //return List::create(Named
    return wrap(out);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }

}
SEXP RobustlyComputeCoefficientsForDataset(SEXP pPCA_, SEXP dataset_, SEXP niterations_, SEXP nu_, SEXP sigma2_) {
  try {
    List dataset(dataset_);
    double sigma2 = as<double>(sigma2_);
    unsigned int niterations = as<unsigned int>(niterations_);
    unsigned int nu = as<unsigned int>(nu_);
    
    const vtkSmartPointer<vtkPolyData> datasetRef = R2polyData(dataset["vb"],dataset["it"]);
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    Eigen::VectorXf out = model->RobustlyComputeCoefficientsForDataset(datasetRef,niterations,nu,sigma2);
    //return List::create(Named
    return wrap(out);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }

}

typedef std::vector<vtkPoint> DomainPointsListType;
SEXP GetDomainPoints(SEXP pPCA_) {
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
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
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

  
SEXP GetCovarianceAtPointId(SEXP pPCA_, SEXP pt1_, SEXP pt2_) {
  try {
    unsigned int ptId1 = as<unsigned int>(pt1_);
    unsigned int ptId2 = as<unsigned int>(pt2_);
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    MatrixXf cov = model->GetCovarianceAtPoint(ptId1,ptId2);
  
    return wrap(cov);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }

}
SEXP GetCovarianceAtPointPt(SEXP pPCA_, SEXP pt1_, SEXP pt2_) {
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    vtkPoint pt1 = SEXP2vtkPoint(pt1_);
    vtkPoint pt2 = SEXP2vtkPoint(pt2_);
    MatrixXf cov =model->GetCovarianceAtPoint(pt1,pt2);
  
    return wrap(cov);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }

}
SEXP GetCovarianceMatrix(SEXP pPCA_) {
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    MatrixXf cov = model->GetCovarianceMatrix();
  
    return wrap(cov);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

SEXP GetJacobian(SEXP pPCA_, SEXP pt_) {
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    vtkPoint pt = SEXP2vtkPoint(pt_);
    MatrixXf cov = model->GetJacobian(pt);
  
    return wrap(cov);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

typedef std::pair<vtkPoint, vtkPoint> PointValuePairType;
typedef std::list<PointValuePairType> PointValueListType;

SEXP ComputeCoefficientsForPointValues(SEXP pPCA_, SEXP sample_, SEXP mean_, SEXP noise_) {
  try {
    double noise = as<double>(noise_);
    NumericMatrix sample(sample_);
    NumericMatrix mean(mean_);
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
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
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

typedef MatrixType 	PointCovarianceMatrixType;
typedef std::pair < PointValuePairType, PointCovarianceMatrixType> 	PointValueWithCovariancePairType;
typedef std::list < PointValueWithCovariancePairType > 	PointValueWithCovarianceListType;

SEXP ComputeCoefficientsForPointValuesWithCovariance(SEXP pPCA_, SEXP sample_, SEXP mean_, SEXP noise_) {
  try {
    Map<MatrixXd> ptValueNoise(as<Map<MatrixXd> >(noise_));
    NumericMatrix sample(sample_);
    NumericMatrix mean(mean_);
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    PointValueListType ptValueList;
    PointValueWithCovarianceListType ptValueWithCovPair;
    MatrixXd tmpcovd;
    MatrixType tmpcov;	 
    for (int i = 0; i < mean.ncol();i++) {
      
      vtkPoint tmp0 = SEXP2vtkPoint(wrap(sample(_,i)));
      vtkPoint tmp1 = SEXP2vtkPoint(wrap(mean(_,i)));
      
      if (ptValueNoise.cols() == 1) {
	float scalarnoise =  ptValueNoise(i,0);
	if (scalarnoise == 0)
	  scalarnoise = 1e-6;
	tmpcov = Eigen::MatrixXf::Identity(3, 3) * scalarnoise;
      } else if (ptValueNoise.cols() == 3) {
	tmpcov = ptValueNoise.block<3,3>(i*3,0).cast<float>();
	if (tmpcov.isZero())
	  tmpcov = Eigen::MatrixXf::Identity(3, 3) * 1e-6;
	
      } else {
	::Rf_error("noise must be vector or 3 column matrix\n");
      }
      PointValuePairType tmppair = PointValuePairType(tmp1,tmp0);
      PointValueWithCovariancePairType covpair = PointValueWithCovariancePairType(tmppair,tmpcov);
      ptValueWithCovPair.push_back(covpair);
    }
  
    VectorXf coeff = model->ComputeCoefficientsForPointValuesWithCovariance(ptValueWithCovPair);
    
  
    return wrap(coeff);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

SEXP EvaluateSampleAtPoint(SEXP pPCA_, SEXP dataset_, SEXP meanpt_) {
  try {
    List dataset(dataset_);
    const vtkSmartPointer<vtkPolyData> sample = R2polyData(dataset["vb"],dataset["it"]);
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    vtkPoint pt = SEXP2vtkPoint(meanpt_);
    vtkPoint samplept = model->EvaluateSampleAtPoint(sample,pt);
    return NumericVector(&samplept[0],&samplept[3]);
    
  
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

SEXP GetPCABasisMatrix(SEXP pPCA_) {
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    MatrixXf pcaBasis = model->GetPCABasisMatrix();
    return wrap(pcaBasis);
    
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

SEXP GetOrthonormalPCABasisMatrix(SEXP pPCA_) {
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    MatrixXf pcaBasis = model->GetOrthonormalPCABasisMatrix();
    return wrap(pcaBasis);
    
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}


SEXP GetNumberOfPrincipalComponents(SEXP pPCA_) {
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    unsigned int nofPCS = model->GetNumberOfPrincipalComponents();
    return wrap(nofPCS);
    
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

SEXP GetMeanVector(SEXP pPCA_) {
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    VectorXf meanVector  = model->GetMeanVector();
    return wrap(meanVector);
    
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}
SEXP GetNoiseVariance(SEXP pPCA_) {
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    double noise  = model->GetNoiseVariance();
    return wrap(noise);
    
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

SEXP GetPCAVarianceVector(SEXP pPCA_) {
  try {
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    VectorXf pcaVector  = model->GetPCAVarianceVector();
    return wrap(pcaVector);
    
  } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}
