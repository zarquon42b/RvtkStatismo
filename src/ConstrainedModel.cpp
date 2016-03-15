#include "ConstrainedModel.h"


typedef std::pair<vtkPoint, vtkPoint> PointValuePairType;
typedef std::list<PointValuePairType> PointValueListType;
typedef MatrixType 	PointCovarianceMatrixType;
typedef std::pair < PointValuePairType, PointCovarianceMatrixType> 	PointValueWithCovariancePairType;
typedef std::list < PointValueWithCovariancePairType > 	PointValueWithCovarianceListType;

typedef PosteriorModelBuilder<vtkPolyData> ModelBuilderType;

double mahadist(const vtkMeshModel* model, vtkPoint targetPt, vtkPoint meanPt) {
    MatrixType cov = model->GetCovarianceAtPoint(meanPt, meanPt);
    int pointDim =3;
    VectorType x = VectorType::Zero(pointDim);
    for (unsigned d = 0; d < pointDim; d++) {
      x(d) = targetPt[d] - meanPt[d];
    }
    return x.transpose() * cov.inverse() * x;
}

// calculate a posterior model given two sets of points - one on the model's mean and a sample used to restrict the model.
SEXP PosteriorModel(SEXP pPCA_,SEXP sample_, SEXP mean_, SEXP ptValueNoise_, SEXP pointer_) {

   try {
     NumericMatrix ptValueNoise(ptValueNoise_);
     bool pointer = as<bool>(pointer_);
     NumericMatrix sample(sample_);
     NumericMatrix mean(mean_);
     XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
     PointValueListType ptValueList;
     PointValueWithCovarianceListType ptValueWithCovPair;
     XPtr<ModelBuilderType> modelBuilder(ModelBuilderType::Create());
     
     for (int i = 0; i < mean.ncol();i++) {
	 
       vtkPoint tmp0 = SEXP2vtkPoint(wrap(sample(_,i)));
       vtkPoint tmp1 = SEXP2vtkPoint(wrap(mean(_,i)));
       if (ptValueNoise.nrow() == 1) {
	 ptValueList.push_back(PointValuePairType(tmp1,tmp0));
       } else {
	 MatrixType tmpcov = Eigen::MatrixXf::Identity(3, 3) * ptValueNoise(i,0);
	 PointValuePairType tmppair = PointValuePairType(tmp1,tmp0);
	 PointValueWithCovariancePairType covpair = PointValueWithCovariancePairType(tmppair,tmpcov);
	 ptValueWithCovPair.push_back(covpair);
       }
     }
     if (ptValueNoise.nrow() == 1) {
       double noise = ptValueNoise(0,0);
       XPtr<vtkMeshModel> postModel(modelBuilder->BuildNewModelFromModel(model.get(), ptValueList,noise));
       return statismo2pPCA(postModel);
     } else {
       XPtr<vtkMeshModel> postModel(modelBuilder->BuildNewModelFromModel(model.get(),ptValueWithCovPair));
       return statismo2pPCA(postModel);
     }
   }  catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}


// evaluate the probabilty of each point before using it as a prior
SEXP PosteriorModelSafe(SEXP pPCA_,SEXP sample_, SEXP mean_, SEXP ptValueNoise_,SEXP maha_, SEXP pointer_) {

   try {
     
     double maha = as<double>(maha_);
     bool pointer = as<bool>(pointer_);
     NumericMatrix ptValueNoise(ptValueNoise_);
     NumericMatrix sample(sample_);
     NumericMatrix mean(mean_);
     XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
     PointValueListType ptValueList;
     PointValueWithCovarianceListType ptValueWithCovPair;
     XPtr<ModelBuilderType> modelBuilder(ModelBuilderType::Create());
      for (int i = 0; i < mean.ncol();i++) {
      
	vtkPoint tmp0 = SEXP2vtkPoint(wrap(sample(_,i)));
	vtkPoint tmp1 = SEXP2vtkPoint(wrap(mean(_,i)));
	double mahaget = mahadist(model.get(),tmp0,tmp1);
	if (mahaget <= maha) {
	  
	  if (ptValueNoise.nrow() == 1) {
	    ptValueList.push_back(PointValuePairType(tmp1,tmp0));
	  } else {
	    MatrixType tmpcov = Eigen::MatrixXf::Identity(3, 3) * ptValueNoise(i,0);
	    PointValuePairType tmppair = PointValuePairType(tmp1,tmp0);
	    PointValueWithCovariancePairType covpair = PointValueWithCovariancePairType(tmppair,tmpcov);
	    ptValueWithCovPair.push_back(covpair);
	  }	
	}
      }
      if (ptValueNoise.nrow() == 1) {
	double noise = ptValueNoise(0,0);
	XPtr<vtkMeshModel> postModel(modelBuilder->BuildNewModelFromModel(model.get(), ptValueList,noise));
	return statismo2pPCA(postModel);
      } else {
	XPtr<vtkMeshModel> postModel(modelBuilder->BuildNewModelFromModel(model.get(),ptValueWithCovPair));
	return statismo2pPCA(postModel);
      }
      
   }  catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}
