#include "ConstrainedModel.h"

using namespace Eigen;

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
     Map<MatrixXd> ptValueNoise(as<Map<MatrixXd> >(ptValueNoise_));
     bool pointer = as<bool>(pointer_);
     NumericMatrix sample(sample_);
     NumericMatrix mean(mean_);
     XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
     PointValueListType ptValueList;
     PointValueWithCovarianceListType ptValueWithCovPair;
     XPtr<ModelBuilderType> modelBuilder(ModelBuilderType::Create());
     MatrixXd tmpcovd;
     MatrixType tmpcov;	 
     for (int i = 0; i < mean.ncol();i++) {
	 
       vtkPoint tmp0 = SEXP2vtkPoint(wrap(sample(_,i)));
       vtkPoint tmp1 = SEXP2vtkPoint(wrap(mean(_,i)));
       if (ptValueNoise.rows() == 1) {
	 ptValueList.push_back(PointValuePairType(tmp1,tmp0));
       } else if (ptValueNoise.cols() == 1) {
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
     }
     if (ptValueNoise.rows() == 1) {
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
      Map<MatrixXd> ptValueNoise(as<Map<MatrixXd> >(ptValueNoise_));
     NumericMatrix sample(sample_);
     NumericMatrix mean(mean_);
     XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
     PointValueListType ptValueList;
     PointValueWithCovarianceListType ptValueWithCovPair;
     XPtr<ModelBuilderType> modelBuilder(ModelBuilderType::Create());
     MatrixXd tmpcovd;
     MatrixType tmpcov;	 
      for (int i = 0; i < mean.ncol();i++) {
      
	vtkPoint tmp0 = SEXP2vtkPoint(wrap(sample(_,i)));
	vtkPoint tmp1 = SEXP2vtkPoint(wrap(mean(_,i)));
	double mahaget = mahadist(model.get(),tmp0,tmp1);
	if (mahaget <= maha) {
	  
	  if (ptValueNoise.rows() == 1) {
	 ptValueList.push_back(PointValuePairType(tmp1,tmp0));
       } else if (ptValueNoise.cols() == 1) {
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
	}
      }
      if (ptValueNoise.rows() == 1) {
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
