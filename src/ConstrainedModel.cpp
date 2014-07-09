#include "ConstrainedModel.h"


typedef std::pair<vtkPoint, vtkPoint> PointValuePairType;
typedef std::list<PointValuePairType> PointValueListType;
typedef PosteriorModelBuilder<vtkPolyData> ModelBuilderType;

double mahadist(const StatisticalModelType* model, vtkPoint targetPt, vtkPoint meanPt) {
    MatrixType cov = model->GetCovarianceAtPoint(meanPt, meanPt);
    int pointDim =3;
    VectorType x = VectorType::Zero(pointDim);
    for (unsigned d = 0; d < pointDim; d++) {
      x(d) = targetPt[d] - meanPt[d];
    }
    return x.transpose() * cov.inverse() * x;
}

// calculate a posterior model given two sets of points - one on the model's mean and a sample used to restrict the model.
SEXP PosteriorModel(SEXP pPCA_,SEXP sample_, SEXP mean_, SEXP ptValueNoise_) {

   try {
     double ptValueNoise = as<double>(ptValueNoise_);
     NumericMatrix sample(sample_);
     NumericMatrix mean(mean_);
     auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
     PointValueListType ptValueList;
     auto_ptr<ModelBuilderType> modelBuilder(ModelBuilderType::Create());
      for (int i = 0; i < mean.ncol();i++) {
      
      vtkPoint tmp0 = SEXP2vtkPoint(wrap(sample(_,i)));
      vtkPoint tmp1 = SEXP2vtkPoint(wrap(mean(_,i)));
      
      ptValueList.push_back(PointValuePairType(tmp1,tmp0));
      }
      auto_ptr<StatisticalModelType> postModel(modelBuilder->BuildNewModelFromModel(model.get(), ptValueList,ptValueNoise));
      
      return statismo2pPCA(postModel);
   }  catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}


// evaluate the probabilty of each point before using it as a prior
SEXP PosteriorModelSafe(SEXP pPCA_,SEXP sample_, SEXP mean_, SEXP ptValueNoise_,SEXP maha_) {

   try {
     double maha = as<double>(maha_);
     double ptValueNoise = as<double>(ptValueNoise_);
     NumericMatrix sample(sample_);
     NumericMatrix mean(mean_);
     auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
     PointValueListType ptValueList;
     auto_ptr<ModelBuilderType> modelBuilder(ModelBuilderType::Create());
      for (int i = 0; i < mean.ncol();i++) {
      
	vtkPoint tmp0 = SEXP2vtkPoint(wrap(sample(_,i)));
	vtkPoint tmp1 = SEXP2vtkPoint(wrap(mean(_,i)));
	double mahaget = mahadist(model.get(),tmp0,tmp1);
	if (mahaget <= maha)
	  ptValueList.push_back(PointValuePairType(tmp1,tmp0));
      }
      auto_ptr<StatisticalModelType> postModel(modelBuilder->BuildNewModelFromModel(model.get(), ptValueList,ptValueNoise));
      
      return statismo2pPCA(postModel);
   }  catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}
