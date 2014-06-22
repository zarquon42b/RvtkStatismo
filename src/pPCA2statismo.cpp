#include "pPCA2statismo.h"
#include "polyData2R.h"


using namespace Eigen;
auto_ptr<StatisticalModelType> pPCA2statismo(SEXP pPCA_) {
  try {
    List pPCA(pPCA_);
    List reflist = pPCA["representer"];  
    List PCA = pPCA["PCA"];
    vtkSmartPointer<vtkPolyData> reference;
    if (! Rf_isNull(reflist["it"])) {
      reference = R2vtk(reflist["vb"],reflist["it"]);
    } else {
      reference = R2vtk(reflist["vb"]);
    }
    auto_ptr<RepresenterType> representer(RepresenterType::Create(reference));
    VectorXf meanshape = PCA["center"];
    VectorXf PCVariance = PCA["sdev"];
    Map<MatrixXd> PCBasisOrtho0(as<Map<MatrixXd> >(PCA["rotation"]));
    
    typedef Eigen::MatrixXf fMat;
    typedef Eigen::Map<fMat> MfMat;
    MatrixXf PCBasisOrtho = PCBasisOrtho0.cast<float>();
    //VectorXf PCVariance = PCBasis.colwise().norm();
    PCVariance = PCVariance.array().pow(2);//get Variance from sdev
    double sigma = as<double>(pPCA["sigma"]);
    auto_ptr<StatisticalModelType> model(StatisticalModelType::Create(representer.get(),meanshape,PCBasisOrtho,PCVariance,sigma));
    if (! Rf_isNull(PCA["x"])) {
      Map<MatrixXd> scores0(as<Map<MatrixXd> >(PCA["x"]));
      MatrixXf scores = scores0.transpose().cast<float>();
      ModelInfo myinfo(scores);
      model->SetModelInfo(myinfo);
    }
    return model;
    
  }  catch (std::exception& e) {
    ::Rf_error( e.what());
    auto_ptr<StatisticalModelType> model(NULL);
    return model;
  } catch (...) {
    ::Rf_error("unknown exception");
    auto_ptr<StatisticalModelType> model(NULL);
    return model;
  }
  
  
 
  }


  Rcpp::List statismo2pPCA(auto_ptr<StatisticalModelType> model) {
    if (model.get()) {
      vtkSmartPointer<vtkPolyData> reference = model->DrawMean();
  
      return List::create(Named("PCBasis") = model->GetPCABasisMatrix(),
			  Named("PCBasisOrtho") = model->GetOrthonormalPCABasisMatrix(),
			  Named("PCVariance")= model->GetPCAVarianceVector(),
			  Named("sigma")= model->GetNoiseVariance(),
			  Named("mshape")= model->GetMeanVector(),
			  Named("dim")=model->GetRepresenter()->GetDimensions(),
			  Named("scores")=model->GetModelInfo().GetScoresMatrix(),
			  Named("representer")=polyData2R(reference)
			  );
    } else {
      Rprintf("Invalid model\n");
      return wrap(1);
    }
  }
