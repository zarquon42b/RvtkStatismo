#include "pPCA2statismo.h"
#include "polyData2R.h"


using namespace Eigen;
auto_ptr<StatisticalModelType> pPCA2statismo(SEXP pPCA_) {
  try {
    S4 pPCA(pPCA_);
    //List pPCA(pPCA_);
    List reflist = pPCA.slot("representer");  
    List PCA = pPCA.slot("PCA");
    vtkSmartPointer<vtkPolyData> reference;
    if (! Rf_isNull(reflist["it"])) {
      reference = R2polyData(reflist["vb"],reflist["it"]);
    } else {
      reference = R2polyData(reflist["vb"]);
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
    double sigma = as<double>(pPCA.slot("sigma"));
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

S4 statismo2pPCA(auto_ptr<StatisticalModelType> model) {
   try {
     if (model.get()) {
    vtkSmartPointer<vtkPolyData> reference = model->DrawMean();
    List PCA = List::create(Named("rotation") = model->GetOrthonormalPCABasisMatrix(),
			    Named("center")= model->GetMeanVector(),
			    Named("x")=model->GetModelInfo().GetScoresMatrix().transpose(),
			    Named("sdev")= model->GetPCAVarianceVector().array().sqrt()
			    );
    Language pPCAcall("new", "pPCA");
    Rcpp::S4 pPCA( pPCAcall.eval() );
    //S4 pPCA;
    pPCA.slot("PCA") = PCA;
    pPCA.slot("sigma") = model->GetNoiseVariance();
    pPCA.slot("representer")=polyData2R(reference);
			
    return pPCA;
    } else {
      Rprintf("Invalid model\n");
      return wrap(1);
    }
}  catch (std::exception& e) {
    ::Rf_error( e.what());
     
  } catch (...) {
    ::Rf_error("unknown exception");
     
  }
  }
