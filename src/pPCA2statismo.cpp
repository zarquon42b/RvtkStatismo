#include "pPCA2statismo.h"
#include "polyData2R.h"
#include "ModelInfo.h"
using Rcpp::List;
using Rcpp::NumericMatrix;
using Rcpp::IntegerMatrix;
using Eigen::VectorXf;
using Eigen::VectorXd;
using Eigen::MatrixXf;
using Eigen::MatrixXd;
using Eigen::Map;

typedef  std::pair< std::string,std::string >  KeyValuePair;
typedef std::list< KeyValuePair > KeyValueList;
typedef KeyValueList::iterator keyiter;
typedef std::vector<BuilderInfo> BuilderInfoList;
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
    //Get Scores
    Map<MatrixXd> scores0(as<Map<MatrixXd> >(PCA["x"]));
    MatrixXf scores = scores0.transpose().cast<float>();
    
    //get modelinfo
    KeyValueList datInfo, paraInfo; //empty keyvalue lists
    if (pPCA.hasSlot("modelinfo")) {
      S4 modelinfo(pPCA.slot("modelinfo"));
      if (modelinfo.hasSlot("datainfo")) {
	List datainfo(modelinfo.slot("datainfo"));
	for (int i = 0; i < datainfo.size(); i++) {
	  CharacterVector kval(datainfo[i]);
	  std::string kval0 = as<std::string>(kval[0]);
	  std::string kval1 = as<std::string>(kval[1]);
	  KeyValuePair kpair(kval0,kval1);
	  datInfo.push_back(kpair);
	}
	List paraminfo(modelinfo.slot("paraminfo"));
	for (int i = 0; i < paraminfo.size(); i++) {
	  CharacterVector kval(paraminfo[i]);
	  std::string kval0 = as<std::string>(kval[0]);
	  KeyValuePair kpair(kval0,kval0);
	  paraInfo.push_back(kpair);
	}
      }
    }
    //set modelinfo
    std::string bname = "RvtkStatismo";
    //create BuilderInfo
    BuilderInfo builderInfo(bname,datInfo,paraInfo);
    // create BuilderInfoList and push keyvalues
    BuilderInfoList binfoList;
    binfoList.push_back(builderInfo);
    ModelInfo myinfo(scores,binfoList);//(scores,binfoList);
    model->SetModelInfo(myinfo);
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

typedef  std::pair< std::string,std::string >  KeyValuePair;
typedef std::list< KeyValuePair > KeyValueList;
typedef KeyValueList::iterator keyiter;
typedef std::vector<BuilderInfo> BuilderInfoList;
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

       // get model info
       //create S4 object modelinfo
       Language modInfocall("new", "modelinfo");
       S4 modelinfo(modInfocall.eval());
       
       BuilderInfoList binfo = model->GetModelInfo().GetBuilderInfoList();
       List datinfo;
       List paraminfo;
       if (binfo.size() > 0) {
	 KeyValueList DataInfo = binfo[0].GetDataInfo();
	 KeyValueList BuildInfo = binfo[0].GetParameterInfo();
	 
	 keyiter it;
	 for (it=DataInfo.begin(); it!= DataInfo.end();it++) {
	   CharacterVector kval(2);
	   kval[0] = (*it).first.c_str();
	   kval[1] = (*it).second.c_str();
	   datinfo.push_back(kval);
	 }
	
	 for (it=BuildInfo.begin(); it!= BuildInfo.end();it++) {
	   CharacterVector kval(2);
	   kval[0] = (*it).first.c_str();
	   kval[1] = (*it).second.c_str();
	   paraminfo.push_back(kval);
	 }
       }
       
       modelinfo.slot("datainfo") = datinfo;
       modelinfo.slot("paraminfo") = paraminfo;
    pPCA.slot("modelinfo") = modelinfo;
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
