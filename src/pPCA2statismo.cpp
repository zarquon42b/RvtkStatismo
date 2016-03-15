#include "pPCA2statismo.h"
#include "polyData2R.h"
#include "Helpers.h"
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
XPtr<vtkMeshModel> pPCA2statismo(SEXP pPCA_) {
  try {
    std::string classname = getClassname(pPCA_);
    if (classname == "pPCA") {
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
      XPtr<vtkMeshRepresenter> representer(vtkMeshRepresenter::Create(reference));
      VectorXf meanshape = PCA["center"];
      VectorXf PCVariance = PCA["sdev"];
      Map<MatrixXd> PCBasisOrtho0(as<Map<MatrixXd> >(PCA["rotation"]));
    
      typedef Eigen::MatrixXf fMat;
      typedef Eigen::Map<fMat> MfMat;
      MatrixXf PCBasisOrtho = PCBasisOrtho0.cast<float>();
      //VectorXf PCVariance = PCBasis.colwise().norm();
      PCVariance = PCVariance.array().pow(2);//get Variance from sdev
      double sigma = as<double>(pPCA.slot("sigma"));
      XPtr<vtkMeshModel> model(vtkMeshModel::Create(representer.get(),meanshape,PCBasisOrtho,PCVariance,sigma));
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
	    std::string kval1 = as<std::string>(kval[1]);
	    KeyValuePair kpair(kval0,kval1);
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
    } else {
      S4 pPCA_pointer(pPCA_);
      XPtr<vtkMeshModel> model = as<XPtr<vtkMeshModel> >(pPCA_pointer.slot("pointer"));
      return model;
    }
    
  }  catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
  
  
 
}

typedef  std::pair< std::string,std::string >  KeyValuePair;
typedef std::list< KeyValuePair > KeyValueList;
typedef KeyValueList::iterator keyiter;
typedef std::vector<BuilderInfo> BuilderInfoList;
S4 statismo2pPCA(XPtr<vtkMeshModel> model, bool pointer) {
  try {
    if (pointer) {
      Language pPCAcall("new", "pPCA_pointer");
      Rcpp::S4 pPCA( pPCAcall.eval() );
      pPCA.slot("pointer") = model;
      return pPCA;
    } else {
      vtkPolyData* reference = model->DrawMean();
      List PCA = List::create(
			      Named("sdev")= model->GetPCAVarianceVector().array().sqrt(),
			      Named("rotation") = model->GetOrthonormalPCABasisMatrix(),
			      Named("center")= model->GetMeanVector(),
			      Named("x")=model->GetModelInfo().GetScoresMatrix().transpose()
			     
			      );
      Language pPCAcall("new", "pPCA");
      Rcpp::S4 pPCA( pPCAcall.eval() );
      //S4 pPCA;
      pPCA.slot("PCA") = PCA;
      pPCA.slot("sigma") = model->GetNoiseVariance();
      pPCA.slot("representer")=polyData2R(reference);
      reference->Delete();
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
    } 
  }  catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}
