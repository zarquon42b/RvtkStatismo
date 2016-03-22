#include "SetScale.h"

typedef  std::pair< std::string,std::string >  KeyValuePair;
typedef std::list< KeyValuePair > KeyValueList;
typedef KeyValueList::iterator keyiter;
typedef std::vector<BuilderInfo> BuilderInfoList;
RcppExport SEXP SetScale(SEXP pPCA_, SEXP value_) {
  try {
    bool value = as<bool>(value_);
    std::string scalevalue = "false";
    if (value)
      scalevalue = "true";
    bool findscale = false;
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    
    BuilderInfoList binfo = model->GetModelInfo().GetBuilderInfoList();
    KeyValueList BuildInfo;
    KeyValueList DataInfo;
    if (binfo.size() > 0) {
      DataInfo = binfo[0].GetDataInfo();
      BuildInfo = binfo[0].GetParameterInfo();
	
      for (keyiter it=BuildInfo.begin(); it!= BuildInfo.end();it++) {
	if ((*it).first == "scale") {
	  (*it).second = scalevalue;
	  findscale = true;
	}
      }
      if (! findscale) {
	std::string kval0 = "scale";
	std::string kval1 = scalevalue;
	KeyValuePair kpair(kval0,kval1);
	BuildInfo.push_back(kpair);
      }
      
    } else {
      std::string kval0 = "scale";
      std::string kval1 = scalevalue;
      KeyValuePair kpair(kval0,kval1);
      BuildInfo.push_back(kpair);
    }
    std::string bname = "RvtkStatismo";
    BuilderInfo builderInfo(bname,DataInfo,BuildInfo);
    BuilderInfoList binfoList;
    binfoList.push_back(builderInfo);
    ModelInfo myinfo(model->GetModelInfo().GetScoresMatrix(),binfoList);
    model->SetModelInfo(myinfo);
    return statismo2pPCA(model,true);
    
  }  catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}

bool GetScale(XPtr<vtkMeshModel> model) {
  try {
    bool scalevalue = false;
    BuilderInfoList binfo = model->GetModelInfo().GetBuilderInfoList();
    if (binfo.size() > 0) {
      KeyValueList BuildInfo = binfo[0].GetParameterInfo();
      for (keyiter it=BuildInfo.begin(); it!= BuildInfo.end();it++) {
	if ((*it).first == "scale") {
	  
	  if ((*it).second == "true")
	    scalevalue = true;
	}
      }
    }
    return scalevalue;
  }  catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}
