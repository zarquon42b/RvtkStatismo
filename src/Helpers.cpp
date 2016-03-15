#include "Helpers.h"
vtkPoint NumVec2vtkPoint(NumericVector vec) {
  vtkPoint pt1(vec[0],vec[1],vec[2]);
  return pt1;
}

vtkPoint SEXP2vtkPoint(SEXP vec) {
  NumericVector vec0(vec);
  vtkPoint pt1 = NumVec2vtkPoint(vec0);
  return pt1;

 
}

std::string getClassname(SEXP x) {
  Environment base("package:base");
  Function getAttrib = base["attr"];
  SEXP classname_ = getAttrib(pPCA_, "class");
  std::string classname = as<std::string>(classname_);
  return classname;
}
