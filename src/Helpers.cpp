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
