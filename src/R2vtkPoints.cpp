#include "R2vtkPoints.h"


#include <Rcpp.h>
using namespace Rcpp;
//transforms a 3 x k SEXP matrix into vtkPoints
vtkSmartPointer<vtkPoints> R2vtkPoints(SEXP vb_) {
  NumericMatrix vb(vb_);
  vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
  for (int i = 0; i < vb.ncol();i++) {
    float p[3];
    for (int j = 0;j < 3; j++) 
      p[j] = vb(j,i);  
    vtkIdType pid[1];
    pid[0] = points->InsertNextPoint(p);
  }
  return points;
}
