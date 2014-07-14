#ifndef _VTKPOLYTOIMAGEDATA_H__
#define _VTKPOLYTOIMAGEDATA_H__
#include <vtkVersion.h>
#include <vtkSmartPointer.h>
#include <vtkPolyData.h>
#include <vtkImageWriter.h>
#include <vtkMetaImageWriter.h>
#include <vtksys/SystemTools.hxx>

#if VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1
 #include <vtkNIFTIImageReader.h>
 #include <vtkNIFTIImageWriter.h>
#endif


#include <Rcpp.h>
using namespace Rcpp;

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

#endif
