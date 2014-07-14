#ifndef _R2VTKPOINTS_H__
#define _R2VTKPOINTS_H__
#include <vtkVersion.h>
#include <vtkSmartPointer.h>
#include <vtkPolyData.h>

#if VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1
 #include <vtkNIFTIImageReader.h>
 #include <vtkNIFTIImageWriter.h>
#endif


#include <Rcpp.h>
using namespace Rcpp;

using namespace Rcpp;
//transforms a 3 x k SEXP matrix into vtkPoints
vtkSmartPointer<vtkPoints> R2vtkPoints(SEXP vb_);
 

#endif //_R2VTKPOINTS_H__
