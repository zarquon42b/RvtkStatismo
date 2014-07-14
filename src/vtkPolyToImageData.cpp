#include "R2polyData.h"
#include "vtkPolyData2vtkImageData.h"
#include "vtkImageIO.h"

#include <Rcpp.h>
using namespace Rcpp;

//transforms a 3 x k SEXP matrix into vtkPoints
RcppExport SEXP vtkPolyToImageData(SEXP mesh_, SEXP outname_, SEXP spacing_) {
  try {
    List mesh(mesh_);
    std::string outputFilename = as<std::string>(outname_);
    vtkSmartPointer<vtkPolyData> pd = R2polyData(mesh["vb"],mesh["it"]);
    NumericVector spacingtmp(spacing_);
    double spacing[3]; // desired volume spacing
    spacing[0] = spacingtmp[0];
    spacing[1] = spacingtmp[1];
    spacing[2] = spacingtmp[2];
    
    vtkSmartPointer<vtkImageData> whiteImage = vtkPolyData2vtkImageData(pd,spacing);
    int chk = vtkImageWrite(whiteImage,outputFilename);

    return wrap(0);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}
