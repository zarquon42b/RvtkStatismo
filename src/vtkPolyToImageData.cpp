#include <vtkVersion.h>
#include <vtkSmartPointer.h>
#include <vtkPolyData.h>
#include <vtkImageData.h>
#include <vtkPointData.h>
#include <vtkImageWriter.h>
#include <vtkMetaImageWriter.h>
#include <vtksys/SystemTools.hxx>
#if VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1
#include <vtkNIFTIImageReader.h>
#include <vtkNIFTIImageWriter.h>
#endif
#include "R2polyData.h"
#include "vtkPolyData2vtkImageData.h"

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
    
    
#if VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1
    vtkSmartPointer<vtkImageWriter> writer;
    if (ext.compare(".nii") || ext.compare(".gz")) { 
    writer = vtkSmartPointer<vtkNIFTIImageWriter>::New();
  } else {
    writer = vtkSmartPointer<vtkMetaImageWriter>::New();
  }
#else
    vtkSmartPointer<vtkImageWriter> writer = vtkSmartPointer<vtkMetaImageWriter>::New();
#endif
    writer->SetFileName(outputFilename.c_str());
#if VTK_MAJOR_VERSION <= 5
    writer->SetInputConnection(whiteImage->GetProducerPort());
#else
    writer->SetInputData(whiteImage);
#endif

    writer->Write();
    return wrap(0);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}
