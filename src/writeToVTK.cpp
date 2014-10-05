#include <vtkSmartPointer.h>
#include <vtkSphereSource.h>
#include <vtkSmartPointer.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkSimplePointsReader.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkCellArray.h>
#include <vtkCellData.h>
#include <vtkRenderer.h>
#include <vtkProperty.h>
#include <RcppEigen.h>
#include <string.h>
#include <vtkPolyData.h>
#include <vtkTriangle.h>
#include <vtkXMLPolyDataWriter.h>
#include "vtkPolyDataWriter.h"
#include "R2polyData.h"
using namespace Rcpp;

RcppExport SEXP vtkWrite(SEXP filename_,SEXP vb_, SEXP it_, SEXP type_) {
  try {
    std::string type = as<std::string>(type_);
    CharacterVector filename(filename_);
    vtkSmartPointer<vtkPolyData> polydata = R2polyData(vb_, it_);
    
    if (type == "vtp") {
      vtkSmartPointer<vtkXMLPolyDataWriter> writer =  vtkSmartPointer<vtkXMLPolyDataWriter>::New();
  
      writer->SetFileName(filename[0]);
#if VTK_MAJOR_VERSION <= 5
      writer->SetInput(polydata);
#else
      writer->SetInputData(polydata);
#endif
      writer->Write(); 
      return  wrap(EXIT_SUCCESS); 
    } else if (type == "vtk") {
      vtkSmartPointer<vtkPolyDataWriter> writer =  vtkSmartPointer<vtkPolyDataWriter>::New();
  
      writer->SetFileName(filename[0]);
#if VTK_MAJOR_VERSION <= 5
      writer->SetInput(polydata);
#else
      writer->SetInputData(polydata);
#endif
 
      writer->Write();
      return  wrap(EXIT_SUCCESS);
    } else {
      ::Rf_error("something went wrong");
    }
  } catch  (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
  
}
 
