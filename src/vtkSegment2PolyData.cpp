#include <vtkVersion.h>
#include <vtkSurfaceReconstructionFilter.h>
#include <vtkProgrammableSource.h>
#include <vtkContourFilter.h>
#include <vtkReverseSense.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkPolyData.h>
#include <vtkCamera.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkMath.h>
#include <vtkSmartPointer.h>
#include <cmath>
 #include <vtkMarchingCubes.h>
 #include <vtkImageMarchingCubes.h>

#include <vtkTransform.h>
#include <vtkTransformPolyDataFilter.h>
#include "RcppEigen.h"
#include "R2polyData.h"
#include "polyData2R.h"
#include "vtkpoly2unstruct.h"
#include "R2vtkPoints.h"
#include "vtkImageIO.h"


RcppExport SEXP vtkSegment2PolyData(SEXP images_,SEXP values_) {
try {
std::string inputFilename = as<std::string>(images_);
vtkSmartPointer<vtkImageData> image = vtkImageRead(inputFilename);
vtkSmartPointer<vtkMarchingCubes> surface = vtkSmartPointer<vtkMarchingCubes>::New();
 double isoval = as<double>(values_);
#if VTK_MAJOR_VERSION <= 5
 surface->SetInput(image);
#else
  surface->SetInputData(image);
#endif
  surface->ComputeNormalsOn();
  surface->SetValue(0, isoval);      
  //vtkSmartPointer<vtkImageData> image = surf->GetOutput();
  //vtkImageWrite(image,"test.mha");
 
  // Sometimes the contouring algorithm can create a volume whose gradient
  // vector and ordering of polygon (using the right hand rule) are
  // inconsistent. vtkReverseSense cures this problem.
  vtkSmartPointer<vtkReverseSense> reverse = vtkSmartPointer<vtkReverseSense>::New();
  reverse->SetInputConnection(surface->GetOutputPort());
  reverse->ReverseCellsOn();
  reverse->ReverseNormalsOn();
  reverse->Update();
 
  //vtkSmartPointer<vtkPolyData> newSurf = transform_back( points, reverse->GetOutput());
  return polyData2R(surface->GetOutput()); } catch (std::exception& e) {
  ::Rf_error( e.what());
 } catch (...) {
  ::Rf_error("unknown exception");
 }
}

