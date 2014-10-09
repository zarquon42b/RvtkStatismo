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
#include <vtkTransform.h>
#include <vtkTransformPolyDataFilter.h>
#include "RcppEigen.h"
#include "R2polyData.h"
#include "polyData2R.h"
#include "vtkpoly2unstruct.h"
#include "R2vtkPoints.h"
#include "vtkImageIO.h"

static vtkSmartPointer<vtkPolyData> transform_back(vtkSmartPointer<vtkPoints> pt, vtkSmartPointer<vtkPolyData> pd);

RcppExport SEXP vtkSurfaceReko(SEXP mesh_, SEXP sampSpace_=wrap(0)) {
try {
  double sampSpace = as<double>(sampSpace_);
  List mesh(mesh_);
vtkSmartPointer<vtkPoints> points = R2vtkPoints(mesh["vb"]);
  vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata->SetPoints(points);
 vtkSmartPointer<vtkSurfaceReconstructionFilter> surf = vtkSmartPointer<vtkSurfaceReconstructionFilter>::New();
#if VTK_MAJOR_VERSION <= 5
 surf->SetInput(polydata);
#else
 surf->SetInputData(polydata);
#endif
 if (sampSpace > 0)
   surf->SetSampleSpacing(sampSpace);
 surf->Update();
 vtkSmartPointer<vtkMarchingCubes> contourFilter = vtkSmartPointer<vtkMarchingCubes>::New();
  contourFilter->SetInputConnection(surf->GetOutputPort());
  //contourFilter->UseScalarTreeOn();
  
  contourFilter->SetValue(0,0.0);
  //vtkSmartPointer<vtkImageData> image = surf->GetOutput();
  //vtkImageWrite(image,"test.mha");
 
  // Sometimes the contouring algorithm can create a volume whose gradient
  // vector and ordering of polygon (using the right hand rule) are
  // inconsistent. vtkReverseSense cures this problem.
  vtkSmartPointer<vtkReverseSense> reverse = vtkSmartPointer<vtkReverseSense>::New();
  reverse->SetInputConnection(contourFilter->GetOutputPort());
  reverse->ReverseCellsOn();
  reverse->ReverseNormalsOn();
  reverse->Update();
 
  vtkSmartPointer<vtkPolyData> newSurf = transform_back( points, reverse->GetOutput());
 
  
  return polyData2R(reverse->GetOutput());
 } catch (std::exception& e) {
  ::Rf_error( e.what());
 } catch (...) {
  ::Rf_error("unknown exception");
 }
}

vtkSmartPointer<vtkPolyData> transform_back(vtkSmartPointer<vtkPoints> pt, vtkSmartPointer<vtkPolyData> pd)
{
  // The reconstructed surface is transformed back to where the
  // original points are. (Hopefully) it is only a similarity
  // transformation.
 
  // 1. Get bounding box of pt, get its minimum corner (left, bottom, least-z), at c0, pt_bounds
 
  // 2. Get bounding box of surface pd, get its minimum corner (left, bottom, least-z), at c1, pd_bounds
 
  // 3. compute scale as: 
  //       scale = (pt_bounds[1] - pt_bounds[0])/(pd_bounds[1] - pd_bounds[0]);
 
  // 4. transform the surface by T := T(pt_bounds[0], [2], [4]).S(scale).T(-pd_bounds[0], -[2], -[4])
 
 
 
  // 1.
  double pt_bounds[6];  // (xmin,xmax, ymin,ymax, zmin,zmax)
  pt->GetBounds(pt_bounds);
 
 
  // 2.
  double pd_bounds[6];  // (xmin,xmax, ymin,ymax, zmin,zmax)
  pd->GetBounds(pd_bounds);
 
//   // test, make sure it is isotropic
//   std::cout<<(pt_bounds[1] - pt_bounds[0])/(pd_bounds[1] - pd_bounds[0])<<std::endl;
//   std::cout<<(pt_bounds[3] - pt_bounds[2])/(pd_bounds[3] - pd_bounds[2])<<std::endl;
//   std::cout<<(pt_bounds[5] - pt_bounds[4])/(pd_bounds[5] - pd_bounds[4])<<std::endl;
//   // TEST  
 
 
  // 3
  double scale = (pt_bounds[1] - pt_bounds[0])/(pd_bounds[1] - pd_bounds[0]);
 
 
  // 4.
  vtkSmartPointer<vtkTransform> transp = vtkSmartPointer<vtkTransform>::New();
  transp->Translate(pt_bounds[0], pt_bounds[2], pt_bounds[4]);
  transp->Scale(scale, scale, scale);
  transp->Translate(- pd_bounds[0], - pd_bounds[2], - pd_bounds[4]);
 
  vtkSmartPointer<vtkTransformPolyDataFilter> tpd = vtkSmartPointer<vtkTransformPolyDataFilter>::New();
#if VTK_MAJOR_VERSION <= 5
  tpd->SetInput(pd);
#else
  tpd->SetInputData(pd);
#endif
  tpd->SetTransform(transp);
  tpd->Update();
 
 
  return tpd->GetOutput();
}
