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
#include <vtkDijkstraGraphGeodesicPath.h>

RcppExport SEXP vtkGeodesicPath(SEXP mesh_, SEXP start_, SEXP end_){
try {
  
  List mesh(mesh_);
  int start = as<int>(start_);
  int end = as<int>(end_);
  vtkSmartPointer<vtkPolyData> vtkmesh = R2polyData(mesh["vb"],mesh["it"]);
  vtkSmartPointer<vtkDijkstraGraphGeodesicPath> dijkstra = vtkSmartPointer<vtkDijkstraGraphGeodesicPath>::New();
  dijkstra->SetInputData(vtkmesh);
  dijkstra->SetStartVertex(start);
  dijkstra->SetEndVertex(end);
  //dijkstra->StopWhenEndReachedOn();
  //dijkstra->UseScalarWeightsOn();
  dijkstra->Update();
  vtkSmartPointer<vtkIdList> idlist = dijkstra->GetIdList();
  int idsize = idlist->GetNumberOfIds();
  double mydistance = 0;
  IntegerVector myids(idlist->GetNumberOfIds());
  for (int i=0; i < idsize;i++) {
    myids[i] = idlist->GetId(i);
    if (i < (idsize-1)) {
       double dist=0;
       double p0[3], p1[3], diff[3];
       vtkmesh->GetPoint(idlist->GetId(i),p0);
       vtkmesh->GetPoint(idlist->GetId(i+1),p1);
       for (unsigned int j=0; j < 3; j++) {
	 double tmp = p0[j]-p1[j];
	 tmp *= tmp;
	 dist += tmp;
	 
       }
       mydistance += sqrt(dist);
     }
  }
  List out;
  
  
  return List::create(Named("index") = myids,
		      Named("distance") = mydistance
		      );
wrap(mydistance);
 } catch (std::exception& e) {
  ::Rf_error( e.what());
 } catch (...) {
  ::Rf_error("unknown exception");
 }
}

