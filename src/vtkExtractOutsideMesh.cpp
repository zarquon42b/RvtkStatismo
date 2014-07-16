#include <vtkVersion.h>
#include <vtkSmartPointer.h>
#include <vtkPolyDataConnectivityFilter.h>
#include <vtkCellLocator.h>
#include <vtkTriangleFilter.h>
#include "Rcpp.h"
#include "R2polyData.h"
#include "polyData2R.h"

RcppExport SEXP vtkExtractOutsideMesh(SEXP mesh_, SEXP bbox_) {
try {
  List mesh(mesh_);
  vtkSmartPointer<vtkPolyData> polyData;
  NumericMatrix bbox(bbox_);
  polyData = R2polyData(mesh["vb"],mesh["it"]);
  double center[3], bounds[6];
  polyData->GetCenter(center);
  polyData->GetPoints()->GetBounds(bounds);
  vtkSmartPointer<vtkCellLocator> cellLocator = vtkSmartPointer<vtkCellLocator>::New();
  cellLocator->SetDataSet(polyData);
  cellLocator->BuildLocator();
  double rayStart[3];
 
 
  vtkIdType cellId = -1;
  double xyz[3], t, pcoords[3];
  int subId;
  int j = 0;
  while (cellId == -1 && j < 7) { 
    for (unsigned int i = 0; i < 3; i++) {
      rayStart[i] = bbox(j,i) * 1.1;
    }
    cellLocator->IntersectWithLine( rayStart,center, 0.0001,t, xyz,pcoords, subId,cellId);
    j++;
  }
  vtkSmartPointer<vtkPolyDataConnectivityFilter> connectivityFilter = 
    vtkSmartPointer<vtkPolyDataConnectivityFilter>::New();
#if VTK_MAJOR_VERSION <= 5
  connectivityFilter->SetInput(polyData);
#else
  connectivityFilter->SetInputData(polyData);
#endif
  connectivityFilter->SetExtractionModeToCellSeededRegions(); 
  connectivityFilter->InitializeSeedList();
  connectivityFilter->AddSeed(cellId);
  connectivityFilter->Update();
  vtkSmartPointer<vtkPolyData> newPoly = connectivityFilter->GetOutput();
  return polyData2R(newPoly);
 }catch (std::exception& e) {
  ::Rf_error( e.what());
  return wrap(1);
 } catch (...) {
  ::Rf_error("unknown exception");
  return wrap(1);
 }
}
