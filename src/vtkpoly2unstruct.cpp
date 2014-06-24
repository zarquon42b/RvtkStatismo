#include "vtkpoly2unstruct.h"

vtkSmartPointer<vtkUnstructuredGrid> poly2grid(vtkSmartPointer<vtkPolyData> polydata) {
 
 vtkSmartPointer<vtkAppendFilter> appendFilter = vtkSmartPointer<vtkAppendFilter>::New();
#if VTK_MAJOR_VERSION <= 5
  appendFilter->AddInput(polydata);
#else
  appendFilter->AddInputData(polydata);
#endif
  appendFilter->Update();
 
  vtkSmartPointer<vtkUnstructuredGrid> unstructuredGrid =
     vtkSmartPointer<vtkUnstructuredGrid>::New();
  unstructuredGrid->ShallowCopy(appendFilter->GetOutput());
  return unstructuredGrid;
}
vtkSmartPointer<vtkPolyData> grid2poly(vtkSmartPointer<vtkUnstructuredGrid> unstructuredGrid) {
 
  vtkSmartPointer<vtkDataSetSurfaceFilter> surfaceFilter = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
#if VTK_MAJOR_VERSION <= 5
  surfaceFilter->SetInput(unstructuredGrid);
#else
  surfaceFilter->SetInputData(unstructuredGrid);
#endif
  surfaceFilter->Update(); 
 
  vtkSmartPointer<vtkPolyData> polydata = surfaceFilter->GetOutput();
  return polydata;
}
