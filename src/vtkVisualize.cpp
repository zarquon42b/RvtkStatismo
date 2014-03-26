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
#include <Rcpp.h>
#include <string.h>
#include <vtkPolyData.h>
#include <vtkTriangle.h>
#include <vtkXMLPolyDataWriter.h>
using namespace Rcpp;

RcppExport SEXP vtkVisualize(SEXP vb_, SEXP it_, SEXP size_)
{
  NumericMatrix vb(vb_);
  IntegerMatrix it(it_);
  int size = as<int>(size_);
  vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
  //points->SetNumberOfPoints(vb.ncol());
  vtkSmartPointer<vtkTriangle> triangle = vtkSmartPointer<vtkTriangle>::New();

  vtkSmartPointer<vtkCellArray> vertices = vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkCellArray> triangles = vtkSmartPointer<vtkCellArray>::New();
  bool hasFaces = true;
  if (it.ncol() == 0)
    hasFaces = false;
  // vtkIdType pointIDs[3];
  //int pointId = 0;
  for (int i = 0; i < vb.ncol();i++) {
  
    float p[3];
    for (int j = 0;j < 3; j++) 
      p[j] = vb(j,i);  
    //points->SetPoint(pointId,tmp0);
    vtkIdType pid[1];
    pid[0] = points->InsertNextPoint(p);
    if (!hasFaces)
      vertices->InsertNextCell(1,pid);
    //pointId++;
  }
  vtkSmartPointer<vtkIntArray> index = vtkSmartPointer<vtkIntArray>::New();
  index->SetNumberOfComponents(1);
  index->SetName("index");
  // get faces
  if (hasFaces) {
    for (int i = 0; i < it.ncol();i++) {
      //int i =0;
      triangle->GetPointIds()->SetId (0, it(0,i));
      triangle->GetPointIds()->SetId (1, it(1,i));
      triangle->GetPointIds()->SetId (2, it(2,i));
      triangles->InsertNextCell (triangle);
      index->InsertNextValue(i);
    }
  }  
  vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
 
  // Set the points and vertices we created as the geometry and topology of the polydata
  polydata->SetPoints(points);
  polydata->GetCellData()->AddArray(index);
  if (!hasFaces)
    polydata->SetVerts(vertices);
  if (hasFaces)
    polydata->SetPolys(triangles);
  // Visualize
  vtkSmartPointer<vtkPolyDataMapper> mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
  mapper->SetInput(polydata);

  vtkSmartPointer<vtkActor> actor =
    vtkSmartPointer<vtkActor>::New();
  actor->SetMapper(mapper);
  actor->GetProperty()->SetPointSize(size);
 
  vtkSmartPointer<vtkRenderer> renderer =
    vtkSmartPointer<vtkRenderer>::New();
  vtkSmartPointer<vtkRenderWindow> renderWindow =
    vtkSmartPointer<vtkRenderWindow>::New();
  renderWindow->AddRenderer(renderer);
  vtkSmartPointer<vtkRenderWindowInteractor> renderWindowInteractor = 
    vtkSmartPointer<vtkRenderWindowInteractor>::New();
  renderWindowInteractor->SetRenderWindow(renderWindow);
 
  renderer->AddActor(actor);
 
  renderWindow->Render();
  renderWindowInteractor->Start();

  return  wrap(EXIT_SUCCESS);
}
 
