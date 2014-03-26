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

RcppExport SEXP vtkWrite(SEXP filename_,SEXP vb_, SEXP it_)
{
  CharacterVector filename(filename_);
  NumericMatrix vb(vb_);
  IntegerMatrix it(it_);
  //setup data structures
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
  vtkSmartPointer<vtkIntArray> indexv = vtkSmartPointer<vtkIntArray>::New();
  indexv->SetNumberOfComponents(1);
  indexv->SetName("indexv");
  for (int i = 0; i < vb.ncol();i++) {
  
    float p[3];
    for (int j = 0;j < 3; j++) 
      p[j] = vb(j,i);  
    //points->SetPoint(pointId,tmp0);
    vtkIdType pid[1];
    pid[0] = points->InsertNextPoint(p);
    
    if (!hasFaces) {
      indexv->InsertNextValue(i);
      vertices->InsertNextCell(1,pid);
    }
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
  if (!hasFaces) {
  polydata->SetVerts(vertices);    
  polydata->GetCellData()->AddArray(indexv);
  }
  if (hasFaces) {
    polydata->SetPolys(triangles);
    polydata->GetCellData()->AddArray(index);
  }
  vtkSmartPointer<vtkXMLPolyDataWriter> writer =  vtkSmartPointer<vtkXMLPolyDataWriter>::New();
  writer->SetFileName(filename[0]);
  writer->SetInput(polydata);
  writer->Write();  
  return  wrap(EXIT_SUCCESS);
}
 
