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
#include <vtkXMLPolyDataReader.h>
#include <vtkVRMLImporter.h>
#include <vtkUnstructuredGrid.h>
#include <vtkGenericDataObjectReader.h>
#include <vtkAppendPolyData.h>
#include <vtkTriangleFilter.h>
#include <vtkDataSetSurfaceFilter.h>
#include "polyData2R.h"
using namespace Rcpp;

RcppExport SEXP vtkRead(SEXP filename_, SEXP type_) {
  try {
    CharacterVector filename(filename_);
    int type = as<int>(type_);
    vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
    if (type == 0) {
      vtkSmartPointer<vtkGenericDataObjectReader> reader = vtkSmartPointer<vtkGenericDataObjectReader>::New();
      reader->SetFileName(filename[0]);
      reader->Update();
      if(reader->IsFilePolyData())   
	polydata = reader->GetPolyDataOutput();
      else 
	return wrap(1);
	 
    } else if (type == 1) {
      vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
      if (reader->CanReadFile(filename[0])) {
	reader->SetFileName(filename[0]);
	reader->Update();
	if(reader->GetOutput() != NULL)
	  polydata = reader->GetOutput();
	else 
	  return wrap(1);
      } else  
	return wrap(1);
    } else if (type == 2) {
      vtkVRMLImporter* reader = vtkVRMLImporter::New();
      reader->SetFileName(filename[0]);
      reader->Read();
      reader->Update();
      vtkSmartPointer<vtkDataSet> pDataset;
      vtkSmartPointer<vtkActorCollection> actors = reader->GetRenderer()->GetActors();
      actors->InitTraversal();
      //vtkIdType i = 0;
      for(vtkIdType i = 0; i < actors->GetNumberOfItems(); i++)
	{
	  vtkSmartPointer<vtkPolyData> polydatatmp = vtkSmartPointer<vtkPolyData>::New();
	  vtkSmartPointer<vtkActor> nextActor = actors->GetNextActor();
	  nextActor->GetMapper()->Update();
	  pDataset = nextActor->GetMapper()->GetInput();
	  polydatatmp = vtkPolyData::SafeDownCast(pDataset);
	  
	  vtkSmartPointer<vtkAppendPolyData> appendFilter = vtkSmartPointer<vtkAppendPolyData>::New();
#if VTK_MAJOR_VERSION <= 5
	  //appendFilter->AddInputConnection(polydata->GetProducerPort());
	  appendFilter->AddInputConnection(polydatatmp->GetProducerPort());
#else
	  //appendFilter->AddInputData(polydata);
	  appendFilter->AddInputData(polydatatmp);
#endif
	  appendFilter->Update();
	  
	  polydata = appendFilter->GetOutput();
      
	}
      //this is a very dirty hack to deal with a bug in vtk 6.1 (better a memory leak than a segfault)
#if VTK_MAJOR_VERSION == 6 && VTK_MINOR_VERSION != 1
      reader->Delete();
#endif	
    }
    //convert polydata to triangle mesh
    vtkSmartPointer<vtkTriangleFilter> triangleFilter = vtkSmartPointer<vtkTriangleFilter>::New();
#if VTK_MAJOR_VERSION <= 5 
    triangleFilter->SetInput(polydata);
#else
    triangleFilter->SetInputData(polydata);
#endif
    triangleFilter->Update();
    polydata = triangleFilter->GetOutput();
    
    
    List out = polyData2R(polydata);
    return out;
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
  }
} 

 
