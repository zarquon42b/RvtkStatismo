#include "R2polyData.h"
#include "checkListNames.h"
using Rcpp::List;
using Rcpp::NumericMatrix;
using Rcpp::IntegerMatrix;

vtkSmartPointer<vtkPolyData> R2polyData(SEXP vb_, SEXP it_) {
  try {
    NumericMatrix vb(vb_);
    vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
    vtkSmartPointer<vtkTriangle> triangle = vtkSmartPointer<vtkTriangle>::New();
    vtkSmartPointer<vtkCellArray> vertices = vtkSmartPointer<vtkCellArray>::New();
    vtkSmartPointer<vtkCellArray> triangles = vtkSmartPointer<vtkCellArray>::New();
    vtkSmartPointer<vtkIntArray> indexv = vtkSmartPointer<vtkIntArray>::New();
    indexv->SetNumberOfComponents(1);
    indexv->SetName("indexv");
    for (int i = 0; i < vb.ncol();i++) {
      float p[3];
      for (int j = 0;j < 3; j++) 
	p[j] = vb(j,i);  
      vtkIdType pid[1];
      pid[0] = points->InsertNextPoint(p);
      indexv->InsertNextValue(i);
      vertices->InsertNextCell(1,pid);
    }
    bool hasFaces = true;
    //deal with faces
    vtkSmartPointer<vtkIntArray> index = vtkSmartPointer<vtkIntArray>::New();
    index->SetNumberOfComponents(1);
    index->SetName("index");
    if (Rf_isMatrix(it_)) {//check if input is matrix
      IntegerMatrix it(it_);
      if (it.ncol() == 0)
	hasFaces = false;
      // get faces
      if (hasFaces) {
	for (int i = 0; i < it.ncol();i++) {
	  triangle->GetPointIds()->SetId (0, it(0,i)-1);
	  triangle->GetPointIds()->SetId (1, it(1,i)-1);
	  triangle->GetPointIds()->SetId (2, it(2,i)-1);
	  triangles->InsertNextCell (triangle);
	  index->InsertNextValue(i);
	}
      }
    } else {
      hasFaces = false;
    }
  
    vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
    // Set the points and vertices we created as the geometry and topology of the polydata
    polydata->SetPoints(points);
    if (!hasFaces) {
      polydata->SetVerts(vertices);    
      //polydata->GetCellData()->AddArray(indexv);
    }
    if (hasFaces) {
      polydata->SetPolys(triangles);
      //polydata->GetCellData()->AddArray(index);
    }
    return polydata;
  
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    vtkSmartPointer<vtkPolyData> polydata;  
    return polydata;
  } catch (...) {
    ::Rf_error("unknown exception");
    vtkSmartPointer<vtkPolyData> polydata;      
    return polydata;
  }
}

vtkSmartPointer<vtkPolyData> mesh3d2polyData(SEXP mesh_) {
  try{
    List mesh(mesh_);
    Rcpp::CharacterVector mychar = Rcpp::CharacterVector::create("vb","it");
    std::vector<bool> test = checkListNames(mesh,mychar);
    for (int i = 0; i < 2; i++) {
      if (!test[i]) {
	std::string tmp = Rcpp::as<std::string>(mychar[i]);
	mesh[tmp] = Rcpp::wrap(0);
      }
    }
    return R2polyData(mesh["vb"],mesh["it"]);
  
  }  catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...) {
    ::Rf_error("unknown exception");
  }
}
