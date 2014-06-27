//NOTE: always!! include statismo stuff before this header!!!

#ifndef __R2polyData_H_
#define __R2polyData_H_

#include <vtkSmartPointer.h>
#include <vtkSphereSource.h>
#include <vtkSmartPointer.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
// get vertices and faces from R and create a vtkPolyData object
#include <vtkSimplePointsReader.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkCellArray.h>
#include <vtkCellData.h>
#include <vtkRenderer.h>
#include <vtkProperty.h>
#include <string.h>
#include <vtkPolyData.h>
#include <vtkTriangle.h>
#include <Rcpp.h>


vtkSmartPointer<vtkPolyData> R2polyData(SEXP vb_, SEXP it_= Rcpp::NumericMatrix(0,0));
#endif 
//NOTE: always!! include statismo stuff before this header!!!
