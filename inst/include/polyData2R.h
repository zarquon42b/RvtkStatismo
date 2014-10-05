//NOTE: always!! include statismo stuff before this header!!!

#ifndef _polyData2R_H_
#define _polyData2R_H_

#include <vtkSmartPointer.h>
#include <vtkSphereSource.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkSimplePointsReader.h>
#include <vtkCellArray.h>
#include <vtkCellData.h>
#include <vtkProperty.h>
#include <string.h>
#include <vtkPolyData.h>
#include <vtkTriangle.h>
#include <vtkUnstructuredGrid.h>
#include <vtkGenericDataObjectReader.h>
#include <RcppEigen.h>



using namespace Rcpp;
List polyData2R(vtkSmartPointer<vtkPolyData> polydata);
#endif
//NOTE: always!! include statismo stuff before this header!!!
