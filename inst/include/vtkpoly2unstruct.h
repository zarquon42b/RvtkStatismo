#include <vtkVersion.h>
#include <vtkSmartPointer.h>
#include <vtkPolyData.h>
#include <vtkUnstructuredGrid.h>
#include <vtkCellArray.h>
#include <vtkGeometryFilter.h>
#include <vtkPointSource.h>
#include <vtkDataSetSurfaceFilter.h>
#include <vtkAppendFilter.h>


#ifndef _VTK_POLY2UNSTRUCT_H__
#define _VTK_POLY2UNSTRUCT_H__
vtkSmartPointer<vtkUnstructuredGrid> poly2grid(vtkSmartPointer<vtkPolyData> polydata);

vtkSmartPointer<vtkPolyData> grid2poly(vtkSmartPointer<vtkUnstructuredGrid> unstructuredGrid);

#endif //_VTK_POLY2UNSTRUCT_H__
