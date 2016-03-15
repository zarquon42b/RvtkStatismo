#ifndef _VTK_TYPES_H__
#define _VTK_TYPES_H__
#include "vtkStandardMeshRepresenter.h"
#include "PCAModelBuilder.h"
#include "StatisticalModel.h"
#include "DataManager.h"
//#include <boost/XPtr.hpp>


using namespace statismo;
//using boost::XPtr;

typedef vtkStandardMeshRepresenter vtkMeshRepresenter;
typedef DataManager<vtkPolyData> vtkMeshDataManager;
typedef StatisticalModel<vtkPolyData> vtkMeshModel;

#endif
