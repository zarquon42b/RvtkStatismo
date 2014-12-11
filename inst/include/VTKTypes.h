#ifndef _VTK_TYPES_H__
#define _VTK_TYPES_H__
#include "vtkStandardMeshRepresenter.h"
#include "PCAModelBuilder.h"
#include "StatisticalModel.h"
#include "DataManager.h"
#include "DataManagerWithSurrogateVector.h"
#include <boost/shared_ptr.hpp>


using namespace statismo;
using boost::shared_ptr;

typedef vtkStandardMeshRepresenter vtkMeshRepresenter;
typedef DataManager<vtkPolyData> vtkMeshDataManager;
typedef DataManagerWithSurrogateVector<vtkPolyData> vtkMeshDataManagerWithSurrogates;
typedef StatisticalModel<vtkPolyData> vtkMeshModel;

#endif
