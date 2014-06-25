#include "Representers/VTK/vtkStandardMeshRepresenter.h"
#include "statismo/PCAModelBuilder.h"
#include "statismo/StatisticalModel.h"
#include "statismo/DataManager.h"
#include <memory>

#ifndef _VTK_TYPES_H__
#define _VTK_TYPES_H__
using namespace statismo;
using std::auto_ptr;

typedef vtkStandardMeshRepresenter RepresenterType;
typedef DataManager<vtkPolyData> DataManagerType;
typedef StatisticalModel<vtkPolyData> StatisticalModelType;

#endif
