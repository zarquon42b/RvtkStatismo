#include "Representers/VTK/vtkStandardMeshRepresenter.h"
#include "statismo/PCAModelBuilder.h"
#include "statismo/StatisticalModel.h"
#include "statismo/DataManager.h"
#include <memory>
using namespace statismo;
using std::auto_ptr;

typedef vtkStandardMeshRepresenter RepresenterType;
typedef DataManager<vtkPolyData> DataManagerType;
typedef PCAModelBuilder<vtkPolyData> ModelBuilderType;
typedef StatisticalModel<vtkPolyData> StatisticalModelType;

