

#include <vtkVersion.h>
#include <vtkPolyDataToImageStencil.h>
#include <vtkImageStencil.h>
#include <vtkSmartPointer.h>
#include <vtkPolyData.h>
#include <vtkImageData.h>

vtkSmartPointer<vtkImageData> vtkPolyData2vtkImageData(vtkSmartPointer<vtkPolyData> pd, double& spacing);
