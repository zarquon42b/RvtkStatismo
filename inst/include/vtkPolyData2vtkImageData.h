#ifndef _VTKPOLYDATA2VTKIMAGEDATA_H__
#define _VTKPOLYDATA2VTKIMAGEDATA_H__
#include <vtkVersion.h>
#include <vtkPolyDataToImageStencil.h>
#include <vtkImageStencil.h>
#include <vtkSmartPointer.h>
#include <vtkPolyData.h>
#include <vtkImageData.h>
#include <vtkPointData.h>

vtkSmartPointer<vtkImageData> vtkPolyData2vtkImageData(vtkSmartPointer<vtkPolyData> pd, double* spacing);

#endif // _VTKPOLYDATA2VTKIMAGEDATA_H__
