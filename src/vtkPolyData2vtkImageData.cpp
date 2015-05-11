#include "vtkPolyData2vtkImageData.h"
#include "RcppEigen.h"
vtkSmartPointer<vtkImageData> vtkPolyData2vtkImageData(vtkSmartPointer<vtkPolyData> pd, double* spacing, double margin, int col) {
  vtkSmartPointer<vtkImageData> whiteImage = vtkSmartPointer<vtkImageData>::New();
  
  double bounds[6];
  pd->GetBounds(bounds);
  double dimbounds[6];
  for (int i = 0; i < 3;i++) {
    double tmp = abs(bounds[i*2]-bounds[i*2+1]);
    dimbounds[i*2] = tmp;
    dimbounds[i*2+1] = tmp;
  }
  //add margin for each dimension
  for (int i = 0; i < 6; i++) {
    double margintmp = std::pow(-1.0,i+1)*margin*dimbounds[i];
    bounds[i] += margintmp;//add 10 % margin
  }
  whiteImage->SetSpacing(spacing);
  int dim[3];
  for (int i = 0; i < 3; i++) {
    dim[i] = static_cast<int>(ceil((bounds[i * 2 + 1] - bounds[i * 2]) / spacing[i]));
  }
  whiteImage->SetDimensions(dim);
  whiteImage->SetExtent(0, dim[0] - 1, 0, dim[1] - 1, 0, dim[2] - 1);
 
    double origin[3];
    origin[0] = bounds[0] + spacing[0] / 2;
    origin[1] = bounds[2] + spacing[1] / 2;
    origin[2] = bounds[4] + spacing[2] / 2;
    whiteImage->SetOrigin(origin);
 
#if VTK_MAJOR_VERSION <= 5
    whiteImage->SetScalarTypeToUnsignedChar();
    whiteImage->AllocateScalars();
#else
    whiteImage->AllocateScalars(VTK_UNSIGNED_CHAR,1);
#endif
    // fill the image with foreground voxels:
    unsigned char inval = col;
    unsigned char outval = 0;
    vtkIdType count = whiteImage->GetNumberOfPoints();
    for (vtkIdType i = 0; i < count; ++i) {
    whiteImage->GetPointData()->GetScalars()->SetTuple1(i, inval);
    }
 
    // polygonal data --> image stencil:
    vtkSmartPointer<vtkPolyDataToImageStencil> pol2stenc = 
      vtkSmartPointer<vtkPolyDataToImageStencil>::New();
#if VTK_MAJOR_VERSION <= 5
    pol2stenc->SetInput(pd);
#else
    pol2stenc->SetInputData(pd);
#endif
    //pol2stenc->SetTolerance(0.0);
    pol2stenc->SetOutputOrigin(origin);
    pol2stenc->SetOutputSpacing(spacing);
    pol2stenc->SetOutputWholeExtent(whiteImage->GetExtent());
    pol2stenc->Update();
 
    // cut the corresponding white image and set the background:
    vtkSmartPointer<vtkImageStencil> imgstenc = 
      vtkSmartPointer<vtkImageStencil>::New();
#if VTK_MAJOR_VERSION <= 5
    imgstenc->SetInput(whiteImage);
    imgstenc->SetStencil(pol2stenc->GetOutput());
#else
    imgstenc->SetInputData(whiteImage);
    imgstenc->SetStencilConnection(pol2stenc->GetOutputPort());
#endif
    imgstenc->ReverseStencilOff();
    imgstenc->SetBackgroundValue(outval);
    imgstenc->Update();
    vtkSmartPointer<vtkImageData> outImage = imgstenc->GetOutput();
    return outImage;
}

