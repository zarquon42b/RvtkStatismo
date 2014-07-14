#include "vtkImageIO.h"
#include "Rcpp.h"

vtkSmartPointer<vtkImageData> vtkImageRead(std::string inputFilename) {
  vtkSmartPointer<vtkImageData> image =vtkSmartPointer<vtkImageData>::New();
  vtkSmartPointer<vtkImageReader2Factory> readerFactory = vtkSmartPointer<vtkImageReader2Factory>::New();
  vtkImageReader2* imageReader = readerFactory->CreateImageReader2(inputFilename.c_str());
   
  if (imageReader) {
    imageReader->SetFileName(inputFilename.c_str());
    imageReader->Update();
    image = imageReader->GetOutput();
    imageReader->Delete();
    return image;
  }
      
#if VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1
  //try NIFTII
  else {
    vtkSmartPointer<vtkNIFTIImageReader> niftiReader = vtkSmartPointer<vtkNIFTIImageReader>::New();
    if (niftiReader->CanReadFile(inputFilename.c_str())) {
      niftiReader->SetFileName(inputFilename.c_str());
      niftiReader->Update();
      image = niftiReader->GetOutput();
      vtkSmartPointer<vtkTransform> niftiiTransform = vtkSmartPointer<vtkTransform>::New();
      vtkSmartPointer<vtkMatrix4x4> iotrans = niftiReader->GetSFormMatrix();
      niftiiTransform->SetMatrix(iotrans);
      vtkSmartPointer<vtkImageReslice> transform2 = vtkSmartPointer<vtkImageReslice>::New();
      transform2->SetResliceTransform(niftiiTransform);
      transform2->AutoCropOutputOn();
      transform2->SetInterpolationMode(2);
 #if VTK_MAJOR_VERSION <= 5  
      transform2->SetInput(image);
 #else
      transform2->SetInputData(image);
 #endif
      transform2->Update();
      vtkSmartPointer<vtkImageData> transformImage = transform2->GetOutput();
      return transformImage;
    } else {
      ::Rf_error("image not readable");
      
    }
#else
     else
       ::Rf_error("image not readable");
#endif
    
}
