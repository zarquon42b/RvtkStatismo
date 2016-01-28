#include "vtkImageIO.h"
#include "RcppEigen.h"

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
      
#if (VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1) || VTK_MAJOR_VERSION > 6
  //try NIFTII
  else {
    vtkSmartPointer<vtkNIFTIImageReader> niftiReader = vtkSmartPointer<vtkNIFTIImageReader>::New();
    if (niftiReader->CanReadFile(inputFilename.c_str())) {
      niftiReader->SetFileName(inputFilename.c_str());
      niftiReader->Update();
      image = niftiReader->GetOutput();
      vtkSmartPointer<vtkTransform> niftiiTransform = vtkSmartPointer<vtkTransform>::New();
      vtkSmartPointer<vtkMatrix4x4> iotrans0 = niftiReader->GetQFormMatrix();
      //iotrans0->Invert();
      vtkSmartPointer<vtkMatrix4x4> ras =vtkSmartPointer<vtkMatrix4x4>::New();
      ras->Identity();
      ras->SetElement(0,0,-1);
      ras->SetElement(1,1,-1);
      vtkSmartPointer<vtkMatrix4x4> iotrans = vtkSmartPointer<vtkMatrix4x4>::New();
      vtkMatrix4x4::Multiply4x4(ras,iotrans0,iotrans);
      niftiiTransform->SetMatrix(iotrans);
      niftiiTransform->Inverse();
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
  }
#else
  else
    ::Rf_error("image not readable");
#endif
    
}

//write images to MHA and NIFTI 
int vtkImageWrite(vtkSmartPointer<vtkImageData> image, std::string outputFilename) {
  std::string ext = vtksys::SystemTools::GetFilenameLastExtension(outputFilename);
  std::string fname = vtksys::SystemTools::GetFilenameWithoutLastExtension(outputFilename);    
  std::string ext1 = vtksys::SystemTools::GetFilenameLastExtension(fname);
  std::string extext = ext1+ext;
  vtkSmartPointer<vtkMatrix4x4> ras =vtkSmartPointer<vtkMatrix4x4>::New();
  ras->Identity();
  ras->SetElement(0,0,-1);
  ras->SetElement(1,1,-1);
  vtkSmartPointer<vtkImageWriter> writer;
#if (VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1) || VTK_MAJOR_VERSION > 6
  
  if (ext.compare(".nii") ==0 || extext.compare(".nii.gz") == 0) { 
    vtkSmartPointer<vtkNIFTIImageWriter> writertmp = vtkSmartPointer<vtkNIFTIImageWriter>::New();
    
    writertmp->SetQFormMatrix(ras);
    writertmp->SetSFormMatrix(ras);
    writer = writertmp;      
  } else if (ext.compare(".mhd") ==0 || ext.compare(".mha") == 0) {
    writer = vtkSmartPointer<vtkMetaImageWriter>::New();
  } else
    ::Rf_error("Unsupported output format");
#else
  if (ext.compare(".mhd") ==0 || ext.compare(".mha") == 0)
    writer = vtkSmartPointer<vtkMetaImageWriter>::New();
  else 
    ::Rf_error("Unsupported output format");
#endif
  writer->SetFileName(outputFilename.c_str());
#if VTK_MAJOR_VERSION <= 5
  writer->SetInputConnection(image->GetProducerPort());
#else
  writer->SetInputData(image);
#endif
  writer->Write();
  return 0;
}
