#include <vtkTransformPolyDataFilter.h>
#include <vtkLandmarkTransform.h>
#include <vtkMath.h>
#include <vtkMatrix4x4.h>
#include <vtkSmartPointer.h>
#include <vtkImageReader2Factory.h>
#include <vtkImageReader2.h>
#include <vtkImageData.h>
#include <vtkImageReslice.h>
#include <vtkXMLImageDataWriter.h>
#include <vtkMetaImageWriter.h>
#include <Rcpp.h>
using namespace Rcpp;
//transforms a 3 x k SEXP matrix into vtkPoints
vtkSmartPointer<vtkPoints> R2vtkPoints(SEXP vb_) {
  NumericMatrix vb(vb_);
  vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
  for (int i = 0; i < vb.ncol();i++) {
    float p[3];
    for (int j = 0;j < 3; j++) 
      p[j] = vb(j,i);  
    vtkIdType pid[1];
    pid[0] = points->InsertNextPoint(p);
  }
  return points;
}

RcppExport SEXP vtkLMTransfrorm(SEXP images_, SEXP reflm_, SEXP tarlm_ , SEXP outname_,SEXP type_, SEXP interpolation_) {
  try{ 
    int interpolation = as<int>(interpolation_);
    std::string type = as<std::string>(type_);
    std::string inputFilename = as<std::string>(images_);
    std::string outputFilename = as<std::string>(outname_);
    //calculate landmark transform
    vtkSmartPointer<vtkLandmarkTransform> landmarkTransform = vtkSmartPointer<vtkLandmarkTransform>::New();
    vtkSmartPointer<vtkPoints> sourcePoints = R2vtkPoints(reflm_);
    vtkSmartPointer<vtkPoints> targetPoints = R2vtkPoints(tarlm_);
    
    
    //read image
    vtkSmartPointer<vtkImageReader2Factory> readerFactory = vtkSmartPointer<vtkImageReader2Factory>::New();
    vtkImageReader2* imageReader = readerFactory->CreateImageReader2(inputFilename.c_str());
    if (imageReader) {
      imageReader->SetFileName(inputFilename.c_str());
      imageReader->Update();
      vtkSmartPointer<vtkImageData> image =vtkSmartPointer<vtkImageData>::New();
      image = imageReader->GetOutput();
            landmarkTransform->SetSourceLandmarks(sourcePoints);
      landmarkTransform->SetTargetLandmarks(targetPoints);
      if (type == "a")
	landmarkTransform->SetModeToAffine();
      else if (type == "s")
	landmarkTransform->SetModeToSimilarity();
      else 
	landmarkTransform->SetModeToRigidBody();
      landmarkTransform->Update();
            
      //transform image
      vtkSmartPointer<vtkImageReslice> transform2 = vtkSmartPointer<vtkImageReslice>::New(); // Apply transform
#if VTK_MAJOR_VERSION <= 5  
    transform2->SetInput(image);
#else
    transform2->SetInformationInput(image);
#endif
      landmarkTransform->Inverse();
      transform2->SetResliceTransform(landmarkTransform);
      transform2->AutoCropOutputOn();
      transform2->SetInterpolationMode(interpolation);
      transform2->Update();
      vtkSmartPointer<vtkImageData> transformImage = transform2->GetOutput();
    
      //write image to file
      vtkSmartPointer<vtkMetaImageWriter> writermha = vtkSmartPointer<vtkMetaImageWriter>::New();
      writermha->SetFileName(outputFilename.c_str());
#if VTK_MAJOR_VERSION <= 5
      writermha->SetInputConnection(transformImage->GetProducerPort());
#else
      writermha->SetInputData(transformImage);
#endif
      writermha->Write();
      imageReader->Delete();
      
    } else {
      ::Rf_error("Unsupported image format");
    }
    return wrap(0);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }
}
