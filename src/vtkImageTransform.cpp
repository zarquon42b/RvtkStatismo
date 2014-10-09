#include <vtkTransformPolyDataFilter.h>
#include <vtkLandmarkTransform.h>
#include <vtkMath.h>
#include <vtkMatrix4x4.h>
#include <vtkSmartPointer.h>
#include <vtkImageReader2Factory.h>
#include <vtkImageReader2.h>
#include <vtkImageReader.h>
#include <vtkImageData.h>
#include <vtkImageReslice.h>
#include <vtkImageWriter.h>
#include <vtkMetaImageWriter.h>
#include <vtksys/SystemTools.hxx>
#if VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1
#include <vtkNIFTIImageReader.h>
#include <vtkNIFTIImageWriter.h>
#endif
#include "vtkImageTransform.h"
#include "R2vtkPoints.h"
#include <RcppEigen.h>
#include "vtkImageIO.h"

using namespace Rcpp;


SEXP vtkImageTransform(SEXP images_, SEXP reflm_, SEXP tarlm_ , SEXP outname_,SEXP type_, SEXP interpolation_) {
  try{ 
    int interpolation = as<int>(interpolation_);
    std::string type = as<std::string>(type_);
    std::string inputFilename = as<std::string>(images_);
    std::string outputFilename = as<std::string>(outname_);
    //calculate landmark transform
    vtkSmartPointer<vtkLandmarkTransform> landmarkTransform = vtkSmartPointer<vtkLandmarkTransform>::New();
    vtkSmartPointer<vtkPoints> sourcePoints = R2vtkPoints(reflm_);
    vtkSmartPointer<vtkPoints> targetPoints = R2vtkPoints(tarlm_);
    vtkSmartPointer<vtkImageData> image = vtkImageRead(inputFilename);
	
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
    transform2->SetInputData(image);
#endif
    landmarkTransform->Inverse();
    transform2->SetResliceTransform(landmarkTransform);
    transform2->AutoCropOutputOn();
    transform2->SetInterpolationMode(interpolation);
    transform2->Update();
    vtkSmartPointer<vtkImageData> transformImage = transform2->GetOutput();
    
    //write image to file
    vtkImageWrite(transformImage,outputFilename);
    
    return wrap(0);
} catch (std::exception& e) {
  ::Rf_error( e.what());
 } catch (...) {
  ::Rf_error("unknown exception");
 }
}
