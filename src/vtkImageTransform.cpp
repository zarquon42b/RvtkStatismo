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
#include <vtkXMLImageDataWriter.h>
#include <vtkImageWriter.h>
#include <vtkMetaImageWriter.h>
#include <vtksys/SystemTools.hxx>
#if VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1
#include <vtkNIFTIImageReader.h>
#include <vtkNIFTIImageWriter.h>
#endif

#include "R2vtkPoints.h"
#include <Rcpp.h>
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
    
    
    //read image
    vtkSmartPointer<vtkImageReader2Factory> readerFactory = vtkSmartPointer<vtkImageReader2Factory>::New();

    vtkImageReader2* imageReader = readerFactory->CreateImageReader2(inputFilename.c_str());
    //vtkSmartPointer<vtkImageReader>imageReader =  vtkSmartPointer<vtkImageReader>::New();
#if VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1
    if (!imageReader) {
      imageReader = vtkNIFTIImageReader::New();
    } 
    if (imageReader->CanReadFile(inputFilename.c_str()))  {
#else
      if (imageReader) {
#endif
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
	transform2->SetInputData(image);
#endif
	landmarkTransform->Inverse();
	transform2->SetResliceTransform(landmarkTransform);
	transform2->AutoCropOutputOn();
	transform2->SetInterpolationMode(interpolation);
	transform2->Update();
	vtkSmartPointer<vtkImageData> transformImage = transform2->GetOutput();
    
	//write image to file
	std::string ext = vtksys::SystemTools::GetFilenameLastExtension(outputFilename);

#if VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1
	vtkSmartPointer<vtkImageWriter> writer;
	if (ext.compare(".nii") || ext.compare(".gz")) { 
writer = vtkSmartPointer<vtkNIFTIImageWriter>::New();
	} else {
	  writer = vtkSmartPointer<vtkMetaImageWriter>::New();
	}
#else
	vtkSmartPointer<vtkImageWriter> writer = vtkSmartPointer<vtkMetaImageWriter>::New();
#endif
	writer->SetFileName(outputFilename.c_str());
#if VTK_MAJOR_VERSION <= 5
	writer->SetInputConnection(transformImage->GetProducerPort());
#else
	writer->SetInputData(transformImage);
#endif
	writer->Write();
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
