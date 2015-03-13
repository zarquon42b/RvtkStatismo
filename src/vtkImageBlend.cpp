#include <vtkTransformPolyDataFilter.h>
#include <vtkLandmarkTransform.h>
#include <vtkMath.h>
#include <vtkMatrix4x4.h>
#include <vtkImageBlend.h>
#include <vtkImageCast.h>
#include <vtkSmartPointer.h>
#include <vtkImageReader2Factory.h>
#include <vtkImageReader2.h>
#include <vtkImageReader.h>
#include <vtkImageData.h>
#include <vtkActor.h>
#include <vtkDataSet.h>
#include <vtkDataSetMapper.h>
#include <vtkImageBlend.h>
#include <vtkImageData.h>
#include <vtkImageMapToColors.h>
#include <vtkLookupTable.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkRenderer.h>
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


RcppExport SEXP vtkImageBlender(SEXP images1_, SEXP images2_, SEXP outname_) {
  try{ 

    std::string inputFilename1 = as<std::string>(images1_);
    std::string inputFilename2 = as<std::string>(images2_);
    std::string outputFilename = as<std::string>(outname_);
    //calculate landmark transform
    
    vtkSmartPointer<vtkImageData> image1 = vtkImageRead(inputFilename1);
    vtkSmartPointer<vtkImageData> image2 = vtkImageRead(inputFilename2);
    vtkSmartPointer<vtkImageBlend> blend = vtkSmartPointer<vtkImageBlend>::New();
    vtkSmartPointer<vtkImageCast> castFilter = vtkSmartPointer<vtkImageCast>::New();
#if VTK_MAJOR_VERSION <= 5
    castFilter->SetInput(image2);
#else
    castFilter->SetInputData(image2);
#endif 
    castFilter->SetOutputScalarTypeToUnsignedChar();
    int ctype = image1->GetScalarType();
    castFilter->SetOutputScalarType(ctype);
    castFilter->Update();
#if VTK_MAJOR_VERSION <= 5                  
    blend->AddInput(image1);
    blend->AddInput(castFilter->GetOutput());
#else
    blend->AddInputData(image1);                
    blend->AddInputData(castFilter->GetOutput());
   
#endif
  
    blend->SetOpacity(1,.5);
    vtkSmartPointer<vtkImageData> outputImage = blend->GetOutput();
    //write image to file
    vtkImageWrite(outputImage,outputFilename);
    return wrap(0);
} catch (std::exception& e) {
  ::Rf_error( e.what());
 } catch (...) {
  ::Rf_error("unknown exception");
 }
}
