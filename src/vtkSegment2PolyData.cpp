#include <vtkImageData.h>
#include <vtkImageThreshold.h>
#include <vtkImageOpenClose3D.h>
#include <vtkDiscreteMarchingCubes.h>
#include <vtkInteractorStyleImage.h>
#include <vtkImageMapper3D.h>
#include <vtkVersion.h>
#include <vtkSurfaceReconstructionFilter.h>
#include <vtkProgrammableSource.h>
#include <vtkContourFilter.h>
#include <vtkReverseSense.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkPolyData.h>
#include <vtkMath.h>
#include <vtkSmartPointer.h>
#include <cmath>
#include <vtkTransform.h>
#include <vtkTransformPolyDataFilter.h>

#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkRenderer.h>
#include <vtkImageActor.h>
#include <vtkImageCast.h>
#include <vtkImageMandelbrotSource.h>

#include "RcppEigen.h"
#include "R2polyData.h"
#include "polyData2R.h"
#include "vtkpoly2unstruct.h"
#include "R2vtkPoints.h"
#include "vtkImageIO.h"


vtkSmartPointer<vtkImageThreshold> vtkThreshold(std::string inputFilename,double lower,double upper, double inValue, double outValue, bool dicom) {
  try
    {
      vtkSmartPointer<vtkImageData> image = vtkImageRead(inputFilename,dicom);
      vtkSmartPointer<vtkImageThreshold> imageThreshold = vtkSmartPointer<vtkImageThreshold>::New();

      imageThreshold->SetInputDataObject(image);
      imageThreshold->ThresholdBetween(lower, upper);
      imageThreshold->ReplaceInOn();
      imageThreshold->SetInValue(inValue);
      imageThreshold->SetOutValue(outValue);
      imageThreshold->Update();
      //vtkSmartPointer<vtkPolyData> newSurf = transform_back( points, reverse->GetOutput());
      return imageThreshold;
    }
  catch (std::exception& e)
    {
      ::Rf_error( e.what());
    }
  catch (...)
    {
      ::Rf_error("unknown exception");
    }
}

RcppExport SEXP vtkSegment2PolyData(SEXP inputFilename_,SEXP lower_,SEXP upper_,SEXP kernel_,SEXP dicom_,SEXP thresholding_) {
  try
    {
      double inValue = 1;
      double outValue = 0;
      double lower = as<double>(lower_);
      double upper = as<double>(upper_);
      bool dicom = as<bool>(dicom_);
      bool thresholding = as<bool>(thresholding_);
      std::string inputFilename = as<std::string>(inputFilename_);
      if (thresholding) {
	vtkSmartPointer<vtkImageThreshold> threshImage = vtkThreshold(inputFilename,lower,upper,inValue,outValue,dicom);
	vtkSmartPointer<vtkDiscreteMarchingCubes> discreteCubes = vtkSmartPointer<vtkDiscreteMarchingCubes>::New();
	  
	NumericVector kernel(kernel_);
	if (kernel[0]==0 && kernel[1]==0 && kernel[2]==0) {
	    discreteCubes->SetInputConnection(threshImage->GetOutputPort());
	} else {
	    vtkSmartPointer<vtkImageOpenClose3D> openedImage = vtkSmartPointer<vtkImageOpenClose3D>::New();
	    openedImage->SetInputConnection(threshImage->GetOutputPort());
	    openedImage->SetOpenValue(1);
	    openedImage->SetCloseValue(0);
	    openedImage->SetKernelSize(kernel[0],kernel[1],kernel[2]);
	    discreteCubes->SetInputConnection(openedImage->GetOutputPort());
	}
	  
	discreteCubes->GenerateValues(1, 1, 1);
	discreteCubes->ComputeNormalsOn();
	  
	// Sometimes the contouring algorithm can create a volume whose gradient
	// vector and ordering of polygon (using the right hand rule) are
	// inconsistent. vtkReverseSense cures this problem.
	vtkSmartPointer<vtkReverseSense> reverse = vtkSmartPointer<vtkReverseSense>::New();
	reverse->SetInputConnection(discreteCubes->GetOutputPort());
	reverse->ReverseCellsOn();
	reverse->ReverseNormalsOn();
	reverse->Update();
	  
	return polyData2R(discreteCubes->GetOutput());
      } else {
	vtkSmartPointer<vtkImageData> image = vtkImageRead(inputFilename,dicom);
	vtkSmartPointer<vtkMarchingCubes> surface = vtkSmartPointer<vtkMarchingCubes>::New();
#if VTK_MAJOR_VERSION <= 5
	surface->SetInput(image);
#else
	surface->SetInputData(image);
#endif
	surface->ComputeNormalsOn();
	surface->SetValue(0, lower);      
	vtkSmartPointer<vtkReverseSense> reverse = vtkSmartPointer<vtkReverseSense>::New();
	reverse->SetInputConnection(surface->GetOutputPort());
	reverse->ReverseCellsOn();
	reverse->ReverseNormalsOn();
	reverse->Update();
 
	//vtkSmartPointer<vtkPolyData> newSurf = transform_back( points, reverse->GetOutput());
	return polyData2R(surface->GetOutput());
      }
    } catch (std::exception& e) {
    ::Rf_error( e.what());
  } catch (...){
      ::Rf_error("unknown exception");
  }
}
