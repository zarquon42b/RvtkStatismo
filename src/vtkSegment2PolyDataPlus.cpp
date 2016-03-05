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

RcppExport SEXP vtkSegment2PolyDataPlus(SEXP inputFilename_,SEXP lower_,SEXP upper_,SEXP kernelX_,SEXP kernelY_,SEXP kernelZ_,SEXP dicom_) {
    try
    {
        double inValue = 1;
        double outValue = 0;
        double lower = as<double>(lower_);
        double upper = as<double>(upper_);
        bool dicom = as<bool>(dicom_);
        std::string inputFilename = as<std::string>(inputFilename_);
        vtkSmartPointer<vtkImageThreshold> threshImage = vtkThreshold(inputFilename,lower,upper,inValue,outValue,dicom);
        vtkSmartPointer<vtkDiscreteMarchingCubes> discreteCubes = vtkSmartPointer<vtkDiscreteMarchingCubes>::New();

        int kernelX = as<int>(kernelX_);
        int kernelY = as<int>(kernelY_);
        int kernelZ = as<int>(kernelZ_);
        if (kernelX==0 && kernelY==0 && kernelZ==0)
        {
            discreteCubes->SetInputConnection(threshImage->GetOutputPort());
        }
        else
        {
            vtkSmartPointer<vtkImageOpenClose3D> openedImage = vtkSmartPointer<vtkImageOpenClose3D>::New();
            openedImage->SetInputConnection(threshImage->GetOutputPort());
            openedImage->SetOpenValue(0);
            openedImage->SetCloseValue(1);
            openedImage->SetKernelSize(kernelX,kernelY,kernelZ);
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
