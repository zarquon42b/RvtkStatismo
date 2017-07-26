#ifndef _VTKIMAGETRANSFORM_H__
#define _VTKIMAGETRANSFORM_H__
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
#include <RcppEigen.h>
using namespace Rcpp;

RcppExport SEXP vtkImageTransformCpp(SEXP images_, SEXP reflm_, SEXP tarlm_ , SEXP outname_,SEXP type_, SEXP interpolation_);

#endif //_VTKIMAGETRANSFORM_H__
