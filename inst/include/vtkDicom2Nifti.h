#include <vtkImageData.h>
#include <vtkImageThreshold.h>
#include <vtkVersion.h>
#include <vtkSurfaceReconstructionFilter.h>
#include <vtkProgrammableSource.h>
#include <vtkContourFilter.h>
#include <vtkReverseSense.h>
#include <vtkProperty.h>
#include <vtkMath.h>
#include <vtkSmartPointer.h>
#include <cmath>
#include <vtkTransform.h>
#include <vtkTransformPolyDataFilter.h>
#include "vtkImageIO.h"
#include "RcppEigen.h"
using namespace Rcpp;

RcppExport SEXP vtkDicom2Nifti(SEXP inputFilename_, SEXP outputFilename);
