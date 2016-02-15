#ifndef _VTK_IMAGE_IO_H__
#define _VTK_IMAGE_IO_H__
#include <vtkVersion.h>
#include <vtkSmartPointer.h>
#include <vtkImageReader2Factory.h>
#include <vtkImageReader2.h>
#include <vtkImageReader.h>
#include <vtkImageData.h>
#include <vtkImageReslice.h>
#include <vtkImageWriter.h>
#include <vtkMetaImageWriter.h>
#include <vtkDICOMImageReader.h>
#include <vtksys/SystemTools.hxx>
#if (VTK_MAJOR_VERSION > 5 && VTK_MINOR_VERSION > 1) || VTK_MAJOR_VERSION > 6
#include <vtkNIFTIImageReader.h>
#include <vtkNIFTIImageWriter.h>
#endif
#include <vtkMath.h>
#include <vtkMatrix4x4.h>
#include <vtkTransform.h>

vtkSmartPointer<vtkImageData> vtkImageRead(std::string inputFilename, bool dicom=false) ;

int vtkImageWrite(vtkSmartPointer<vtkImageData> image, std::string outputFilename);

#endif
