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
#include <vtkMath.h>
#include <vtkMatrix4x4.h>
#include <vtkTransform.h>

vtkSmartPointer<vtkImageData> vtkImageRead(std::string inputFilename) ;
