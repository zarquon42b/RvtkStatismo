#include "vtkDicom2Nifti.h"

RcppExport SEXP vtkDicom2Nifti(SEXP inputFilename_, SEXP outputFilename_) {
   try {
     std::string inputFilename = as<std::string>(inputFilename_);
     std::string outputFilename = as<std::string>(outputFilename_);
     vtkSmartPointer<vtkImageData> image = vtkImageRead(inputFilename,true);
     vtkImageWrite(image,outputFilename);
     return wrap(0);
   } catch (std::exception& e) {
     ::Rf_error( e.what());
   } catch (...) {
     ::Rf_error("unknown exception");
   }
}
