#include "R2polyData.h"
#include "polyData2R.h"
#include <vtkVersion.h>
#include <vtkMassProperties.h>
#include <vtkFillHolesFilter.h>

RcppExport SEXP vtkMeshInfo(SEXP refmesh_) {
  try {
    List out;
    List refmesh(refmesh_);
    vtkSmartPointer<vtkPolyData> source = R2polyData(refmesh["vb"],refmesh["it"]);
    vtkSmartPointer<vtkMassProperties> myprops = vtkSmartPointer<vtkMassProperties>::New();
#if VTK_MAJOR_VERSION <= 5
    myprops->SetInput(source);
#else
    myprops->SetInputData(source);
#endif
    
    double volume = myprops->GetVolume();
    out["volume"] = volume;
    out["projVol"] = myprops->GetVolumeProjected();
    out["surfaceArea"] = myprops->GetSurfaceArea();
    return out;
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    
  } catch (...) {
    ::Rf_error("unknown exception");
    
  }
}

RcppExport SEXP vtkFillHole(SEXP refmesh_, SEXP holesize_) {
  try {
    List out;
    List refmesh(refmesh_);
    double holesize = as<double>(holesize_);
    vtkSmartPointer<vtkPolyData> source = R2polyData(refmesh["vb"],refmesh["it"]);
    vtkSmartPointer<vtkFillHolesFilter> fillHoles = vtkSmartPointer<vtkFillHolesFilter>::New();
#if VTK_MAJOR_VERSION <= 5
    fillHoles->SetInput(source);
#else
    fillHoles->SetInputData(source);
#endif
    
    fillHoles->SetHoleSize(holesize);
    fillHoles->Update();
    
    vtkSmartPointer<vtkPolyData> outmesh = fillHoles->GetOutput();
      return polyData2R(outmesh);
  } catch (std::exception& e) {
    ::Rf_error( e.what());
    
  } catch (...) {
    ::Rf_error("unknown exception");
    
  }
}
