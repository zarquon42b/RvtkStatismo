#include <vtkIterativeClosestPointTransform.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkLandmarkTransform.h>
#include <vtkMath.h>
#include <vtkMatrix4x4.h>
#include "R2polyData.h"
#include "polyData2R.h"
RcppExport SEXP vtkICP(SEXP refmesh_, SEXP tarmesh_ , SEXP iterations_, SEXP scale_) {
  try {
    bool scale = as<bool>(scale_);
    List refmesh(refmesh_);
    List tarmesh(tarmesh_);
    SEXP tarvb = tarmesh["vb"];
    SEXP tarit = tarmesh["it"];
    int iterations = as<int>(iterations_);
    vtkSmartPointer<vtkPolyData> source = R2polyData(refmesh["vb"],refmesh["it"]);
    
    vtkSmartPointer<vtkPolyData> target = R2polyData(tarmesh["vb"],tarmesh["it"]);
    vtkSmartPointer<vtkIterativeClosestPointTransform> icp = vtkSmartPointer<vtkIterativeClosestPointTransform>::New();
  icp->SetSource(source);
  icp->SetTarget(target);
  if (!scale)
    icp->GetLandmarkTransform()->SetModeToRigidBody();
  else 
    icp->GetLandmarkTransform()->SetModeToSimilarity();
  
  icp->SetMaximumNumberOfIterations(iterations);
  //icp->StartByMatchingCentroidsOn();
  icp->Modified();
  icp->Update();
  
  // Get the resulting transformation matrix (this matrix takes the source points to the target points)
  vtkSmartPointer<vtkMatrix4x4> m = icp->GetMatrix();
  vtkSmartPointer<vtkTransformPolyDataFilter> icpTransformFilter =
    vtkSmartPointer<vtkTransformPolyDataFilter>::New();
#if VTK_MAJOR_VERSION <= 5
  icpTransformFilter->SetInput(source);
#else
  icpTransformFilter->SetInputData(source);
#endif
  icpTransformFilter->SetTransform(icp);
  icpTransformFilter->Update();
  
  vtkSmartPointer<vtkPolyData> mapped = icpTransformFilter->GetOutput();
 
 List out = polyData2R(mapped);
  //return out;
  return out;
 } catch (std::exception& e) {
    ::Rf_error( e.what());
    return wrap(1);
  } catch (...) {
    ::Rf_error("unknown exception");
    return wrap(1);
  }

}
