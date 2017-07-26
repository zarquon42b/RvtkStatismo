#include "ModelMembers.h"
#include "Helpers.h"

using namespace Rcpp;
using namespace Eigen;

RcppExport SEXP GetFullJacobianCpp(SEXP pPCA_, SEXP pts_) {
  try {
    NumericMatrix pts(pts_);
    XPtr<vtkMeshModel> model = pPCA2statismo(pPCA_);
    VectorXf pcavar = model->GetPCAVarianceVector();
    const int npc = pcavar.size();

    MatrixXf fullmat(3*pts.nrow(), npc);
    
      for (int i = 0; i < pts.nrow(); i++) {
	NumericVector tmppt = pts.row(i);
	vtkPoint pt = NumVec2vtkPoint(pts.row(i));
	MatrixXf cov = model->GetJacobian(pt);
	fullmat.block(i*3,0,3,npc) = cov;
    }
    return wrap(fullmat);
  }

  catch (std::exception& e) {
    ::Rf_error( e.what());
    
  } catch (...) {
    ::Rf_error("unknown exception");
    
  }
}
