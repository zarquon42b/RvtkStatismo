#ifndef _BUILD_GP_MODEL_H__
#define _BUILD_GP_MODEL_H__

#include "VTKTypes.h"
#include "pPCA2statismo.h"
#include "KernelCombinators.h"
#include "LowRankGPModelBuilder.h"
#include "polyData2R.h"



using namespace Rcpp;
using namespace statismo;

class GaussianKernel: public ScalarValuedKernel<vtkPoint> {
public:

  GaussianKernel(double sigma) : m_sigma(sigma), m_sigma2(sigma * sigma) {
  }

  inline double operator()(const vtkPoint& x, const vtkPoint& y) const {
    VectorType r(3);
    r << x[0] - y[0], x[1] - y[1], x[2] - y[2];
    return exp(-r.dot(r) / m_sigma2);
  }

  std::string GetKernelInfo() const {
    std::ostringstream os;
    os << "GaussianKernel(" << m_sigma << ")";
    return os.str();
  }

private:

  double m_sigma;
  double m_sigma2;
};

class IsoKernel: public MatrixValuedKernel<vtkPoint> {
public:
  
  IsoKernel(unsigned dim, double sigma, vtkPoint centroid) :
    MatrixValuedKernel<vtkPoint>(dim), m_sigma(sigma),m_sigma2(sigma*sigma), m_center(centroid) {}
  
  
  inline MatrixType operator()(const vtkPoint& x, const vtkPoint& y) const {
    MatrixType covar(3,3);
    VectorType x0(3), y0(3);
    for (int i=0; i < 3;i++) {
      y0[i] = y[i] - m_center[i];
      x0[i] = x[i] - m_center[i];
    }
    //Rcout << m_center[0] << endl;
    covar = (x0*y0.transpose())*m_sigma2;
    //Rcout << covar << endl << endl;
    return covar;
  }

  std::string GetKernelInfo() const {
    std::ostringstream os;
    os << "IsoKernel(" << m_sigma << ")";
    return os.str();
  }

private:

  double m_sigma;
  double m_sigma2;
  vtkPoint m_center;
  
  
};

typedef GaussianKernel GaussianKernelType;
typedef MatrixValuedKernel<vtkPoint> MatrixValuedKernelType;
typedef LowRankGPModelBuilder<vtkPolyData> ModelBuilderType;

shared_ptr<vtkMeshModel> BuildGPModel(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_, SEXP useEmp_, SEXP combine_, SEXP combineEmp_ , SEXP isoScale_, SEXP centroid_);

RcppExport SEXP BuildGPModelExport(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_, SEXP useEmp_, SEXP combine_, SEXP combineEmp_,SEXP isoScale_, SEXP centroid_);
#endif //_BUILD_GP_MODEL_H__
