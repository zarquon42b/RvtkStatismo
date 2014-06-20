#include "VTKTypes.h"
#include "pPCA2statismo.h"
#include "statismo/LowRankGPModelBuilder.h"
#include "polyData2R.h"

#ifndef _BUILD_GP_MODEL_H__
#define _BUILD_GP_MODEL_H__
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

typedef GaussianKernel GaussianKernelType;
typedef MatrixValuedKernel<vtkPoint> MatrixValuedKernelType;
typedef LowRankGPModelBuilder<vtkPolyData> ModelBuilderType;

auto_ptr<StatisticalModelType> BuildGPModel(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_= wrap(500), SEXP useEmp_ = wrap(true));

RcppExport SEXP BuildGPModelExport(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_, SEXP useEmp_);
#endif //_BUILD_GP_MODEL_H__