#ifndef _BUILD_GP_MODEL_H__
#define _BUILD_GP_MODEL_H__

#include "VTKTypes.h"
#include "pPCA2statismo.h"
#include "KernelCombinators.h"
#include "LowRankGPModelBuilder.h"
#include "polyData2R.h"
#include <boost/ptr_container/ptr_vector.hpp>



using namespace Rcpp;
using namespace statismo;

class GaussianKernel: public ScalarValuedKernel<vtkPoint> {
public:

  GaussianKernel(double sigma) : m_sigma(sigma), m_sigma2(sigma * sigma) {
  }

  inline double operator()(const vtkPoint& x, const vtkPoint& y) const {
    VectorType r(3);
    r << x[0] - y[0], x[1] - y[1], x[2] - y[2];
    return exp(-r.dot(r) / (2 * m_sigma2));
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

class NeutralSumKernel: public MatrixValuedKernel<vtkPoint> {
public:
  
  NeutralSumKernel() :
    MatrixValuedKernel<vtkPoint>(3) {}
  
  
  inline MatrixType operator()(const vtkPoint& x, const vtkPoint& y) const {
    MatrixType covar = MatrixType::Zero(3,3);
    return covar;
  }

  std::string GetKernelInfo() const {
    std::ostringstream os;
    os << "NeutralSumKernel";
    return os.str();
  }


  
  
};

class NeutralProductKernel: public MatrixValuedKernel<vtkPoint> {
public:
  
  NeutralProductKernel() :
    MatrixValuedKernel<vtkPoint>(3) {}
  
  
  inline MatrixType operator()(const vtkPoint& x, const vtkPoint& y) const {
    MatrixType covar = MatrixType::Identity(3,3);
    return covar;
  }

  std::string GetKernelInfo() const {
    std::ostringstream os;
    os << "NeutralProductKernel";
    return os.str();
  }


  
  
};


inline vtkPoint vtkPointScale(vtkPoint x,double scale) {
  for (unsigned int i=0; i < 3; i++) {
    x[i] = x[i]*scale;
  }
  return x;
}

inline vtkPoint vtkPointSum(vtkPoint x,vtkPoint y) {
  for (unsigned int i=0; i < 3; i++) {
    x[i] = x[i]+y[i];
  }
  return x;
}
inline vtkPoint vtkPointDif(vtkPoint x,vtkPoint y) {
  for (unsigned int i=0; i < 3; i++) {
    x[i] = x[i]-y[i];
  }
  return x;
}

class BSplineKernel : public statismo::ScalarValuedKernel<vtkPoint> {
public:
  
  BSplineKernel(double support) : m_support(support) {}


  // the 3rd order b-spline basis function
  double bspline3(const double& x) const {
    const double absX = std::fabs(x);
    const double absXSquared = absX * absX;
    const double absXCube = absXSquared * absX;
    const double twoMinAbsX = 2.0 - absX;
    const double twoMinAbsXCube = twoMinAbsX * twoMinAbsX * twoMinAbsX;
    const double twoByThree = 2.0 / 3.0;

    double splineValue = 0;
    if (absX >= 0 && absX < 1) {
      splineValue = twoByThree - absXSquared + 0.5 * absXCube;
    }
    else if (absX >= 1 && absX < 2) {
      splineValue = twoMinAbsXCube / 6.0;
    }
    else {
      splineValue = 0;
    }
    return splineValue;
  }

  double tensorProductSpline(const vtkPoint& x) const {
    double prod = 1;
    for (unsigned d = 0; d < 3; ++d) {
      prod *= bspline3(x[d]);
    }
    return prod;
  }


  inline double operator()(const vtkPoint& x, const vtkPoint& y) const {

    const unsigned dim =3;
    const double supportBasisFunction = 4.0;
    const double scale = -1.0 * std::log(m_support / supportBasisFunction) / std::log(2.0);
      
    vtkPoint xScaled = vtkPointScale(x, std::pow(2.0, scale));
    vtkPoint yScaled = vtkPointScale(y, std::pow(2.0, scale));
      
    std::vector<int> kLower(dim);
    std::vector<int> kUpper(dim);
    for (unsigned d = 0; d < dim; ++d) {
      kLower[d] = static_cast<int>(std::ceil(std::max(xScaled[d], yScaled[d]) - 0.5 * supportBasisFunction));
      kUpper[d] = static_cast<int>(std::floor(std::min(xScaled[d], yScaled[d]) + 0.5 * supportBasisFunction));
    }
      

    // We need to generate the cartesian product k_1 x ... x k_d, where k_i goes through all the integers
    // within the given bounds. A non-recursive solution requires d loops. Here we just write down the cases
    // for 1 2 and 3D
    double sum = 0.0;
    double kx = kLower[0];
    while (kx <= kUpper[0]) {
      double ky = kLower[1];
      while (ky <= kUpper[1]) {
	double kz = kLower[2];
	while (kz <= kUpper[2]) {
	  vtkPoint k;
	  k[0] = kx; k[1] = ky; k[2] = kz;
	  sum += (tensorProductSpline(vtkPointDif(xScaled, k)) * tensorProductSpline(vtkPointDif(yScaled, k)));
	  kz += 1;
	}
	ky += 1;
      }
      kx += 1;
    }
    
    return sum;
  }

  std::string GetKernelInfo() const {
    std::ostringstream os;
    os << "BSplineKernel(" << m_support << ")";
    return os.str();
  }

private:
  double m_support;
};

class MultiscaleKernel : public statismo::ScalarValuedKernel<vtkPoint> {
public:
  

  MultiscaleKernel(double supportBaseLevel, unsigned numberOfLevels) :
    m_supportBaseLevel(supportBaseLevel),
    m_numberOfLevels(numberOfLevels)
  {
    double support = supportBaseLevel;
    for (unsigned i = 0; i < numberOfLevels; ++i) {
      m_kernels.push_back(new BSplineKernel(m_supportBaseLevel * std::pow(2, -1.0 * i)));
      m_kernelWeights.push_back(std::pow(2, -1.0 * i));
    }
  }



  inline double operator()(const vtkPoint& x, const vtkPoint& y) const {

    const unsigned dim = 3;
    double sum = 0;
    for (unsigned i = 0; i < m_kernels.size(); ++i) {
      sum += m_kernels[i](x, y) * m_kernelWeights[i];
    }

    return sum;
  }

  std::string GetKernelInfo() const {
    std::ostringstream os;
    os << "MultiscaleKernel(" << m_supportBaseLevel << ", " << m_numberOfLevels << ")";
    return os.str();
  }

private:
  double m_supportBaseLevel;
  unsigned m_numberOfLevels;
  boost::ptr_vector<BSplineKernel>  m_kernels;
  std::vector<double> m_kernelWeights;
};



typedef GaussianKernel GaussianKernelType;
typedef MatrixValuedKernel<vtkPoint> MatrixValuedKernelType;
typedef LowRankGPModelBuilder<vtkPolyData> ModelBuilderType;

XPtr<vtkMeshModel> BuildGPModel(XPtr<vtkMeshModel> model, SEXP mykernel_, SEXP ncomp_,SEXP nystroem_);

RcppExport SEXP BuildGPModelExport(SEXP pPCA_,SEXP kernels_, SEXP ncomp_,SEXP nystroem_, SEXP pointer_);
#endif //_BUILD_GP_MODEL_H__
