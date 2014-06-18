#include "VTKTypes.h"
#include "pPCA2statismo.h"
#include "statismo/LowRankGPModelBuilder.h"
#include "polyData2R.h"
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

auto_ptr<StatisticalModelType> BuildGPModel(SEXP pPCA_,SEXP kernels_, SEXP ncomp_) {
  int numberOfComponents = as<int>(ncomp_);
  List kernels(kernels_);
  try {
    auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
    const MatrixValuedKernelType& statModelKernel = StatisticalModelKernel<vtkPolyData>(model.get());
    //for (unsigned int i = 0; i < kernels.size();i++) {
    NumericVector kerntmp = kernels[0];
    double gaussianKernelSigma = kerntmp[0];
    double gaussianKernelScale = kerntmp[1];
    const GaussianKernel gk = GaussianKernel(gaussianKernelSigma);
    const MatrixValuedKernelType& mvGk = UncorrelatedMatrixValuedKernel<vtkPoint>(&gk, model->GetRepresenter()->GetDimensions());
    const MatrixValuedKernelType& scaledGk = ScaledKernel<vtkPoint>(&mvGk, gaussianKernelScale);
      
    const MatrixValuedKernelType& combinedModelAndGaussKernel = SumKernel<vtkPoint>(&statModelKernel, &scaledGk);

    auto_ptr<ModelBuilderType> modelBuilder(ModelBuilderType::Create(model->GetRepresenter()));
    auto_ptr<StatisticalModelType> combinedModel(modelBuilder->BuildNewModel(model->DrawMean(), combinedModelAndGaussKernel, numberOfComponents));
    //combinedModel->Save("test.h5");
    return combinedModel;
  }
  catch (StatisticalModelException& e) {
    Rprintf("Exception occured while building the shape model\n");
    Rprintf("%s\n",  e.what());
    auto_ptr<StatisticalModelType> model;
    return model;
  }
}
RcppExport SEXP BuildGPModelExport(SEXP pPCA_,SEXP kernels_, SEXP ncomp_){
  
  auto_ptr<StatisticalModelType> model = BuildGPModel(pPCA_,kernels_,ncomp_);
//return statismo2pPCA(model);
  vtkSmartPointer<vtkPolyData> reference = model->DrawMean();
  return statismo2pPCA(model);
  
}
RcppExport SEXP DrawMean(SEXP pPCA_){
  auto_ptr<StatisticalModelType> model = pPCA2statismo(pPCA_);
  vtkSmartPointer<vtkPolyData> reference = model->DrawMean();
  List out = polyData2R(reference);
  return out;
}

RcppExport SEXP LoadModel(SEXP modelname_){
  CharacterVector modelname(modelname_);
  vtkStandardMeshRepresenter* representer = vtkStandardMeshRepresenter::Create();
  std::string modelFilename = as<std::string>(modelname);
  

  auto_ptr<StatisticalModelType> model(StatisticalModelType::Load(representer, modelFilename));
  List out = statismo2pPCA(model);
  return out;
}
