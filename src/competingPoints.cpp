#include "competingPoints.h"


IntegerVector sortInt(IntegerVector x) {
   IntegerVector y = clone(x);
   std::sort(y.begin(), y.end());
   return y;
}

SEXP competingPoints(SEXP pPCA_,SEXP sample_, SEXP indices_) {

   try {
     NumericMatrix sample(sample_);
     IntegerVector indices(indices_);
     IntegerVector ranges = sortInt(unique(indices));
     NumericMatrix goodverts(ranges.size(),3);
     NumericVector mahagood(ranges.size());
     IntegerVector goodrows(ranges.size());
     shared_ptr<vtkMeshModel> model = pPCA2statismo(pPCA_);
     NumericVector mahadistance(indices.size());
     unsigned int i = 0;
     for(IntegerVector::iterator it = ranges.begin(); it != ranges.end(); ++it) {
       double md = 1e10;
       for (unsigned int j = 0; j < indices.size(); j++) {
	 
	 if (indices[j] == *it) {
	   vtkPoint tmp0 = SEXP2vtkPoint(wrap(sample(j,_)));
	   vtkPoint tmp1 = model->DrawMeanAtPoint(indices[j]);
	   double mahaget = mahadist(model.get(),tmp0,tmp1);
	   mahadistance[j] = mahaget;
	   if (mahaget < md) {
	     md = mahaget;
	     
	     mahadistance[j] = mahaget;
	     goodverts(i,_) = sample(j,_);
	     mahagood[i] = mahaget;
	     goodrows[i] = j;
	   }
	 }
       }
       i++;
     }
     
       return Rcpp::List::create(Rcpp::Named("mahadistance") = mahadistance,
				 Rcpp::Named("goodverts") = goodverts,
				 Rcpp::Named("goodrows") = goodrows+1,
				 Rcpp::Named("mahagood") = mahagood
			      );
   }  catch (std::exception& e) {
     ::Rf_error( e.what());
   } catch (...) {
     ::Rf_error("unknown exception");
   }
}
