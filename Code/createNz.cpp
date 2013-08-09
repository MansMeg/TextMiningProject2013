#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector createNz(IntegerVector Nz, IntegerVector z) {
  int nz = z.size();
  for (int i = 0;i < nz; ++i){
    Nz[z[i]-1] += 1;
  }
  return Nz;
}

/*** R

*/