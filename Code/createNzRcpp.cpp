#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector createNzRcpp(IntegerVector NzNull, IntegerVector z) {
  int nz = z.size();
  for (int i = 0;i < nz; ++i){
    NzNull[z[i]-1] += 1;
  }
  return NzNull;
}

/*** R

*/