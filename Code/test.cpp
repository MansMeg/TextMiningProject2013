#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int matIndex(IntegerMatrix mat,int i,int j) {
  int res = mat(i,j);
  return res;
}

// [[Rcpp::export]]
int RcppSample(IntegerVector probspace,NumericVector probs) {
  RNGScope scp;
  Rcpp::Function RcppSample("sample");
  int z = Rcpp::as<int>(RcppSample(_["x"]=probspace,_["size"]=1,_["prob"]=probs));
  return z;
}

// [[Rcpp::export]]
int RcppWhichMax(NumericVector test) {
  int z = Rcpp::which_max(test);
  return z;
}


