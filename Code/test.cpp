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


int forward(int i,int j) {
  int res = i+j;
  return res;
}

// [[Rcpp::export]]
int backward(int i,int j) {
  int res = i-j+forward(i,j);
  return res;
}

// [[Rcpp::export]]
int testfun(Function test,int i,int j) {
  int res = Rcpp::as<int>(test(i,j));
  return res;
}
// [[Rcpp::export]]
int tryitall(bool back,int i,int j) {
  int res;
  if (back){
    res = Rcpp::as<int>(testfun(backward,i,j));
  }
  if (not back){
    res = Rcpp::as<int>(testfun(forward,i,j));
  }
  return res;
}
