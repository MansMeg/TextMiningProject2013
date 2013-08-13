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
IntegerVector createNzRcpp(int K, IntegerVector z) {
  int nz = z.size();
  IntegerVector NzRes(K);
  
  for (int i = 0;i < nz; ++i){
    NzRes[z[i]-1] += 1;
  }
  return NzRes;
}

// [[Rcpp::export]]
IntegerVector createVecRcpp(int Nd, bool forward) {
  IntegerVector itervec = seq_len(Nd);
  if (not forward){
    itervec = rev(seq_len(Nd));
  }
  return itervec;
}

// [[Rcpp::export]]
int testBreak(int K, int breakNum) {
  IntegerVector itervec = seq_len(K);
  int res = 0;
    for (int i = 0;i < K; ++i){
      if (i==breakNum){
        res = i;
        break;
        }
    }
  return res;
}

// [[Rcpp::export]]
double testLogCpp(int K) {
  double res = log(K);
  return res;
}

// [[Rcpp::export]]
List mkListCpp(int K,int k) {
  List res;
  res["K"] = K;
  res["k"] = k;
  return res;
}

// [[Rcpp::export]]
int testCpp(int num) {
  List test(mkListCpp(5,10));
  int myres;
  if (num==1){
    myres = Rcpp::as<int>(test["K"]);
  } else {
    myres = Rcpp::as<int>(test["k"]);
  }
  return myres;
}


