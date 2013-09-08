#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector calcNzRcpp(int K, IntegerVector z) {
  int nz = z.size();
  IntegerVector NzRes(K);
  
  for (int i = 0;i < nz; ++i){
    NzRes[z[i]-1] += 1;
  }
  return NzRes;
}

// Gibbs sampling of Z (for heldout data w)
// [[Rcpp::export]]
List gibbsZRcpp(IntegerVector z, IntegerVector w, NumericVector alpha, NumericMatrix phi, int iter, bool forward) {
  // Create parameters
  int Nd = w.size();
  int K = phi.nrow();
  
  IntegerVector zz = clone<IntegerVector>(z); 
  IntegerVector Nzz = calcNzRcpp(K,zz);
  NumericVector prob(K);
  IntegerVector itervec = seq_len(Nd);
  if (not forward){
    itervec = rev(seq_len(Nd));
  }
  
  RNGScope scp;
  Rcpp::Function RcppSample("sample");
  IntegerVector samplespace = seq_len(K);

  // Loop
  int i;
  for (int sweep = 0;sweep < iter; ++sweep){
    for (int h = 0;h < Nd; ++h){
        i = itervec[h]-1;
        Nzz[zz[i]-1] -= 1;
        for (int j = 0;j < K; ++j){
          prob[j] = phi(j,w[i]-1) * (Nzz[j]+alpha[j]);
        } 
        // Normalizing
        double probSum = std::accumulate(prob.begin(),prob.end(),0.0);
        for (int j = 0;j < K; ++j){
          prob[j] /= probSum;
        }
        // Sample z[n]
        zz[i] = Rcpp::as<int>(RcppSample(_["x"]=samplespace,_["size"]=1,_["prob"]=prob));
        Nzz[zz[i]-1] += 1; 
    }
  }

  // Save output
  List ret;
  ret["Nz"] = Nzz;
  ret["z"] = zz;
  return ret;
}

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
IntegerVector getPart(IntegerVector z, IntegerVector w, NumericVector alpha, NumericMatrix phi, int iter, bool forward) {
  List tempList = gibbsZRcpp(z=z,w=w,alpha=alpha,phi=phi,iter=iter,forward=TRUE);
  IntegerVector myres = tempList["z"];
  return myres;
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

