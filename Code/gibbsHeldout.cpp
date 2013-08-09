#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector gibbsHeldout(IntegerMatrix w, NumericMatrix phi, NumericVector alpha,int iter,int thin,int burnin) {
  int k = phi.nrow();
  // Create vector T <- 1:20
  IntegerVector T = seq_len(k);
  // sample(T,1,probs)
  NumericVector prob(k,1.0/k);
  RNGScope scp;
  Rcpp::Function mySample("sample");
  int z = as<int>(mySample(T,1));
  return z;
  //IntegerMatrix z(10,1);
  //IntegerMatrix z = R::rmultinom(1.0, 1.0, p);
  //IntegerMatrix z = R::rmultinom(1.0, 1.0, prob2);
  //int z = R::sample(1:k,)
}

/*** R

*/