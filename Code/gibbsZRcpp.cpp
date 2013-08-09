#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List gibbsZRcpp(IntegerVector Nz, IntegerVector z, IntegerVector w, IntegerVector alpha, NumericMatrix phi, IntegerVector itervec, int iter, bool mode) {
  // Create parameters
  int n = itervec.size();
  int K = Nz.size();
  NumericVector prob(K);
  
  // Loop
  int i;
  for (int sweep = 0;sweep < iter; ++sweep){
    for (int h = 0;h < n; ++h){
      i = itervec[h];
      Nz[z[i]-1] -= 1; // Done
      for (int j = 0;j < K; ++j){
        prob[j] = phi[j,w[i]-1] * (Nz[j]+alpha[j]);
      }
      //Normalizing prob
      double probSum = std::accumulate(prob.begin(),prob.end(),0.0);
      for (int j = 0;j < K; ++j){
        prob[j] /= probSum;
      }
      // Select/sample z[n]
      if(mode){// ICM
        z[i] = Rcpp::which_max(prob)+1;
      }else{// Sample
        RNGScope scp;
        Rcpp::Function RcppSample("sample");
        IntegerVector samplespace = seq_len(K);
        z[i] = Rcpp::as<int>(RcppSample(_["x"]=samplespace,_["size"]=1,_["prob"]=prob));
      }
      Nz[z[i]-1] += 1;
    }
  }

  // Save output
  List ret;
  ret["Nz"] = Nz;
  ret["x"] = z;
  return ret;
}

/*** R
#  for (sweep in 1:iter){
#    for (n in itervec){
#      Nz[z[n]]<-Nz[z[n]]-1
#      for (k in 1:K){
#        prob[k] <- phi[k,w[n]] * (Nz[k]+alpha[k])/(N-1+sumAlpha)
#      }
#      prob<-prob/sum(prob)
#      z[n]<-sample(1:K,1,prob=prob)
#      Nz[z[n]]<-Nz[z[n]]+1
#    }
#  }
#  return(list(z,Nz))
*/