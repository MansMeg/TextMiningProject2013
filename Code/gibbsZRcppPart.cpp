#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List gibbsZRcpp(IntegerVector Nz, IntegerVector z, IntegerVector w, IntegerVector alpha, NumericMatrix phi, IntegerVector itervec, int iter, bool mode) {
  // Create parameters
  int n = itervec.size();
  int K = Nz.size();
  IntegerVector zz = clone<IntegerVector>(z); 
  IntegerVector Nzz = clone<IntegerVector>(Nz);
  NumericVector prob(K);
  // IntegerMatrix debug(n,K); //tabort

  RNGScope scp; //Cant do this given !mode BUGreport?
  Rcpp::Function RcppSample("sample");
  IntegerVector samplespace = seq_len(K);

  // Loop
  int i;
  int randomz;
  for (int sweep = 0;sweep < iter; ++sweep){
    for (int h = 0;h < n; ++h){
        i = itervec[h]-1;
        Nzz[zz[i]-1] -= 1; // not word h ERROR
        for (int j = 0;j < K; ++j){
          prob[j] = phi(j,w[i]-1) * (Nzz[j]+alpha[j]);
        } 
        // Normalizing
        double probSum = std::accumulate(prob.begin(),prob.end(),0.0);
        for (int j = 0;j < K; ++j){
          prob[j] /= probSum;
        }
        // Select/sample z[n]
        if(mode){// ICM
          zz[i] = Rcpp::which_max(prob)+1;
        }else{// Sample
          zz[i] = Rcpp::as<int>(RcppSample(_["x"]=samplespace,_["size"]=1,_["prob"]=prob));
        }
        Nzz[zz[i]-1] += 1; // TA BORT
        //save every step
        //debug(h,_) = clone<IntegerVector>(Nzz);
    }
  }

  // Save output
  List ret;
  ret["Nz"] = Nzz;
  ret["z"] = zz;
  //ret["prob"] = prob; //ta bort
  //ret["debug"] = debug; //ta bort  
  //ret["samlespace"] = samplespace; //ta bort
  //ret["K"] = K; //ta bort
  //ret["n"] = n; //ta bort
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