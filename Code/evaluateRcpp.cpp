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
List gibbsZRcpp(IntegerVector z, IntegerVector w, NumericVector alpha, NumericMatrix phi, int iter, bool forward, bool allz) {
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
  
  // Create matrix if all z should be returned
  IntegerMatrix zzall(iter,Nd);
  IntegerMatrix Nzzall(iter,K);
  
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
    if (allz){
      zzall(sweep,_) = clone<IntegerVector>(zz);
      Nzzall(sweep,_) = clone<IntegerVector>(Nzz);
    }  
  }

  // Save output
  List ret;
  if (not allz){
    ret["Nz"] = Nzz;
    ret["z"] = zz;
  } else {
    ret["NzMatrix"] = Nzzall;
    ret["zMatrix"] = zzall;    
  }
  return ret;
}

// Iterative conditional means (z*)
// [[Rcpp::export]]
List icmZRcpp(IntegerVector z, IntegerVector w, NumericVector alpha, NumericMatrix phi, int iter, bool forward) {
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
  
  // Loop
  int i;
  for (int sweep = 0;sweep < iter; ++sweep){
    IntegerVector zzstart = clone<IntegerVector>(zz); 
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
        // Choose z[n] with largest prob
        zz[i] = Rcpp::which_max(prob)+1;
        Nzz[zz[i]-1] += 1; 
    }
    if (all(zzstart == zz).is_true()){
      break;
    }
  }

  // Save output
  List ret;
  ret["Nz"] = Nzz;
  ret["z"] = zz;
  return ret;
}


// Transition operator probability
// [[Rcpp::export]]
double logTprobRcpp(IntegerVector zto, IntegerVector zfrom, IntegerVector Nz, IntegerVector w, NumericVector alpha, NumericMatrix phi) {
  // Create parameters
  int Nd = w.size();
  int K = phi.nrow();
  
  IntegerVector zzto = clone<IntegerVector>(zto); 
  IntegerVector zzfrom = clone<IntegerVector>(zfrom); 
  IntegerVector Nzz = clone<IntegerVector>(Nz);
  NumericVector prob(K);
  IntegerVector itervec = seq_len(Nd);
  
  double lp = 0;
  
  // Loop
  int i;
  for (int h = 0;h < Nd; ++h){
      i = itervec[h]-1;
      Nzz[zzfrom[i]-1] -= 1;
      for (int j = 0;j < K; ++j){
        prob[j] = phi(j,w[i]-1) * (Nzz[j]+alpha[j]);
      } 

      // Normalizing
      double probSum = std::accumulate(prob.begin(),prob.end(),0.0);
      for (int j = 0;j < K; ++j){
        prob[j] /= probSum;
      }
      // Add log(P)
      lp += log(prob[zzto[i]-1]);
      Nzz[zzto[i]-1] += 1; 
  }

  return lp;
}

// Calculating forward and backward part och the Chib Method
// [[Rcpp::export]]
NumericVector chibIterateRcpp(IntegerVector zstart, IntegerVector zstar, IntegerVector w, NumericVector alpha, NumericMatrix phi, int iter, bool forward) {
  NumericVector logTProbValue(iter);
  int K = phi.nrow();
  List gibbZ;
  gibbZ["Nz"] = calcNzRcpp(K,zstart);
  gibbZ["z"] = clone<IntegerVector>(zstart);  

  bool allz = FALSE;
  for (int i = 0;i < iter; ++i){
    gibbZ = gibbsZRcpp(gibbZ["z"],w,alpha,phi,1,forward,allz);
    logTProbValue[i] = logTprobRcpp(zstar,gibbZ["z"],gibbZ["Nz"],w,alpha,phi);
  }
  
  return logTProbValue;
}


// Calculating P(w|z,phi)
// [[Rcpp::export]]
double logProbDoc(IntegerVector w, IntegerVector z, NumericMatrix phi){
  double resprob = 0;
  for (int i = 0;i < w.size(); ++i){
    resprob += log(phi(z[i]-1,w[i]-1));
  }
  return resprob;
}