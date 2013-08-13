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

// Gibbs sampling of Z (for heldout data w)
// [[Rcpp::export]]
List chibIterateRcpp(IntegerVector zstar, IntegerVector w, NumericVector alpha, NumericMatrix phi, int iter, bool forward) {
  
}

/*
# Draw forward part 
for (step in (ss+1):Siter){ #All måste bygga på zstar och framåt - gör en Rcpploop för denna och nedan
  gibbzz<-gibbSample(K=K,N=Nd,phi=phi,w=w,z=zstar,alpha=alpha,iter=1,forward=TRUE,mode=FALSE)
  logTvals[step]<-logTprob(zto=zstar,zfrom=gibbzz[[1]],Nz=gibbzz[[2]],phi=phi,w=w,alpha=alpha)
}
# Draw backward part
for (step in (ss-1):1){
  gibbzz<-gibbSample(K=K,N=Nd,phi=phi,w=w,z=zstar,alpha=alpha,iter=1,forward=FALSE)
  logTvals[step]<-logTprob(zto=zstar,zfrom=gibbzz[[1]],Nz=gibbzz[[2]],phi=phi,w=w,alpha=alpha)
}

*/

