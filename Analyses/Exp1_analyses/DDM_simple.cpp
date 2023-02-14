// in r: install rcpp library and ZigguratRCPP library, then source this script and the function becomes available (source('DDM_KSfitting'))
#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;
// [[Rcpp::export]]
NumericMatrix DDM_confidence_bounds(double v, double a, double ter, double z, int ntrials, double s, double dt) {
  
  // initialize output
  NumericMatrix DATA(ntrials,3);
  
  // loop over trials
  for (int i = 0; i < ntrials; i++) {
    
    // initialize variables
    int resp = -1;
    int acc = 0;
    double evidence = a*z;
    double t = 0;

    // Decisional processing
    while (t > -1){
      
      t = t + dt;
      evidence = evidence + v * dt + s * sqrt(dt) * zigg.norm();
      
      if (evidence >= a){
        resp = 1;
        evidence = a;
        if (v >= 0){ 
          acc = 1;
        }
        break;
      } else if (evidence <= 0) {
        resp = -1;
        evidence=0;
        if (v < 0){
          acc = 1;
        }
        break;
      }
    }
    
    DATA(i,0) = (t + ter);
    DATA(i,1) = resp;
    DATA(i,2) = acc;
  }
  return DATA; //RT (including non-decision time), resp (1 or -1), accuracy (1 or 0)
}
