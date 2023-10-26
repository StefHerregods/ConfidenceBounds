// in r: install rcpp library and ZigguratRCPP library, then source this script and the function becomes available (source('DDM_KSfitting'))
#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;
// [[Rcpp::export]]
NumericMatrix DDM_confidence_bounds(double v, double a, double ter, double z, int ntrials, double s, double dt, double a2, double postdriftmod, double a2_slope, double ter2) {
  
  // initialize output
  NumericMatrix DATA(ntrials,7);
  
  // loop over trials
  for (int i = 0; i < ntrials; i++) {
    
    // initialize variables
    int resp = -1;
    int acc = 0;
    double evidence = a*z;
    double t = 0;
    double t2 = 0;
    
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
    
    //Post-decisional processing
    double v_post = v * postdriftmod;
    if (resp == 1){
      while ((evidence < a + a2/2 - t2*a2_slope) && (evidence > a - a2/2 + t2*a2_slope)){
        t2 = t2 + dt;
        evidence = evidence + v_post * dt + s * sqrt(dt) * zigg.norm();
        if (evidence >= a + a2/2 - t2*a2_slope){
          DATA(i,6) = 1;
        } else if (evidence <= a - a2/2 + t2*a2_slope){
          DATA(i,6) = 0;
          }
      }
    } else if (resp == -1){
      while ((evidence < a2/2 - t2*a2_slope) && (evidence > -a2/2 + t2*a2_slope)){
        t2 = t2 + dt;
        evidence = evidence + v_post * dt + s * sqrt(dt) * zigg.norm();
        if (evidence >= a2/2 - t2*a2_slope){
          DATA(i,6) = 0;
        } else if (evidence <= - a2/2 + t2*a2_slope){
          DATA(i,6) = 1;
        }
      }
    }
    DATA(i,3) = evidence;
    DATA(i,4) = (t + t2 + ter + ter2);
    DATA(i,5) = t2 + ter2;
  }
  return DATA; //RT (including non-decision time), resp (1 or -1), accuracy (1 or 0), evidence2 (control variable), RTfull, RTconfidence, ConfidenceRating (high-1 or low-0)
}
