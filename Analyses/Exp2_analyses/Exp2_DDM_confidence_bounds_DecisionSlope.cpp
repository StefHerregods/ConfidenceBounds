// in r: install rcpp library and ZigguratRCPP library, then source this script and the function becomes available (source('DDM_KSfitting'))
#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;
// [[Rcpp::export]]
NumericMatrix DDM_confidence_bounds(double v, double a, double a_slope, double ter, double z, int ntrials, double s, double dt, double a2_upper, double a2_lower, double postdriftmod, double a2_slope_upper, double a2_slope_lower, double ter2) {
  
  // initialize output
  NumericMatrix DATA(ntrials,8);
  
  // loop over trials
  for (int i = 0; i < ntrials; i++) {
    
    // initialize variables
    int resp = -1;
    int acc = 0;
    double evidence = a*z;
    double t = 0;
    double t2 = 0;
    double evidence_at_decision = 0;
    
    // Decisional processing
    while (t > -1){
      
      t = t + dt;
      evidence = evidence + v * dt + s * sqrt(dt) * zigg.norm();
      
      if (evidence >= a - (t * a_slope)){
        resp = 1;
        evidence = a - (t * a_slope);
        if (v >= 0){ 
          acc = 1;
        }
        break;
      } else if (evidence <= 0 + (t * a_slope)) {
        resp = -1;
        evidence= 0 + (t * a_slope);
        if (v < 0){
          acc = 1;
        }
        break;
      }
    }
    
    evidence_at_decision = evidence;
    DATA(i,0) = (t + ter);
    DATA(i,1) = resp;
    DATA(i,2) = acc;
    DATA(i,7) = evidence;

    //Post-decisional processing
    double v_post = v * postdriftmod;
    if (resp == 1){
      while ((evidence < evidence_at_decision + a2_upper - t2*a2_slope_upper) && (evidence > evidence_at_decision - a2_lower + t2*a2_slope_lower)){
        t2 = t2 + dt;
        evidence = evidence + v_post * dt + s * sqrt(dt) * zigg.norm();
        if (evidence >= evidence_at_decision + a2_upper - t2*a2_slope_upper){
          DATA(i,6) = 1;
        } else if (evidence <= evidence_at_decision - a2_lower + t2*a2_slope_lower){
          DATA(i,6) = 0;
          }
      }
    } else if (resp == -1){
      while ((evidence < evidence_at_decision + a2_lower - t2*a2_slope_lower) && (evidence > evidence_at_decision - a2_upper + t2*a2_slope_upper)){
        t2 = t2 + dt;
        evidence = evidence + v_post * dt + s * sqrt(dt) * zigg.norm();
        if (evidence >= evidence_at_decision + a2_lower - t2*a2_slope_lower){
          DATA(i,6) = 0;
        } else if (evidence <= evidence_at_decision - a2_upper + t2*a2_slope_upper){
          DATA(i,6) = 1;
        }
      }
    }
    DATA(i,3) = evidence;
    DATA(i,4) = (t + t2 + ter + ter2);
    DATA(i,5) = t2 + ter2;
  }
  return DATA; //RT (including non-decision time), resp (1 or -1), accuracy (1 or 0), evidence2 (control variable), RTfull, RTconfidence, ConfidenceRating (high-1 or low-0), evidence at decision-making

}
