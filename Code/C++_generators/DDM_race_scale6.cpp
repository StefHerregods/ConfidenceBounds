// in r: install rcpp library and ZigguratRCPP library, then source this script and the function becomes available 
#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;
// [[Rcpp::export]]
NumericMatrix DDM_race_scale6(double v_correct, double v_error, double a, double a2, double ter, double ter2, int n_trials) {
  
  // initialize output
  NumericMatrix DATA(n_trials,4);

  // initialize constants
  double s = 1;
  double dt = 0.001;
  
  // loop over trials
  for (int i = 0; i < n_trials; i++) {
    
    // initialize variables
    double evidence_correct = 0;
    double evidence_error = 0;
    double t = 0;
    int acc = -1;
    
    // Decisional processing
    while (evidence_correct < a && evidence_error < a){
      
      t = t + dt;
      evidence_correct = evidence_correct + v_correct * dt + s * sqrt(dt) * zigg.norm();
      evidence_error = evidence_error + v_error * dt + s * sqrt(dt) * zigg.norm();
      
      if (evidence_correct >= a){
        acc = 1;
        break;
      } else if (evidence_error >= a) {
        acc = 0;
        break;
      }

    }
    
    DATA(i,0) = (t + ter);
    DATA(i,1) = acc;

    // confidence processing
    t = 0;
    while (evidence_correct < a2 && evidence_error < a2){
      
      t = t + dt;
      evidence_correct = evidence_correct + v_correct * dt + s * sqrt(dt) * zigg.norm();
      evidence_error = evidence_error + v_error * dt + s * sqrt(dt) * zigg.norm();
      
      if (evidence_correct >= a2){
        break;
      } else if (evidence_error >= a2) {
        break;
      }

    }
    
    double cj_evidence = evidence_correct - evidence_error;
    if (cj_evidence > 2*a2/3){
      DATA(i,3) = 6;
    } else if (cj_evidence > 1*a2/3){
      DATA(i,3) = 5;
    } else if (cj_evidence > 0*a2/2){
      DATA(i,3) = 4;
    } else if (cj_evidence > -1*a2/2){
      DATA(i,3) = 3;
    } else if (cj_evidence > -2*a2/2){
      DATA(i,3) = 2;
    } else {
      DATA(i,3) = 1;
    }

    DATA(i,2) = (t + ter2);

  }

  return DATA;  // RT (including non-decision time), accuracy, confidence RT, confidence rating
}
