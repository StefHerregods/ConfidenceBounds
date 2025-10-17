// in r: install rcpp library and ZigguratRCPP library, then source this script and the function becomes available (source('DDM_KSfitting'))
#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;
// [[Rcpp::export]]
NumericMatrix DDM_confidence_bounds_binary(int n_trials, double a, double v, double a_slope, double ter, double a2, double vratio, double a2_slope_upper, double a2_slope_lower, double ter2, double starting_point_confidence) {
  
  // initialize output
  NumericMatrix DATA(n_trials,4);
  
  // initialize constants
  double s = 1;
  double dt = 0.001;
  double z = 0.5;

  // loop over trials
  for (int i = 0; i < n_trials; i++) {

    // initialize variables
    double evidence = a*z;
    double t = 0;
    int cor = -1;
    double t2 = 0;

    // Decisional processing
    while (evidence <= a && evidence >=0){
      
      t = t + dt;
      evidence = evidence + v * dt + s * sqrt(dt) * zigg.norm();
      
      if (evidence >= a - t * a_slope){
        cor = 1;
        break;
      } else if (evidence <= 0 + t * a_slope) {
        cor = 0;
        break;
      }
    }

    DATA(i,0) = (t + ter);
    DATA(i,1) = cor;

    // Post-decisional processing

    double v_post = v * vratio;
    evidence = a2 * starting_point_confidence;
    if (cor == 0){
      v_post = -1 * v_post;
    }

    while ((evidence < a2 - t2*a2_slope_upper) && (evidence > t2*a2_slope_lower)){
      t2 = t2 + dt;
      evidence = evidence + v_post * dt + s * sqrt(dt) * zigg.norm();
    }

    if (evidence >= a2 / 2){
      DATA(i,3) = 1;
    } else {
      DATA(i,3) = 0;
    }

    DATA(i,2) = t2 + ter2;
  }
  return DATA; // decision RT, decision accuracy, confidence RT, confidence judgment
}
