// in r: install rcpp library and ZigguratRCPP library, then source this script and the function becomes available (source('DDM_KSfitting'))
#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;
// [[Rcpp::export]]
NumericMatrix DDM_CCB_binary(int n_trials, double a, double v, double a_slope, double ter, double ter2, double vratio, double absolute_height, double collapse_timer) {
  
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
    double cj = -1;

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
    evidence = 0;
    if (cor == 0){
      v_post = -1 * v_post;
    }

    while ((evidence < absolute_height) && (t2 < collapse_timer)){
      t2 = t2 + dt;
      evidence = evidence + v_post * dt + s * sqrt(dt) * zigg.norm();
    }

    if (evidence >= absolute_height){
      cj = 1;
    } else {
      cj = 0;
    }

    DATA(i,2) = t2 + ter2;
    DATA(i,3) = cj;
  }
  return DATA; // decision RT, decision accuracy, confidence RT, confidence judgment
}
