// in r: install rcpp library and ZigguratRCPP library, then source this script and the function becomes available (source('DDM_KSfitting'))
#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
#include <random>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;
// [[Rcpp::export]]
NumericMatrix DDM_2DSD_binary(int n_trials, double v, double a, double a_slope, double ter, double ter2, double eta, double sz, double tau, double vratio, double c1) {

  // initialize output
  NumericMatrix DATA(n_trials,4);

  // initialize constants
  double s = 1;
  double dt = 0.001;
  double z = 0.5;

  // uniform distribution
  std::default_random_engine generator; // Random number generator
  std::uniform_real_distribution<double> distribution(a*z - sz*0.5, a*z + sz*0.5); // Uniform distribution 
  
  // loop over trials
  for (int i = 0; i < n_trials; i++) {

    // initialize variables
    double evidence = distribution(generator);
    double t = 0;
    int cor = -1;
    double t2 = 0;
    v = v + zigg.norm() * eta;

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
      v_post = (-1) * v_post;
    }  

    while (t2 < tau){
      evidence = evidence + v_post * dt + sqrt(dt) * zigg.norm();
      t2 = t2 + dt;    
    }

    if (evidence < c1){
      DATA(i,3) = 0;
    } else{
      DATA(i,3) = 1;
    }

    DATA(i,2) = tau + ter2;
  }
  return DATA; // decision RT, decision accuracy, confidence RT, confidence judgment
}
