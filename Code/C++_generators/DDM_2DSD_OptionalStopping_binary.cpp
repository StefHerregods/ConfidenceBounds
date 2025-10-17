// in r: install rcpp library and ZigguratRCPP library, then source this script and the function becomes available (source('DDM_KSfitting'))
#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;

#include <iostream>
#include <random>

std::mt19937 gen(std::random_device{}()); 

bool generate_TF(double probability) {
  std::bernoulli_distribution dist(probability);
  return dist(gen);
}

// [[Rcpp::export]]
NumericMatrix DDM_2DSD_OptionalStopping_scale6(int n_trials, double a, double v, double a_slope, double ter, double ter2, double vratio, double cj0, double cj1) {
  
  // initialize output
  NumericMatrix DATA(n_trials,4);

  // initialize constants
  double s = 1;
  double dt = 0.001;
  double z = 0.5;
  cj2 = cj2 + cj1;
  cj3 = cj3 + cj2;
  cj4 = cj4 + cj3;
  cj5 = cj5 + cj4;
  cj6 = cj6 + cj5;  
  
  // loop over trials
  for (int i = 0; i < n_trials; i++) {

    // initialize variables
    double evidence = a*z;
    double t = 0;
    int cor = -1;
    double t2 = 0;
    double cj = 6;

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
    double evidence_current = 0;
    double evidence_previous = 0;
    bool absorbing = false;
    if (cor == 0){
      v_post = -1 * v_post;
    }  

    while (evidence_current <= cj6 && evidence_current >= cj1){

      t2 = t2 + dt;
      evidence_current = evidence_current + v * dt + s * sqrt(dt) * zigg.norm();

      if ((cj2 >= evidence_previous && cj2 < evidence_current) || (cj2 >= evidence_current && cj2 < evidence_previous)){
        absorbing = generate_TF(cj2_p);
        if (absorbing == true){
          cj = 2;
          break;
        }
      } else if ((cj3 >= evidence_previous && cj3 < evidence_current) || (cj3 >= evidence_current && cj3 < evidence_previous)){
        absorbing = generate_TF(cj3_p);
        if (absorbing == true){
          cj = 3;
          break;
        }
      } else if ((cj4 >= evidence_previous && cj4 < evidence_current) || (cj4 >= evidence_current && cj4 < evidence_previous)){
        absorbing = generate_TF(cj4_p);
        if (absorbing == true){
          cj = 4;
          break;
        }
      } else if ((cj5 >= evidence_previous && cj5 < evidence_current) || (cj5 >= evidence_current && cj5 < evidence_previous)){
        absorbing = generate_TF(cj5_p);
        if (absorbing == true){
          cj = 5;
          break;
        }
      } else if (evidence_current >= cj6){
        cj = 6;
        break;
      } else if (evidence_current <= cj1){
        cj = 1;
        break;
      }

      evidence_previous = evidence_current;

    }

    DATA(i,2) = t2 + ter2;
    DATA(i,3) = cj;
  }
  return DATA; // decision RT, decision accuracy, confidence RT, confidence judgment
}
