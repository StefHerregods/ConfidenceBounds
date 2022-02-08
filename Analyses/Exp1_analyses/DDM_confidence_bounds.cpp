// in r: install rcpp library and ZigguratRCPP library, then source this script and the function becomes available (source('DDM_KSfitting'))
#include <Rcpp.h>
// [[Rcpp::depends(RcppZiggurat)]]
#include <Ziggurat.h>
using namespace Rcpp;
static Ziggurat::Ziggurat::Ziggurat zigg;
// [[Rcpp::export]]
NumericMatrix DDM_confidence_bounds(double v, double a, double ter, double z, int ntrials, double s, double dt, double a2, double postdriftmod) {
  
  // initialize output
  NumericMatrix DATA(ntrials,8);
  
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
        evidence=a;
        //added = sign 
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
      while ((evidence < a + a2/2) && (evidence > a - a2/2)){
        t = t + dt;
        evidence = evidence + v_post * dt + s * sqrt(dt) * zigg.norm();
        if (evidence >= a + a2/2){
          DATA(i,7) = 1;
        } else if (evidence <= a - a2/2){
          DATA(i,7) = 0;
          }
      }
    } else if (resp == -1){
      while ((evidence < a2/2) && (evidence > -a2/2)){
        t = t + dt;
        evidence = evidence + v_post * dt + s * sqrt(dt) * zigg.norm();
        if (evidence >= a2/2){
          DATA(i,7) = 1;
        } else if (evidence <= - a2/2){
          DATA(i,7) = 0;
        }
      }
    }
    DATA(i,3) = evidence;
    DATA(i,4) = (t + ter);
    if (resp == 1){
      DATA(i,5) = evidence-a;
    }
    if (resp == -1){
      DATA(i,5) = -evidence; 
    }
    DATA(i,6) = t;
  }
  return DATA; //RT, resp, accuracy, evidence2, RTfull, confidence, RTconfidence, ConfBound
}
