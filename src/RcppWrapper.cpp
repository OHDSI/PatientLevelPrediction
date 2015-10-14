/**
 * @file RcppWrapper.cpp
 *
 * This file is part of PatientLevelPrediction
 *
 * Copyright 2015 Observational Health Data Sciences and Informatics
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef __RcppWrapper_cpp__
#define __RcppWrapper_cpp__


#include <Rcpp.h>
#include "Auc.h"
#include "BySum.h"
#include "ByMax.h"

using namespace Rcpp;

// [[Rcpp::export(".aucWithCi")]]
std::vector<double> aucWithCi(std::vector<double> propensityScores, std::vector<int> treatment) {

  using namespace ohdsi::patientLevelPrediction;

  try {
		std::vector<double> auc = Auc::aucWithCi(propensityScores, treatment);
		return auc;
	} catch (std::exception &e) {
		forward_exception_to_r(e);
	} catch (...) {
		::Rf_error("c++ exception (unknown reason)");
	}
  std::vector<double> auc(3,0);
	return auc;
}

// [[Rcpp::export(".auc")]]
double auc(std::vector<double> propensityScores, std::vector<int> treatment) {

  using namespace ohdsi::patientLevelPrediction;

  try {
		double auc = Auc::auc(propensityScores, treatment);
		return auc;
	} catch (std::exception &e) {
		forward_exception_to_r(e);
	} catch (...) {
		::Rf_error("c++ exception (unknown reason)");
	}
	return 0.0;
}

// [[Rcpp::export(".bySum")]]
DataFrame bySum(List ffValues, List ffBins) {
  
  using namespace ohdsi::patientLevelPrediction;
  
  try {
    std::map<double,double> map = BySum::bySum(ffValues, ffBins);
    std::vector<double> bins;
    std::vector<double> sums;
    for(std::map<double,double>::iterator iter = map.begin(); iter != map.end(); ++iter){
      bins.push_back(iter->first);
      sums.push_back(iter->second);
    }
    return DataFrame::create(_["bins"] = bins, _["sums"] = sums);
  } catch (std::exception &e) {
    forward_exception_to_r(e);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  return DataFrame::create();
}

// [[Rcpp::export(".byMax")]]
DataFrame byMax(List ffValues, List ffBins) {
  
  using namespace ohdsi::patientLevelPrediction;
  
  try {
    std::map<double,double> map = ByMax::byMax(ffValues, ffBins);
    std::vector<double> bins;
    std::vector<double> maxs;
    for(std::map<double,double>::iterator iter = map.begin(); iter != map.end(); ++iter){
      bins.push_back(iter->first);
      maxs.push_back(iter->second);
    }
    return DataFrame::create(_["bins"] = bins, _["maxs"] = maxs);
  } catch (std::exception &e) {
    forward_exception_to_r(e);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  return DataFrame::create();
}

#endif // __RcppWrapper_cpp__