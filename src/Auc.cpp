/**
 * @file Auc.cpp
 *
 * This file is part of PatientLevelPrediction
 *
 * Copyright 2019 Observational Health Data Sciences and Informatics
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

#ifndef __Auc_cpp__
#define __Auc_cpp__

#include <cmath>
#include <iostream>

#include "Auc.h"

namespace ohdsi {
namespace patientLevelPrediction {

double Auc::mannWhitneyKernel(const double &x, const double &y) {
  if (y < x) {
    return 1;
  }
  if (y == x) {
    return 0.5;
  }
  return -0;
}

double Auc::auc(const std::vector<double> &propensityScores, const std::vector<int> &treatment) {
  unsigned long int m = 0;
  for (unsigned long int i = 0; i < treatment.size(); i++) {
    if (treatment.at(i) == 1) {
      m++;
    }
  }
  unsigned long int n = treatment.size() - m;
  double *cases;
  double *controls;
  cases = new double[m];
  controls = new double[n];
  m = 0;
  n = 0;
  for (unsigned long int i = 0; i < treatment.size(); i++) {
    if (treatment.at(i) == 1) {
      cases[m++] = propensityScores.at(i);
    } else {
      controls[n++] = propensityScores.at(i);
    }
  }
  double mean = 0;
  for (unsigned long int i = 0; i < m; i++) {
    double localMean = 0;
    for (unsigned long int j = 0; j < n; j++) {
      double mw = mannWhitneyKernel(cases[i], controls[j]);
      localMean += mw;
    }
    mean += localMean / n;
  }
  mean /= m;
  delete[] cases;
  delete[] controls;
  return mean;
}

std::vector<double> Auc::aucWithCi(const std::vector<double> &propensityScores, const std::vector<int> &treatment) {
  unsigned long int m = 0;
  unsigned long int n = 0;
  for (unsigned long int i = 0; i < treatment.size(); i++) {
    if (treatment.at(i) == 1) {
      m++;
    } else {
      n++;
    }
  }
  double *cases;
  double *controls;
  cases = new double[m];
  controls = new double[n];
  m = 0;
  n = 0;
  for (unsigned long int i = 0; i < treatment.size(); i++) {
    if (treatment.at(i) == 1) {
      cases[m++] = propensityScores.at(i);
    } else {
      controls[n++] = propensityScores.at(i);
    }
  }
  double mean = 0;
  for (unsigned long int i = 0; i < m; i++) {
    double localMean = 0;
    for (unsigned long int j = 0; j < n; j++) {
      double mw = mannWhitneyKernel(cases[i], controls[j]);
      localMean += mw;
    }
    mean += localMean / n;
  }
  mean /= m;
  
  double *vr10;
  vr10 = new double[m];
  for (unsigned long int i = 0; i < m; i++) {
    double sum = 0;
    for (unsigned long int j = 0; j < n; j++) {
      sum += mannWhitneyKernel(cases[i], controls[j]);
    }
    vr10[i] = sum / n;
  }
  
  double *vr01;
  vr01 = new double[n];
  for (unsigned long int i = 0; i < n; i++) {
    double sum = 0;
    for (unsigned long int j = 0; j < m; j++) {
      sum += mannWhitneyKernel(cases[j], controls[i]);
    }
    vr01[i] = sum / m;
  }
  
  double s10 = 0;
  for (unsigned long int i = 0; i < m; i++) {
    s10 += (vr10[i] - mean) * (vr10[i] - mean);
  }
  s10 /= (double) (m - 1);
  
  double s01 = 0;
  for (unsigned long int i = 0; i < n; i++) {
    s01 += (vr01[i] - mean) * (vr01[i] - mean);
  }
  s01 /= (double) (n - 1);
  
  double s = s10 / m + s01 / n;
  double sd = sqrt(s);
  
  delete[] cases;
  delete[] controls;
  delete[] vr10;
  delete[] vr01;
  
  std::vector<double> ci;
  ci.push_back(mean);
  ci.push_back(mean - (1.96 * sd));
  ci.push_back(mean + (1.96 * sd));
  return ci;
}
}
}

#endif // __Auc_cpp__
