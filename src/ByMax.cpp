/**
 * @file ByMax.cpp
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

#ifndef __ByMax_cpp__
#define __ByMax_cpp__

#include "ByMax.h"


namespace ohdsi {
namespace patientLevelPrediction {

std::map<double, double> ByMax::byMax(const List &ffValues, const List &ffBins){
  std::map<double, double> binToValue;
  Environment bit = Environment::namespace_env("bit");
  Function chunk = bit["chunk"];
  List chunks = chunk(ffValues);
  Environment ff = Environment::namespace_env("ff");
  Function subset = ff["[.ff"];
  for (int i = 0; i < chunks.size(); i++){
    NumericVector bins = subset(ffBins, chunks[i]);
    NumericVector values = subset(ffValues, chunks[i]);
    for (int j = 0; j < bins.size(); j++){
      binToValue[bins[j]] = std::max(binToValue[bins[j]],values[j]);
    }
  }
  return binToValue;
}
}
}

#endif // __ByMax_cpp__
