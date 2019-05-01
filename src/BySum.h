/**
 * @file BySum.h
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

#ifndef __BySum_h__
#define __BySum_h__

#include <vector>
#include <map>
#include <Rcpp.h>

using namespace Rcpp;

namespace ohdsi {
	namespace patientLevelPrediction {

		struct BySum {
		public:
			static std::map<double, double> bySum(const List &ffValues, const List &ffBins);
		};
	}
}

#endif // __BySum_h__
