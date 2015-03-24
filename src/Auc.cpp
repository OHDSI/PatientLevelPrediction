/**
 * @file Auc.cpp
 *
 * This file is part of PatientLevelPrediction
 *
 * Copyright 2014 Observational Health Data Sciences and Informatics
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
	namespace cohortMethod {

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
			unsigned int m = 0;
			for (unsigned int i = 0; i < treatment.size(); i++) {
				if (treatment.at(i) == 1) {
					m++;
				}
			}
			unsigned int n = treatment.size() - m;
      std::vector<double> cases(m);
			std::vector<double> controls(n);
			m = 0;
			n = 0;
			for (unsigned int i = 0; i < treatment.size(); i++) {
				if (treatment.at(i) == 1) {
					cases[m++] = propensityScores.at(i);
				} else {
					controls[n++] = propensityScores.at(i);
				}
			}
			double mean = 0;
			for (unsigned int i = 0; i < m; i++) {
        double localMean = 0;
				for (unsigned int j = 0; j < n; j++) {
					double mw = mannWhitneyKernel(cases.at(i), controls.at(j));
					localMean += mw;
				}
        mean += localMean / n;
			}
			mean /= m;
			return mean;
		}

		std::vector<double> Auc::aucWithCi(const std::vector<double> &propensityScores, const std::vector<int> &treatment) {
			unsigned int m = 0;
			for (unsigned int i = 0; i < treatment.size(); i++) {
				if (treatment.at(i) == 1) {
					m++;
				}
			}
			unsigned int n = treatment.size() - m;
			std::vector<double> cases(m);
			std::vector<double> controls(n);
			m = 0;
			n = 0;
			for (unsigned int i = 0; i < treatment.size(); i++) {
				if (treatment.at(i) == 1) {
					cases[m++] = propensityScores.at(i);
				} else {
					controls[n++] = propensityScores.at(i);
				}
			}

      double mean = 0;
			for (unsigned int i = 0; i < m; i++) {
        double localMean = 0;
				for (unsigned int j = 0; j < n; j++) {
					double mw = mannWhitneyKernel(cases.at(i), controls.at(j));
					localMean += mw;
				}
        mean += localMean / n;
			}
			mean /= m;
			double vr10[m];
			for (unsigned int i = 0; i < m; i++) {
				double sum = 0;
				for (unsigned int j = 0; j < n; j++) {
          sum += mannWhitneyKernel(cases.at(i), controls.at(j));
				}
				vr10[i] = sum / n;
			}
      
			double vr01[n];
			for (unsigned int i = 0; i < n; i++) {
				double sum = 0;
				for (unsigned int j = 0; j < m; j++) {
          sum += mannWhitneyKernel(cases.at(j), controls.at(i));
				}
					
				vr01[i] = sum / m;
			}

      double s10 = 0;
			for (unsigned int i = 0; i < m; i++) {
				s10 += (vr10[i] - mean) * (vr10[i] - mean);
			}
			s10 /= (double) (m - 1);

			double s01 = 0;
			for (unsigned int i = 0; i < n; i++) {
				s01 += (vr01[i] - mean) * (vr01[i] - mean);
			}
			s01 /= (double) (n - 1);

			double s = s10 / m + s01 / n;
			double sd = sqrt(s);
			std::vector<double> ci;
			ci.push_back(mean);
			ci.push_back(mean - (1.96 * sd));
			ci.push_back(mean + (1.96 * sd));
			return ci;
		}
	}
}

#endif // __Auc_cpp__
