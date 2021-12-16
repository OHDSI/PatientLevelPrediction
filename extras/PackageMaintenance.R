# @file PackageMaintenance
#
# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# recreate the html index when new documentation
pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

# Format and check code
OhdsiRTools::formatRFolder()
OhdsiRTools::checkUsagePackage("PatientLevelPrediction")
OhdsiRTools::updateCopyrightYearFolder()
devtools::spell_check()

# Create manual and vignettes
unlink("extras/PatientLevelPrediction.pdf")
system("R CMD Rd2pdf ./ --output=extras/PatientLevelPrediction.pdf")

rmarkdown::render("vignettes/BuildingPredictiveModels.Rmd",
                  output_file = "../inst/doc/BuildingPredictiveModels.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE, toc_depth = 3,
                                          number_sections = TRUE))

rmarkdown::render("vignettes/BuildingMultiplePredictiveModels.Rmd",
                  output_file = "../inst/doc/BuildingMultiplePredictiveModels.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

rmarkdown::render("vignettes/CreatingNetworkstudies.Rmd",
                  output_file = "../inst/doc/CreatingNetworkstudies.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

rmarkdown::render("vignettes/AddingCustomModels.Rmd",
                  output_file = "../inst/doc/AddingCustomModels.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

rmarkdown::render(
  "vignettes/AddingCustomSamples.Rmd",
  output_file = "../inst/doc/AddingCustomSamples.pdf",
  rmarkdown::pdf_document(latex_engine = "pdflatex",
    toc = TRUE,
    number_sections = TRUE)
)

rmarkdown::render(
  "vignettes/AddingCustomSplitting.Rmd",
  output_file = "../inst/doc/AddingCustomSplitting.pdf",
  rmarkdown::pdf_document(latex_engine = "pdflatex",
    toc = TRUE,
    number_sections = TRUE)
)

rmarkdown::render(
  "vignettes/AddingCustomFeatureEngineering.Rmd",
  output_file = "../inst/doc/AddingCustomFeatureEngineering.pdf",
  rmarkdown::pdf_document(latex_engine = "pdflatex",
    toc = TRUE,
    number_sections = TRUE)
)

rmarkdown::render("vignettes/InstallationGuide.Rmd",
                  output_file = "../inst/doc/InstallationGuide.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

rmarkdown::render("vignettes/BuildingEnsembleModels.Rmd",
                  output_file = "../inst/doc/BuildingEnsembleModels.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))


rmarkdown::render("vignettes/CreatingLearningCurves.Rmd",
                  output_file = "../inst/doc/CreatingLearningCurves.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

rmarkdown::render("vignettes/CreatingShinyApp.Rmd",
                  output_file = "../inst/doc/CreatingShinyApp.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

