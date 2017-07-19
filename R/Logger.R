# @file Logger.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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

# Add explicit bindings. 
# Could maybe better be done using importFrom (to be decided)
# MAS: Highly recommend using importFrom (in case futile.logger changes its API)

INFO  <- futile.logger::INFO
TRACE <- futile.logger::TRACE
DEBUG <- futile.logger::DEBUG
ERROR <- futile.logger::ERROR
FATAL <- futile.logger::FATAL

flog.info     = futile.logger::flog.info
flog.trace    = futile.logger::flog.trace
flog.debug    = futile.logger::flog.debug
flog.warn     = futile.logger::flog.warn
flog.error    = futile.logger::flog.error
flog.fatal    = futile.logger::flog.fatal
flog.appender = futile.logger::flog.appender
flog.layout   = futile.logger::flog.layout
flog.threshold= futile.logger::flog.threshold
layout.format = futile.logger::layout.format
appender.tee  = futile.logger::appender.tee
ftry          = futile.logger::ftry

flog.seperator <- function(name='ROOT') {
    msg <- '************************************************************************************************'
    layout_org <- flog.layout(name=name)
    layout <- layout.format('~m')
    temp <- flog.layout(layout)
    flog.fatal(msg = msg, name=name)  
    temp <- flog.layout(layout_org,name)
}

flog.emptyLine <- function(name='ROOT') {
  msg <- '\n'
  layout_org <- flog.layout(name=name)
  layout <- layout.format('~m')
  flog.layout(layout)
  flog.fatal(msg = msg, name=name)  
  flog.layout(layout_org,name)
}
