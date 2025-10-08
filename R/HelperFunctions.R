# @file HelperFunctions.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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
removeInvalidString <- function(string) {
  modString <- gsub("_", " ", string)
  modString <- gsub("\\.", " ", modString)
  modString <- gsub("[[:punct:]]", "", modString)
  modString <- gsub(" ", "_", modString)
  return(modString)
}

#' Check if the required packages for survival analysis are installed
#' @keywords internal
#' @noRd
checkSurvivalPackages <- function() {
  rlang::check_installed(c("survival", "polspline"),
    reason = "Please install the required packages for survival analysis"
  )
}

checkSklearn <- function() {
  rlang::check_installed(
    "reticulate",
    reason = "Reticulate is required to use the Python models"
  )
  reticulate::py_require("sklearn")
  tryCatch({
    reticulate::import("sklearn")
  }, error = function(e) {
    stop("scikit-learn in a python environment reachable by reticulate is required to use the Python models")
  })
}

#' Create a temporary model location
#' @return A string for the location of the temporary model location
#' @examples
#' modelLoc <- createTempModelLoc()
#' dir.exists(modelLoc)
#' # clean up
#' unlink(modelLoc, recursive = TRUE)
#' @export
createTempModelLoc <- function() {
  repeat {
    loc <- file.path(tempdir(), paste0("python_models_", sample(10002323, 1)))
    if (!dir.exists(loc)) {
      return(loc)
    }
  }
}

#' join two lists
#'
#' @details
#' This function joins two lists
#' @param a   A list
#' @param b   Another list
#' @return the joined list
#' @examples
#' a <- list(a = 1, b = 2)
#' b <- list(c = 3, d = 4)
#' listAppend(a, b)
#'
#' @export
listAppend <- function(a, b) {
  size <- length(a) + length(b)
  x <- list()
  length(x) <- size
  for (i in 1:size) {
    if (i <= length(a)) {
      x[[i]] <- a[[i]]
    } else {
      x[[i]] <- b[[i - length(a)]]
    }
  }
  names(x) <- c(names(a), names(b))
  return(x)
}


#' Sets up a python environment to use for PLP (can be conda or venv)
#'
#' @details
#' This function creates a python environment that can be used by PatientLevelPrediction
#' and installs all the required package dependancies. 
#'
#' @param envname   A string for the name of the virtual environment (default is 'PLP')
#' @param envtype   An option for specifying the environment as'conda' or 'python'.  If NULL then the default is 'conda' for windows users and 'python' for non-windows users
#' @param condaPythonVersion String, Python version to use when creating a conda environment
#' @return location of the created conda or virtual python environment
#' @examples
#' \dontshow{ # dontrun reason: don't modify environment in examples }
#' \dontrun{ 
#'  configurePython(envname="PLP", envtype="conda")
#' }
#' @export
configurePython <- function(envname = "PLP", envtype = NULL, condaPythonVersion = "3.11") {
  if (is.null(envtype)) {
    if (getOs() == "windows") {
      envtype <- "conda"
    } else {
      envtype <- "python"
    }
  }

  if (envtype == "conda") {
    pEnvironments <- reticulate::conda_list()
    if (length(pEnvironments) > 0 && envname %in% pEnvironments$name) {
      location <- ""
      warning(paste0("Conda environment ", envname, " exists.  You can use reticulate::conda_remove() to remove if you want to fresh config"))
    } else {
      ParallelLogger::logInfo(paste0("Creating virtual conda environment called ", envname))
      location <- reticulate::conda_create(envname = envname, packages = paste0("python==", condaPythonVersion), conda = "auto")
    }
    packages <- c("numpy", "scipy", "scikit-learn", "pandas", "pydotplus", "joblib")
    ParallelLogger::logInfo(paste0("Adding python dependancies to ", envname))
    reticulate::conda_install(
      envname = envname, packages = packages, forge = TRUE, pip = FALSE,
      pip_ignore_installed = TRUE, conda = "auto"
    )
  } else {
    pEnvironments <- reticulate::virtualenv_list()
    if (length(pEnvironments) > 0 && envname %in% pEnvironments) {
      location <- ""
      warning(paste0("Python environment ", envname, " exists."))
    } else {
      ParallelLogger::logInfo(paste0("Creating virtual python environment called ", envname))
      location <- reticulate::virtualenv_create(envname = envname)
    }
    packages <- c("numpy", "scikit-learn", "scipy", "pandas", "pydotplus")
    ParallelLogger::logInfo(paste0("Adding python dependancies to ", envname))
    reticulate::virtualenv_install(
      envname = envname, packages = packages,
      ignore_installed = TRUE
    )
  }

  return(invisible(location))
}

#' Use the python environment created using configurePython()
#'
#' @details
#' This function sets PatientLevelPrediction to use a python environment
#'
#' @param envname   A string for the name of the virtual environment (default is 'PLP')
#' @param envtype   An option for specifying the environment as'conda' or 'python'.  If NULL then the default is 'conda' for windows users and 'python' for non-windows users
#'
#' @return A string indicating the which python environment will be used
#' @examples
#' \dontshow{ # dontrun reason: don't modify environment in examples }
#' \dontrun{ #' # create a conda environment named PLP
#' configurePython(envname="PLP", envtype="conda")
#' }
#' @export
setPythonEnvironment <- function(envname = "PLP", envtype = NULL) {
  if (is.null(envtype)) {
    if (getOs() == "windows") {
      envtype == "conda"
    } else {
      envtype == "python"
    }
  }

  if (envtype == "conda") {
    pEnvironments <- reticulate::conda_list()
    if (!envname %in% pEnvironments$name) {
      return(paste0("Conda environment ", envname, " not found.  Please set up using configurePython()"))
    }
    reticulate::use_condaenv(envname)
    return(paste0("Using conda environment ", envname))
  } else {
    pEnvironments <- reticulate::virtualenv_list()
    if (!envname %in% pEnvironments$name) {
      return(paste0("Python environment ", envname, " not found.  Please set up using configurePython()"))
    }
    reticulate::use_virtualenv(envname)
    return(paste0("Using python environment ", envname))
  }
}

getOs <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
}


# Borrowed and adapted from Hmisc: https://github.com/harrelfe/Hmisc/blob/39011dae3af3c943e67401ed6000644014707e8b/R/cut2.s
cut2 <- function(x, g, m = 150, digits = 3) {
  method <- 1 ## 20may02
  x.unique <- sort(unique(c(x[!is.na(x)])))
  min.dif <- min(diff(x.unique)) / 2
  min.dif.factor <- 1

  oldopt <- options("digits")
  options(digits = digits)
  on.exit(options(oldopt))

  xlab <- attr(x, "label")

  nnm <- sum(!is.na(x))
  if (missing(g)) g <- max(1, floor(nnm / m))
  if (g < 1) {
    stop("g must be >=1, m must be positive")
  }

  options(digits = 15)
  n <- table(x)
  xx <- as.double(names(n))
  options(digits = digits)
  cum <- cumsum(n)
  m <- length(xx)

  y <- as.integer(ifelse(is.na(x), NA, 1))
  labs <- character(g)
  cuts <- stats::approx(cum, xx,
    xout = (1:g) * nnm / g,
    method = "constant", rule = 2, f = 1
  )$y
  cuts[length(cuts)] <- max(xx)
  lower <- xx[1]
  upper <- 1e45
  up <- low <- double(g)
  i <- 0
  for (j in 1:g) {
    cj <- if (method == 1 || j == 1) {
      cuts[j]
    } else {
      if (i == 0) {
        stop("program logic error")
      }
      # Not used unique values found in table(x)
      s <- if (is.na(lower)) FALSE else xx >= lower
      cum.used <- if (all(s)) 0 else max(cum[!s])
      if (j == m) {
        max(xx)
      } else if (sum(s) < 2) {
        max(xx)
      } else {
        stats::approx(cum[s] - cum.used, xx[s],
          xout = (nnm - cum.used) / (g - j + 1),
          method = "constant", rule = 2, f = 1
        )$y
      }
    }

    if (cj == upper) next

    i <- i + 1
    upper <- cj
    # assign elements to group i
    # y contains the group number in the end
    y[x >= (lower - min.dif.factor * min.dif)] <- i
    low[i] <- lower
    lower <- if (j == g) upper else min(xx[xx > upper])

    if (is.na(lower)) lower <- upper

    up[i] <- lower
  }

  low <- low[1:i]
  up <- up[1:i]
  # Are the bounds different?
  variation <- logical(i)
  for (ii in 1:i) {
    r <- range(x[y == ii], na.rm = TRUE)
    variation[ii] <- diff(r) > 0
  }
  flow <- do.call(format, c(list(low), digits = 3))
  fup <- do.call(format, c(list(up), digits = 3))
  bb <- c(rep(")", i - 1), "]")
  labs <- ifelse(low == up | (!variation), flow,
    paste("[", flow, ",", fup, bb, sep = "")
  )
  ss <- y == 0 & !is.na(y)
  if (any(ss)) {
    stop(paste(
      "categorization error in cut2.  Values of x not appearing in any interval:\n",
      paste(format(x[ss], digits = 12), collapse = " "),
      "\nLower endpoints:",
      paste(format(low, digits = 12), collapse = " "),
      "\nUpper endpoints:",
      paste(format(up, digits = 12), collapse = " ")
    ))
  }

  y <- structure(y, class = "factor", levels = labs)

  attr(y, "class") <- "factor"
  if (length(xlab)) {
    class(y) <- "labelled"
    attr(y, "label") <- xlab
  }

  return(y)
}
