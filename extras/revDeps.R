options(crayon.enabled = TRUE)
dir.create("revdep/results", recursive = TRUE, showWarnings = FALSE)

readRepoList <- function() {
  x <- Sys.getenv("INPUT_REPOS", "")
  if (nzchar(x)) {
    cat("[config] Taking repo list from workflow input\n")
    repos <- strsplit(x, "\n", fixed = TRUE)[[1]]
  } else if (file.exists("extras/github.txt")) {
    cat("[config] Taking repo list from revdep/github.txt\n")
    repos <- readLines("extras/github.txt", warn = FALSE)
  } else {
    stop(
      "No reverse-dep list found. Provide workflow input or revdep/github.txt"
    )
  }
  repos <- trimws(repos)
  repos <- repos[nzchar(repos) & !startsWith(repos, "#")]
  if (!length(repos)) {
    stop("Reverse-dep list is empty.")
  }
  unique(repos)
}

safe <- function(expr, default = NULL) {
  tryCatch(expr, error = function(e) {
    message("! ", conditionMessage(e))
    default
  })
}

runOneRepo <- function(ownerRepo, timeoutMin = 90L) {
  start <- Sys.time()
  outDir <- file.path(
    "revdep",
    "results",
    gsub("/", "_", ownerRepo, fixed = TRUE)
  )
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

  message("\n==> [", ownerRepo, "] starting at ", format(start), " UTC\n")
  tmp <- tempfile("revdep_repo_")
  dir.create(tmp)

  repoUrl <- paste0("https://github.com/", ownerRepo, ".git")
  message("[", ownerRepo, "] cloning ", repoUrl, "\n")
  ok <- safe(
    system2(
      "git",
      c("clone", "--depth", "1", repoUrl, tmp),
      stdout = TRUE,
      stderr = TRUE
    ),
    ""
  )
  if (!dir.exists(file.path(tmp, ".git"))) {
    writeLines(as.character(ok), file.path(outDir, "clone.log"))
    stop(sprintf("[%s] clone failed", ownerRepo))
  }
  writeLines(as.character(ok), file.path(outDir, "clone.log"))

  descPath <- file.path(tmp, "DESCRIPTION")
  if (!file.exists(descPath)) {
    stop(sprintf("[%s] DESCRIPTION not found", ownerRepo))
  }
  pkg <- safe(
    as.character(read.dcf(descPath, fields = "Package")[1]),
    NA_character_
  )
  if (is.na(pkg)) {
    stop(sprintf("[%s] Could not read Package field", ownerRepo))
  }
  message("[", ownerRepo, "] package: ", pkg, "\n")

  # Install dependencies + package into the current library to speed checks
  message(
    "[",
    ownerRepo,
    "] installing dependencies via pak (this may take a while)\n"
  )
  installLog <- safe(
    pak::pkg_install(
      paste0("local::", tmp),
      dependencies = TRUE,
      upgrade = FALSE
    ),
    default = NULL
  )
  capture.output(print(installLog), file = file.path(outDir, "install.log"))

  message("[", ownerRepo, "] running R CMD check\n")
  checkDir <- file.path(outDir, "check")
  dir.create(checkDir, showWarnings = FALSE)

  res <- safe(
    rcmdcheck::rcmdcheck(
      path = tmp,
      args = c("--no-manual", "--as-cran"),
      build_args = c("--no-manual", "--no-build-vignettes"),
      error_on = "never",
      check_dir = checkDir,
      quiet = FALSE,
      libpath = .libPaths(),
      timeout = as.numeric(timeoutMin) * 60
    ),
    default = NULL
  )

  if (!is.null(res)) {
    sum <- list(
      owner_repo = ownerRepo,
      package = pkg,
      duration_sec = as.numeric(difftime(Sys.time(), start, units = "secs")),
      errors = length(res$errors),
      warnings = length(res$warnings),
      notes = length(res$notes)
    )
    json <- jsonlite::toJSON(sum, pretty = TRUE, auto_unbox = TRUE)
    writeLines(json, file.path(outDir, "summary.json"))

    # copy log files if present
    if (length(res$logfile) && file.exists(res$logfile)) {
      file.copy(res$logfile, file.path(outDir, "00check.log"), overwrite = TRUE)
    }
    if (length(res$install_out) && file.exists(res$install_out)) {
      file.copy(
        res$install_out,
        file.path(outDir, "00install.out"),
        overwrite = TRUE
      )
    }
    message(
      "[",
      ownerRepo,
      "] check complete: E=",
      length(res$errors),
      " W=",
      length(res$warnings),
      " N=",
      length(res$notes),
      "\n"
    )
  } else {
    sum <- list(
      owner_repo = ownerRepo,
      package = pkg,
      duration_sec = as.numeric(difftime(Sys.time(), start, units = "secs")),
      errors = NA_integer_,
      warnings = NA_integer_,
      notes = NA_integer_,
      status = "rcmdcheck_failed"
    )
    writeLines(
      jsonlite::toJSON(sum, pretty = TRUE, auto_unbox = TRUE),
      file.path(outDir, "summary.json")
    )
    message("[", ownerRepo, "] check failed (see logs)\n")
  }

  invisible(TRUE)
}

main <- function() {
  repos <- readRepoList()
  message(
    "[config] Repos to check (",
    length(repos),
    "): ",
    paste(repos, collapse = ", "),
    "\n"
  )

  failures <- 0L
  for (r in repos) {
    ok <- TRUE
    tryCatch(runOneRepo(r), error = function(e) {
      message("! ", conditionMessage(e))
      ok <<- FALSE
    })
    if (!ok) failures <- failures + 1L
  }

  message("\n[summary] completed with ", failures, " failures\n")
  if (failures > 0L) quit(status = 1L) else invisible(TRUE)
}

# Ensure needed packages are present
for (p in c("pak", "rcmdcheck", "jsonlite")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
main()
