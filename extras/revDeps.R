options(crayon.enabled = TRUE)
dir.create("revdep/results", recursive = TRUE, showWarnings = FALSE)

# Parse inputs: lines can be "owner/repo" or "owner/repo filter_regex"
readRepoList <- function() {
  x <- Sys.getenv("INPUT_REPOS", "")
  if (nzchar(x)) {
    cat("[config] Taking repo list from workflow input\n")
    lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  } else if (file.exists("extras/revDeps.txt")) {
    cat("[config] Taking repo list from extras/revDeps.txt\n")
    lines <- readLines("extras/revDeps.txt", warn = FALSE)
  } else {
    stop(
      "No reverse-dep list found. Provide workflow input or extras/revDeps.txt"
    )
  }
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (!length(lines)) {
    stop("Reverse-dep list is empty.")
  }

  # Parse lines into a list of list(repo, filter)
  configs <- lapply(lines, function(line) {
    parts <- strsplit(line, "\\s+")[[1]]
    list(
      repo = parts[1],
      filter = if (length(parts) > 1) parts[2] else NULL
    )
  })

  configs
}

safe <- function(expr, default = NULL) {
  tryCatch(expr, error = function(e) {
    message("! ", conditionMessage(e))
    default
  })
}

runOneRepo <- function(config, timeoutMin = 60L) {
  ownerRepo <- config$repo
  filterArg <- config$filter

  start <- Sys.time()

  # Prepare output directory
  outDir <- file.path(
    "revdep",
    "results",
    gsub("/", "_", ownerRepo, fixed = TRUE)
  )
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

  message("\n==> [", ownerRepo, "] starting at ", format(start), " UTC")
  if (!is.null(filterArg)) {
    message("    Filter: '", filterArg, "'")
  }
  message("")

  # Clone
  tmp <- tempfile("revdep_repo_")
  dir.create(tmp)
  repoUrl <- paste0("https://github.com/", ownerRepo, ".git")

  message("[", ownerRepo, "] cloning ", repoUrl)
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
    message(sprintf("[%s] clone failed", ownerRepo))
    return(FALSE)
  }
  writeLines(as.character(ok), file.path(outDir, "clone.log"))

  # Read Package Name
  descPath <- file.path(tmp, "DESCRIPTION")
  if (!file.exists(descPath)) {
    message(sprintf("[%s] DESCRIPTION not found", ownerRepo))
    return(FALSE)
  }

  pkg <- safe(
    as.character(read.dcf(descPath, fields = "Package")[1]),
    NA_character_
  )
  if (is.na(pkg)) {
    message(sprintf("[%s] Could not read Package field", ownerRepo))
    return(FALSE)
  }

  message("[", ownerRepo, "] package: ", pkg)

  # Install dependencies AND the package itself (needed for testthat to run against it)
  message("[", ownerRepo, "] installing dependencies via pak")

  installLog <- safe(
    pak::pkg_install(
      paste0("local::", tmp),
      dependencies = TRUE,
      upgrade = FALSE
    ),
    default = NULL
  )
  capture.output(print(installLog), file = file.path(outDir, "install.log"))
  
  if (is.null(installLog)) {
    message(sprintf(
      "! [%s] installation failed (dependencies or package)",
      ownerRepo
    ))
    return(FALSE)
  }

  # Run Tests
  message(
    "[",
    ownerRepo,
    "] running tests",
    if (!is.null(filterArg)) paste0(" (filter: ", filterArg, ")") else ""
  )

  testLogPath <- file.path(outDir, "tests.log")

  # We use capture.output to save the test console output,
  # but we also assign the result to 'res' to analyze pass/fail programmatically.
  res <- safe(
    {
      # Using callr or managing output directly to capture logs
      sink(testLogPath, split = TRUE)
      on.exit(sink(), add = TRUE)

      testthat::test_local(
        path = tmp,
        filter = if (is.null(filterArg)) NULL else filterArg,
        stop_on_failure = FALSE,
        reporter = "summary" # or 'progress', 'location'
      )
    },
    default = NULL
  )

  if (!is.null(res)) {
    df <- as.data.frame(res)

    nFail <- sum(df$failed, na.rm = TRUE)
    nError <- sum(df$error, na.rm = TRUE) 
    nSkip <- sum(df$skipped, na.rm = TRUE)
    nWarn <- sum(df$warning, na.rm = TRUE)

    sum <- list(
      owner_repo = ownerRepo,
      package = pkg,
      filter = filterArg,
      duration_sec = as.numeric(difftime(Sys.time(), start, units = "secs")),
      test_failures = nFail,
      test_errors = nError,
      test_warnings = nWarn,
      test_skipped = nSkip,
      status = if (nFail > 0 || nError > 0) "failure" else "success"
    )

    json <- jsonlite::toJSON(sum, pretty = TRUE, auto_unbox = TRUE)
    writeLines(json, file.path(outDir, "summary.json"))

    message(
      "[",
      ownerRepo,
      "] complete: Fail=",
      nFail,
      " Err=",
      nError,
      " Warn=",
      nWarn,
      " Skip=",
      nSkip,
      "\n"
    )

    # Return FALSE if there were failures or errors
    invisible(nFail == 0 && nError == 0)
  } else {
    # If res is NULL, the test execution crashed entirely
    sum <- list(
      owner_repo = ownerRepo,
      package = pkg,
      duration_sec = as.numeric(difftime(Sys.time(), start, units = "secs")),
      status = "test_execution_crash"
    )
    writeLines(
      jsonlite::toJSON(sum, pretty = TRUE, auto_unbox = TRUE),
      file.path(outDir, "summary.json")
    )
    message("[", ownerRepo, "] test execution CRASHED (see tests.log)\n")
    invisible(FALSE)
  }
}

main <- function() {
  configs <- readRepoList()
  message("[config] Checking ", length(configs), " repositories\n")

  failures <- 0L
  for (config in configs) {
    ok <- TRUE
    tryCatch(
      {
        # runOneRepo returns TRUE if tests passed, FALSE otherwise
        success <- runOneRepo(config)
        if (!success) ok <- FALSE
      },
      error = function(e) {
        message("! Unhandled error in runOneRepo: ", conditionMessage(e))
        ok <<- FALSE
      }
    )

    if (!ok) failures <- failures + 1L
  }

  message("\n[summary] completed with ", failures, " failures\n")
  if (failures > 0L) quit(status = 1L) else invisible(TRUE)
}

# Ensure needed packages are present
for (p in c("pak", "testthat", "jsonlite")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
main()
