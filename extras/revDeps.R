options(crayon.enabled = TRUE)
dir.create("revdep/results", recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

getCurrentPackageName <- function() {
  if (file.exists("DESCRIPTION")) {
    name <- trimws(read.dcf("DESCRIPTION", fields = "Package")[1])
    message(
      "[config] Current package is: ",
      name,
      " (will be protected from updates)"
    )
    name
  } else {
    stop(
      "Could not find DESCRIPTION file in root. Ensure you are running from package root."
    )
  }
}

readRepoList <- function() {
  x <- Sys.getenv("INPUT_REPOS", "")
  if (nzchar(x)) {
    message("[config] Taking repo list from workflow input\n")
    lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  } else if (file.exists("extras/revDeps.txt")) {
    message("[config] Taking repo list from extras/revDeps.txt\n")
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

  configs <- lapply(lines, function(line) {
    parts <- strsplit(line, "\\s+")[[1]]
    filter <- NULL
    refMode <- NULL

    if (length(parts) > 1) {
      second <- tolower(parts[2])
      if (second %in% c("head", "release", "both")) {
        refMode <- second
      } else {
        filter <- parts[2]
        if (length(parts) > 2) {
          refMode <- tolower(parts[3])
        }
      }
    }

    refMode <- refMode %||% "head"
    if (!refMode %in% c("head", "release", "both")) {
      stop(sprintf(
        "Unsupported ref_mode '%s' in reverse dependency config line: %s",
        refMode,
        line
      ))
    }

    list(repo = parts[1], filter = filter, refMode = refMode)
  })

  expanded <- lapply(configs, function(config) {
    if (identical(config$refMode, "both")) {
      list(
        list(repo = config$repo, filter = config$filter, refMode = "release"),
        list(repo = config$repo, filter = config$filter, refMode = "head")
      )
    } else {
      list(config)
    }
  })

  unlist(expanded, recursive = FALSE)
}

safe <- function(expr, default = NULL) {
  tryCatch(expr, error = function(e) {
    message("! ", conditionMessage(e))
    default
  })
}

resolveLatestReleaseTag <- function(repoUrl) {
  lines <- safe(
    system2(
      "git",
      c("ls-remote", "--tags", "--refs", repoUrl),
      stdout = TRUE,
      stderr = TRUE
    ),
    default = character()
  )

  if (!length(lines)) {
    stop(sprintf("Unable to resolve tags for %s", repoUrl))
  }

  tags <- sub(".*refs/tags/", "", lines)
  stableTags <- tags[grepl("^v?[0-9]+(\\.[0-9]+)*$", tags)]

  if (!length(stableTags)) {
    stop(sprintf("No semver-like release tags found for %s", repoUrl))
  }

  versions <- numeric_version(sub("^v", "", stableTags))
  stableTags[[order(versions, decreasing = TRUE)[1]]]
}

cloneRepo <- function(repoUrl, destination, refMode) {
  refMode <- refMode %||% "head"

  if (identical(refMode, "release")) {
    resolvedRef <- resolveLatestReleaseTag(repoUrl)
    cloneOutput <- safe(
      system2(
        "git",
        c("clone", "--depth", "1", "--branch", resolvedRef, repoUrl, destination),
        stdout = TRUE,
        stderr = TRUE
      ),
      default = character()
    )
  } else {
    resolvedRef <- "HEAD"
    cloneOutput <- safe(
      system2(
        "git",
        c("clone", "--depth", "1", repoUrl, destination),
        stdout = TRUE,
        stderr = TRUE
      ),
      default = character()
    )
  }

  checkedOutCommit <- safe(
    system2("git", c("-C", destination, "rev-parse", "--short", "HEAD"), stdout = TRUE),
    default = NA_character_
  )
  checkedOutCommit <- checkedOutCommit[[1]] %||% NA_character_

  list(
    output = cloneOutput,
    resolvedRef = resolvedRef,
    checkedOutCommit = checkedOutCommit
  )
}

runTestsIsolated <- function(pkgPath, filterArg, testLogPath, countsJsonPath) {
  # Run tests in a fresh R session to avoid cross-repo contamination
  # (e.g., packages left attached on the search path).
  filterArg <- filterArg %||% ""

  runner <- tempfile("revdep_run_tests_", fileext = ".R")
  writeLines(
    c(
      "args <- commandArgs(trailingOnly = TRUE)",
      "if (length(args) < 3) stop('Expected 3 args: <pkgPath> <countsPath> <filterArg>')",
      "pkgPath <- args[[1]]",
      "countsPath <- args[[2]]",
      "filterArg <- args[[3]]",
      "",
      "if (!requireNamespace('testthat', quietly = TRUE)) stop('testthat not installed')",
      "if (!requireNamespace('jsonlite', quietly = TRUE)) stop('jsonlite not installed')",
      "",
      "res <- testthat::test_local(",
      "  path = pkgPath,",
      "  filter = if (nzchar(filterArg)) filterArg else NULL,",
      "  stop_on_failure = FALSE,",
      "  reporter = 'summary'",
      ")",
      "",
      "df <- as.data.frame(res)",
      "counts <- list(",
      "  fail = sum(df$failed, na.rm = TRUE),",
      "  err = sum(df$error, na.rm = TRUE),",
      "  warn = sum(df$warning, na.rm = TRUE),",
      "  skip = sum(df$skipped, na.rm = TRUE)",
      ")",
      "jsonlite::write_json(counts, countsPath, pretty = TRUE, auto_unbox = TRUE)",
      "invisible(counts)"
    ),
    runner
  )
  on.exit(unlink(runner), add = TRUE)

  args <- shQuote(
    c("--vanilla", runner, pkgPath, countsJsonPath, filterArg),
    type = "sh"
  )
  system2(
    "Rscript",
    args,
    stdout = testLogPath,
    stderr = testLogPath
  )
}

stripAnsi <- function(text) {
  gsub("\033\\[[0-9;]*m", "", text)
}

printSummary <- function(results) {
  if (length(results) == 0) {
    return()
  }

  message(
    "\n========================================================================="
  )
  message(sprintf(
    "%-30s | %-12s | %-14s | %-15s | %4s | %4s | %4s",
    "Repository",
    "Ref Mode",
    "Resolved Ref",
    "Status",
    "Fail",
    "Err",
    "Time"
  ))
  message(
    "-------------------------------------------------------------------------"
  )

  for (r in results) {
    statusStr <- r$status
    if (r$fail > 0 || r$err > 0 || !r$status %in% c("SUCCESS")) {
      statusStr <- paste0("!! ", statusStr)
    }
    message(sprintf(
      "%-30s | %-12s | %-14s | %-15s | %4d | %4d | %4.0fs",
      substring(r$repo, 1, 30),
      substring(r$refMode %||% "", 1, 12),
      substring(r$resolvedRef %||% "", 1, 14),
      statusStr,
      r$fail,
      r$err,
      r$time
    ))
  }
  message(
    "=========================================================================\n"
  )

  ghSummaryFile <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (nzchar(ghSummaryFile)) {
    sink(ghSummaryFile, append = TRUE)
    cat("## Reverse Dependency Results\n\n")
    cat("| Repository | Ref Mode | Resolved Ref | Status | Fail | Error | Warn | Skip | Duration |\n")
    cat("| :--- | :--- | :--- | :--- | :---: | :---: | :---: | :---: | ---: |\n")

    for (r in results) {
      icon <- if (r$status == "SUCCESS") "✅" else "❌"
      statusFmt <- if (r$status == "SUCCESS") {
        "SUCCESS"
      } else {
        paste0("**", r$status, "**")
      }

      cat(sprintf(
        "| %s | %s | %s | %s %s | %d | %d | %d | %d | %.0fs |\n",
        r$repo,
        r$refMode %||% "",
        r$resolvedRef %||% "",
        icon,
        statusFmt,
        r$fail,
        r$err,
        r$warn,
        r$skip,
        r$time
      ))
    }
    cat("\n")

    failures <- Filter(function(x) x$status != "SUCCESS", results)

    if (length(failures) > 0) {
      cat("### 🔍 Failure Details\n\n")

      for (f in failures) {
        safeName <- gsub("/", "_", paste(f$repo, f$refMode, sep = "__"), fixed = TRUE)
        # Try finding the most relevant log: tests > install > clone
        logFiles <- file.path(
          "revdep",
          "results",
          safeName,
          c("tests.log", "install.log", "clone.log")
        )
        logFile <- logFiles[file.exists(logFiles)][1]

        logContent <- "No log file found."
        if (!is.na(logFile)) {
          lines <- readLines(logFile, warn = FALSE)
          lines <- stripAnsi(lines)
          # Truncate to last 300 lines to avoid size limits
          if (length(lines) > 300) {
            lines <- c(
              paste0(
                "... [Truncated: showing last 300 of ",
                length(lines),
                " lines] ..."
              ),
              tail(lines, 300)
            )
          }
          logContent <- paste(lines, collapse = "\n")
        }

        # Collapsible HTML details tag
        cat(sprintf(
          "<details><summary><strong>%s</strong> (Click to expand logs)</summary>\n\n",
          f$repo
        ))
        cat("```text\n")
        cat(logContent)
        cat("\n```\n")
        cat("</details>\n\n")
      }
    }

    sink()
  }
}

runOneRepo <- function(config, currentPkgName, timeoutMin = 60L) {
  ownerRepo <- config$repo
  filterArg <- config$filter
  refMode <- config$refMode %||% "head"
  resultKey <- paste(ownerRepo, refMode, sep = "__")
  start <- Sys.time()

  resolvedRef <- NA_character_

  mkResult <- function(status, fail = 0, err = 0, warn = 0, skip = 0) {
    list(
      repo = ownerRepo,
      refMode = refMode,
      resolvedRef = resolvedRef,
      status = status,
      fail = fail,
      err = err,
      warn = warn,
      skip = skip,
      time = as.numeric(difftime(Sys.time(), start, units = "secs"))
    )
  }

  outDir <- file.path(
    "revdep",
    "results",
    gsub("/", "_", resultKey, fixed = TRUE)
  )
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

  message("\n==> [", ownerRepo, "] starting at ", format(start), " UTC")
  if (!is.null(filterArg)) {
    message("    Filter: '", filterArg, "'")
  }
  message("    Ref mode: '", refMode, "'")
  message("")

  tmp <- tempfile("revdep_repo_")
  dir.create(tmp)
  repoUrl <- paste0("https://github.com/", ownerRepo, ".git")

  message("[", ownerRepo, "] cloning ", repoUrl)
  cloneInfo <- safe(cloneRepo(repoUrl, tmp, refMode), default = NULL)
  ok <- if (!is.null(cloneInfo)) cloneInfo$output else character()
  if (!is.null(cloneInfo)) {
    resolvedRef <- if (identical(refMode, "release")) {
      paste0(cloneInfo$resolvedRef, "@", cloneInfo$checkedOutCommit)
    } else {
      cloneInfo$checkedOutCommit
    }
    message("[", ownerRepo, "] resolved ref: ", resolvedRef)
  }

  if (!dir.exists(file.path(tmp, ".git"))) {
    writeLines(as.character(ok), file.path(outDir, "clone.log"))
    message(sprintf("! [%s] clone failed", ownerRepo))
    return(mkResult("CLONE_FAILED"))
  }
  writeLines(as.character(ok), file.path(outDir, "clone.log"))

  descPath <- file.path(tmp, "DESCRIPTION")
  if (!file.exists(descPath)) {
    message(sprintf("! [%s] DESCRIPTION not found", ownerRepo))
    return(mkResult("NO_DESC"))
  }

  pkg <- safe(
    as.character(read.dcf(descPath, fields = "Package")[1]),
    NA_character_
  )
  if (is.na(pkg)) {
    message(sprintf("! [%s] Could not read Package field", ownerRepo))
    return(mkResult("BAD_DESC"))
  }
  message("[", ownerRepo, "] package: ", pkg)

  instDir <- file.path(tmp, "inst")
  if (dir.exists(instDir)) {
    message(sprintf(
      "    [%s] Symlinking inst/ contents to package root",
      ownerRepo
    ))
    files <- list.files(instDir, full.names = FALSE)
    for (f in files) {
      to <- file.path(tmp, f)
      if (!file.exists(to)) file.symlink(file.path(instDir, f), to)
    }
  }

  message(sprintf("    [%s] Calculating dependencies...", ownerRepo))
  depTree <- safe(
    pak::pkg_deps(paste0("local::", tmp), dependencies = TRUE),
    default = NULL
  )

  if (is.null(depTree)) {
    message(sprintf("! [%s] Failed to calculate dependencies", ownerRepo))
    return(mkResult("DEPS_FAILED"))
  }

  refsToInstall <- depTree$ref[
    depTree$package != currentPkgName & depTree$package != pkg
  ]

  if (length(refsToInstall) > 0) {
    message(sprintf(
      "    [%s] Installing %d dependencies (excluding %s)...",
      ownerRepo,
      length(refsToInstall),
      currentPkgName
    ))
    installLog <- safe(
      pak::pkg_install(refsToInstall, upgrade = FALSE),
      default = NULL
    )

    if (is.null(installLog)) {
      message(sprintf("! [%s] Dependency installation failed", ownerRepo))
      return(mkResult("INSTALL_DEPS_FAIL"))
    }
  } else {
    message(sprintf("    [%s] No external dependencies to install.", ownerRepo))
  }

  message(sprintf("    [%s] Installing package %s...", ownerRepo, pkg))
  finalInstallLog <- safe(
    pak::pkg_install(
      paste0("local::", tmp),
      dependencies = FALSE,
      upgrade = FALSE
    ),
    default = NULL
  )
  capture.output(
    print(finalInstallLog),
    file = file.path(outDir, "install.log")
  )

  if (is.null(finalInstallLog)) {
    message(sprintf("! [%s] installation failed", ownerRepo))
    return(mkResult("INSTALL_PKG_FAIL"))
  }

  message(
    "[",
    ownerRepo,
    "] running tests",
    if (!is.null(filterArg)) paste0(" (filter: ", filterArg, ")") else ""
  )
  testLogPath <- file.path(outDir, "tests.log")

  countsPath <- file.path(outDir, "test-counts.json")
  exitCode <- safe(
    runTestsIsolated(tmp, filterArg, testLogPath, countsPath),
    default = 1L
  )

  if (!file.exists(countsPath) || !identical(exitCode, 0L)) {
    message("[", ownerRepo, "] test execution CRASHED (see tests.log)\n")
    return(mkResult("CRASHED"))
  }

  counts <- safe(jsonlite::fromJSON(countsPath), default = NULL)
  if (is.null(counts)) {
    message("[", ownerRepo, "] test results unreadable (see tests.log)\n")
    return(mkResult("CRASHED"))
  }

  nFail <- counts$fail %||% 0
  nError <- counts$err %||% 0
  nWarn <- counts$warn %||% 0
  nSkip <- counts$skip %||% 0

  status <- if (nFail > 0 || nError > 0) "FAILURE" else "SUCCESS"

  sumJson <- list(
    owner_repo = ownerRepo,
    package = pkg,
    filter = filterArg,
    ref_mode = refMode,
    resolved_ref = resolvedRef,
    duration_sec = as.numeric(difftime(Sys.time(), start, units = "secs")),
    test_failures = nFail,
    test_errors = nError,
    test_warnings = nWarn,
    test_skipped = nSkip,
    status = status
  )
  writeLines(
    jsonlite::toJSON(sumJson, pretty = TRUE, auto_unbox = TRUE),
    file.path(outDir, "summary.json")
  )

  message(sprintf(
    "[%s] complete: Fail=%d Err=%d Warn=%d Skip=%d\n",
    ownerRepo,
    nFail,
    nError,
    nWarn,
    nSkip
  ))

  return(mkResult(status, nFail, nError, nWarn, nSkip))
}

main <- function() {
  pkgName <- getCurrentPackageName()
  configs <- readRepoList()
  message("[config] Checking ", length(configs), " repositories\n")

  allResults <- list()
  failures <- 0L

  for (config in configs) {
    res <- tryCatch(
      {
        runOneRepo(config, currentPkgName = pkgName)
      },
      error = function(e) {
        message("! Unhandled critical error: ", conditionMessage(e))
        list(
          repo = config$repo,
          refMode = config$refMode %||% "head",
          resolvedRef = NA_character_,
          status = "SCRIPT_ERROR",
          fail = 0,
          err = 0,
          warn = 0,
          skip = 0,
          time = 0
        )
      }
    )

    allResults[[paste(config$repo, config$refMode %||% "head", sep = "__")]] <- res

    if (res$fail > 0 || res$err > 0 || !res$status %in% c("SUCCESS")) {
      failures <- failures + 1L
    }
  }

  printSummary(allResults)

  message("\n[summary] completed with ", failures, " failures\n")
  if (failures > 0L) quit(status = 1L) else invisible(TRUE)
}

for (p in c("pak", "testthat", "jsonlite")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
main()
