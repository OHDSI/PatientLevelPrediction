readReleaseMetadata <- function(
    descriptionPath = "DESCRIPTION",
    newsPath = "NEWS.md",
    tags = system("git tag --list", intern = TRUE)) {
  description <- read.dcf(descriptionPath, fields = c("Package", "Version"))[1, ]
  packageName <- unname(description[["Package"]])
  packageVersion <- numeric_version(unname(description[["Version"]]))

  versionTags <- tags[grepl("^v[0-9]+\\.[0-9]+\\.[0-9]+$", tags)]
  if (length(versionTags) == 0) {
    stop("No semantic version tags matching v<major>.<minor>.<patch> were found")
  }
  taggedVersions <- numeric_version(sub("^v", "", versionTags))
  latestVersion <- max(taggedVersions)
  isReleaseVersion <- grepl(
    "^[0-9]+\\.[0-9]+\\.[0-9]+$",
    as.character(packageVersion)
  )
  newVersion <- if (isReleaseVersion && packageVersion > latestVersion) {
    paste0("v", packageVersion)
  } else {
    ""
  }

  news <- readLines(newsPath, warn = FALSE)
  heading <- paste(packageName, packageVersion)
  headingIndex <- which(news == heading)
  if (length(headingIndex) != 1) {
    stop("Expected exactly one NEWS heading named: ", heading)
  }
  start <- headingIndex + 1L
  if (start <= length(news) && grepl("^=+$", news[start])) {
    start <- start + 1L
  }
  laterHeadings <- which(
    seq_along(news) > headingIndex &
      grepl(paste0("^", packageName, " [0-9]+\\.[0-9]+\\.[0-9]+"), news)
  )
  end <- if (length(laterHeadings)) min(laterHeadings) - 1L else length(news)
  notes <- news[seq.int(start, end)]
  while (length(notes) && !nzchar(trimws(notes[1]))) notes <- notes[-1]
  while (length(notes) && !nzchar(trimws(notes[length(notes)]))) notes <- notes[-length(notes)]
  if (!length(notes) || !any(nzchar(trimws(notes)))) {
    stop("The NEWS section for ", packageVersion, " is empty")
  }

  list(
    package = packageName,
    version = as.character(packageVersion),
    latestVersion = as.character(latestVersion),
    newVersion = newVersion,
    notes = notes
  )
}

writeReleaseMetadata <- function(metadata, outputPath, notesPath) {
  writeLines(metadata$notes, notesPath)
  if (nzchar(outputPath)) {
    writeLines(c(
      paste0("package_version=", metadata$version),
      paste0("latest_version=", metadata$latestVersion),
      paste0("new_version=", metadata$newVersion),
      paste0("notes_file=", notesPath)
    ), outputPath, useBytes = TRUE)
  }
}

selfTestReleaseMetadata <- function() {
  temp <- tempfile("release-metadata-")
  dir.create(temp)
  descriptionPath <- file.path(temp, "DESCRIPTION")
  newsPath <- file.path(temp, "NEWS.md")
  writeLines(c("Package: ExamplePackage", "Version: 6.7.0"), descriptionPath)
  writeLines(c(
    "ExamplePackage 6.7.0",
    "====================",
    "",
    "- New release",
    "",
    "ExamplePackage 6.6.9",
    "====================",
    "- Previous release"
  ), newsPath)

  metadata <- readReleaseMetadata(
    descriptionPath,
    newsPath,
    tags = c("not-a-version", "v6.6.9", "v6.5.12")
  )
  stopifnot(
    identical(metadata$newVersion, "v6.7.0"),
    identical(metadata$latestVersion, "6.6.9"),
    identical(metadata$notes, "- New release")
  )

  writeLines(c("Package: ExamplePackage", "Version: 6.7.0.9999"), descriptionPath)
  writeLines(c(
    "ExamplePackage 6.7.0.9999",
    "=========================",
    "- Development version"
  ), newsPath)
  developmentMetadata <- readReleaseMetadata(
    descriptionPath,
    newsPath,
    tags = "v6.7.0"
  )
  stopifnot(identical(developmentMetadata$newVersion, ""))
  invisible(TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
if (identical(args, "--self-test")) {
  selfTestReleaseMetadata()
} else {
  outputPath <- if (length(args) >= 1) args[[1]] else ""
  notesPath <- if (length(args) >= 2) args[[2]] else "release-notes.md"
  metadata <- readReleaseMetadata()
  writeReleaseMetadata(metadata, outputPath, notesPath)
  message(
    "Package version ", metadata$version,
    "; latest tag ", metadata$latestVersion,
    if (nzchar(metadata$newVersion)) paste0("; release ", metadata$newVersion) else "; no release"
  )
}
