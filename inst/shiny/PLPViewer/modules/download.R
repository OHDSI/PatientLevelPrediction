downloadViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
  shinydashboard::box(title = "Development R Package", status = 'info', solidHeader = T,
                      shiny::p("Click here to download an R package that contains all the settings requires to replicate the model development using any OMOP CDM database."),
                      shiny::actionButton(inputId = ns('downloadPackageDev'), label = "Download Development")
  ),
  shinydashboard::box(title = "Validation R Package", status = 'info', solidHeader = T,
                      shiny::p("Click here to download an R package that contains all the settings requires to validate the existing model using any OMOP CDM database."),
                      shiny::actionButton(inputId = ns('downloadPackageVal'), label = "Download Validation")
                      
  )
  )
}

downloadServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$downloadPackageDev, {
        
        dir.create(file.path('/Users/jreps/Downloads', 'devPackage'), recursive = T)
        #Hydra::hydrate(specifications = specifications, outputFolder = outputPackageLocation)
        createPackage <- tryCatch({downLoadSkeleton(outputFolder = file.path('/Users/jreps/Downloads'),
                                                    packageName = 'devPackage',
                                                    skeletonType = 'SkeletonPredictionStudy')#'SkeletonPredictionValidationStudy'
        }, error = function(e){return(NULL)})
        
        if(!is.null(createPackage)){
          createPackage <- tryCatch({replaceName(packageLocation = file.path('/Users/jreps/Downloads', 'devPackage'),
                                                 packageName = 'devPackage',
                                                 skeletonType = 'SkeletonPredictionStudy')},
                                    error = function(e){return(NULL)})
        }
        
        
      })
      
    }
  )
}

### DOWNLOAD

downLoadSkeleton <- function(outputFolder,
                             packageName,
                             skeletonType = 'SkeletonPredictionStudy'){
  download.file(url = paste0("https://github.com/ohdsi/",skeletonType,"/archive/master.zip")
                , destfile = file.path(outputFolder, "package.zip"))
  # unzip the .zip file
  unzip(zipfile = file.path(outputFolder, "package.zip"), exdir = outputFolder)
  file.rename( from = file.path(outputFolder, paste0(skeletonType, '-master')),
               to = file.path(outputFolder,  packageName))
  unlink(file.path(outputFolder, "package.zip"))
  return(file.path(outputFolder, packageName))
}

# change name
replaceName <- function(packageLocation = getwd(),
                        packageName = 'ValidateRCRI',
                        skeletonType = 'SkeletonPredictionValidationStudy'){
  
  filesToRename <- c(paste0(skeletonType,".Rproj"),paste0("R/",skeletonType,".R"))
  for(f in filesToRename){
    ParallelLogger::logInfo(paste0('Renaming ', f))
    fnew <- gsub(skeletonType, packageName, f)
    file.rename(from = file.path(packageLocation,f), to = file.path(packageLocation,fnew))
  }
  
  filesToEdit <- c(file.path(packageLocation,"DESCRIPTION"),
                   file.path(packageLocation,"README.md"),
                   file.path(packageLocation,"extras/CodeToRun.R"),
                   dir(file.path(packageLocation,"R"), full.names = T))
  for( f in filesToEdit ){
    ParallelLogger::logInfo(paste0('Editing ', f))
    x <- readLines(f)
    y <- gsub( skeletonType, packageName, x )
    cat(y, file=f, sep="\n")
    
  }
  
  return(packageName)
}