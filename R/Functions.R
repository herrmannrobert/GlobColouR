#--Create package------------------------------------------------------------------

if(F){
  #Packages
  .rs.restartR()
  packages  <- c("devtools", "roxygen2")
  if(length(packages[!(packages %in% installed.packages()[,"Package"])])){
    install.packages(packages[!(packages %in% installed.packages()[,"Package"])])}
  suppressWarnings(loaded <- lapply(packages, require, character.only = TRUE)); names(loaded) <- packages; loaded
  
  #PackageName
  pn <- "GlobColouR"
  
  #DescriptionFile
  custdes <- list("Package" = as.character(pn),
                  "Title" = "FTP client for GlobColour products",
                  "Maintainer" = "Robert Herrmann <robertherrmann@mail.de>",
                  "Author" = "Robert Herrmann",
                  "Version" = "0.1.0",
                  "Description" =  "The package provides a FTP client for the ACRI server including the GlobColour data set. Main features are checking the data availability and automated downloads of previously created file directories.
                                    The GlobColour project (globcolour.info) provides an open-source data set of satellite-based ocean color products. It is developed and maintained by ACRI-ST, France. Data output is based on single sensors (e.g. SeaWIFS, MERIS, MODIS, VIIRS, OLCIA) and also available on a merged basis. Time coverage of data is from 1997 onwards, available in daily, eight day and monthly time steps. Spatial resolution is given in 1 km (Europe only), 4 km, 25 km and 100 km. Data is supplied in NetCDF4 or PNG containing a variety of biological, atmospheric optical, ocean surface optical and ocean subsurface optical parameters.",
                  "Depends"= "R (>= 3.5.1)",
                  "License" = "What license is it under?",
                  "Encoding" = "UTF-8",
                  "LazyData" = "true",
                  "RoxygenNote" = "6.1.0",
                  "Import" = "curl, 
                              httr,
                              png,
                              ncdf4")
  
  #CreatePackage
  setwd(paste(Sys.getenv("HOME"), "Dropbox/GitHub", sep = "/"))
  create(pn, custdes)
  
  #CreateDocumentation
  setwd(paste("./", pn, sep = ""))
  document()
  
  #TestingPackage
  setwd("..")
  install(pn)
  library(GlobColouR)
  help(package = as.character(pn))
  
  #WritingManual
  check(pn, check_dir = getwd(), manual = TRUE)
  
  #LoadFromGitHub
  .rs.restartR()
  remove.packages(pn)
  install_github(paste("herrmannrobert", pn, sep = "/"), force = TRUE)
  library(GlobColouR)
  help(package = as.character(pn))
}

#------------------------------------
#---Set user information-------------
#------------------------------------

#' @title Set username and password used for authentication by ACRI
#' @description Function to set username and password used for authentication by ACRI.
#' @param username character string, username registered at \href{http://hermes.acri.fr/index.php?class=ftp_access}{hermes.acri.fr}.
#' @param password character string, password provided by ACRI after registration.
#' @details Function sets environment variables (ACRI_FTP_USERNAME, ACRI_FTP_PASSWORD) used for FTP connection to the GlobColour database.
#' Previous registration for access on the GlobColour archive is required.
#' @export
#' @examples 
#' \dontrun{
#'
#' gc_set_user("ftp_gc_DDummy", "DDummy_1234")
#' Sys.getenv(c("ACRI_FTP_USERNAME", "ACRI_FTP_PASSWORD"))
#' }

gc_set_user <- function(username, password){
  
  Sys.setenv(ACRI_FTP_USERNAME = as.character(username),
             ACRI_FTP_PASSWORD = as.character(password))
  
}

#------------------------------------
#---Get available options------------
#------------------------------------

#' @title Get vector of available GlobColour products
#' @description Function examines available GlobColour products for a certain query and provides the respective file 
#' paths for direct FTP access via \code{\link{gc_get_colour}}.
#' @param zone character indicating the requested region (mendatory). Use "GLOB" for global coverage or "EURO" for the european area (see Details).
#' @param sens character indicating a satellite acronym (mendatory). Accepted values are "meris", "viirsn" ,"olcia", "modis", "seawifs", or
#' "merged". 
#' @param binp character indicating the binning period (mendatory). Accepted values are "month", "8-day", or "day".
#' @param date character defining the requested date (mendatory). Required format is "yyyy/mm/dd". Use "latest" for 
#' most current data (see Details).
#' @param type character or vector of product types (optional). See Details for provided GlobColour products.
#' @param fext character or vector of file extentions (optional). Provided data formats are "nc" (NetCDF4) and "png" (PortableNetworkGraphic).
#' @param prle character or vector indicating the product level (optional). Accepted values are "L3m" (mapped grid) and "L3b" (ISIN grid).
#' @param reso character or vector specifying the resolution (optional) to "1", "4", "25", and/or "100" km (see Details).
#' @details The first four arguments (\code{zone}, \code{sens}, \code{binp}, \code{date}) are mendatory, without which the function returns 
#' a table of accepted values to type in. The other arguments (\code{type}, \code{fext}, \code{prle}, \code{reso}) provide further filtering. 
#' For additional information see the \href{http://www.globcolour.info/CDR_Docs/GlobCOLOUR_PUG.pdf}{GlobColour Product User's Guide}.
#' @details The resolutions provided for the global coverage are 4, 25, and 100 km. The resolution for the europe-specific dataset 
#' is 1 km only. Values can optionally be set by \code{reso}.
#' @details If the requested \code{date} is after the most current date availabe the function automatically updates this attribute. The user
#' can also type "latest" for automatically fetching the latest release.
#' @details The function shows all available types of data when using the mendatory arguments only. The provided data types (e.g. "KD490", "PAR", "CHL1") 
#' can then be used in \code{type} to accelerate the search process, and much more importantly, to shorten the list of files for download.
#' @export
#' @examples 
#' \dontrun{
#' 
#' #Provide user information
#' gc_set_user("ftp_gc_DDummy", "DDummy_1234")
#' 
#' #Accepted values to use for arguments (mendatory vs. optional)
#' gc_get_option()
#' 
#' #Latest available ocean optics (merged) on global coverage and daily intervals
#' gc_get_option(zone = "GLOB",
#'               sens = "merged",
#'               binp = "day",
#'               date = "latest")
#' 
#' #Further filtering options
#' gc_get_option(zone = "GLOB",
#'               sens = "merged",
#'               binp = "day",
#'               date = "latest",                          
#'               type = c("KD490", "PAR"),
#'               fext = c("nc"),
#'               reso = c(4, 25))
#' }

gc_get_option <- function(zone, sens, binp, date, type, fext, prle, reso){

  #zone = "GLOB"
  #sens = "merged"
  #binp = "day"
  #date = "latest"
  
  ## ChecksMissingPar
  
  ifelse(missing(zone) | missing(sens) | missing(binp) | missing(date),
         missingP <- TRUE,
         missingP <- FALSE)
  
  ## StopIfUserInformationIsMissing
  
  if(Sys.getenv("ACRI_FTP_USERNAME") == "" | Sys.getenv("ACRI_FTP_PASSWORD") == ""){
    stop("Use gc_set_user() to provide required user information...")
  }
  
  ## AdditionalFunction
  
  get_dir_listing <- function(path = "/") {
    res <- curl::curl_fetch_memory(
      paste0("ftp://ftp.hermes.acri.fr", path),
      curl::new_handle(
        username = Sys.getenv("ACRI_FTP_USERNAME"),
        password = Sys.getenv("ACRI_FTP_PASSWORD"),
        dirlistonly = TRUE
      )
    )
    strsplit(readBin(res$content, "character"), "\n")[[1]]
  }
  
  ## ProvidesAvailableOptions
  
  if(missingP){
    t_zone <- get_dir_listing()[c(1,4)]
    t_sens <- get_dir_listing(paste0("/", t_zone[1], "/"))
    t_binp <- get_dir_listing(paste0("/", t_zone[1], "/", t_sens[1], "/"))
    cat("\nUse the following arguments to define your query:\n",
              "\n    MANDATORY",
              "\n    ---------",
        paste("\n    zone: ", paste(t_zone, collapse = " | ")),
        paste("\n    sens: ", paste(t_sens, collapse = " | ")),
        paste("\n    binp: ", paste(t_binp, collapse = " | ")),
              "\n    date:  yyyy/mm/dd | latest\n",
              "\n    OPTIONAL",
              "\n    --------",
        paste("\n    type: ", "KD490 | KDPAR | CHL1 | ... | PAR | ZSD-DORON"),
        paste("\n    fext: ", "nc | png"),
        paste("\n    prle: ", "L3b | L3m"),
        paste("\n    reso: ", "4 | 25 | 100"),
              "\n\n")
  }
  
  ## AvailableDataForMendatoryArguments
  
  if(!missingP){
    y.m <- max(get_dir_listing(paste0("/", zone, "/", sens, "/", binp, "/", "/")))
    m.m <- max(get_dir_listing(paste0("/", zone, "/", sens, "/", binp, "/", y.m, "/")))
    d.m <- max(get_dir_listing(paste0("/", zone, "/", sens, "/", binp, "/", y.m, "/", m.m, "/")))
    latestD <- paste(y.m, m.m, d.m, sep = "/")
    
    if(date == "latest"){
      date <- latestD
    }else{
      if(as.Date(date) > as.Date(latestD)){
        date <- latestD
      }
    }
    
    sD <- unlist(strsplit(date, "/"))
    
    if(binp == "8-day"){
      ym <- paste(sD[1], sD[2], sep = "/")
      d <- as.numeric(sD[3])
      DateOp <- as.numeric(get_dir_listing(paste0("/", zone, "/", sens, "/", binp, "/", ym, "/")))
      newd <- sprintf("%02d", DateOp[which.min(abs(d - replace(DateOp, DateOp > d, Inf)))])
      date <- paste(ym, newd, sep = "/")
    }
    
    if(binp == "month"){
      substr(date, 9, 10) <- "01"
    }
    
    dirpath <- paste0("/", zone, "/", sens, "/", binp, "/", date, "/")
    query <- get_dir_listing(dirpath)
  
  }
  
  ## AvailableDataForOptionalArguments
  
  if(!missingP){
    
    if(!missing(fext)){
      query <- query[grep(paste(paste0(".", fext), collapse = "|"), query)]
    }
    
    if(!missing(type)){
      query <- query[grep(paste(paste0("_", type, "_"), collapse = "|"), query)]
    }
    
    if(!missing(prle)){
      query <- query[grep(paste(prle, collapse = "|"), query)]
    }
    
    if(!missing(reso) & zone != "EURO"){
      queryReso <- query[grep(paste(paste0("_", reso, "_"), collapse = "|"), query)]
      if(!identical(queryReso, character(0))){
        query <- queryReso
      }
    }
    
    ifelse(identical(query, character(0)),
           stop("No files found..."),
           return(paste0(dirpath, query)))
  }
}

#------------------------------------
#---Get data from server-------------
#------------------------------------

#' @title Fetching GlobColour products
#' @description Function downloads the requested GlobColour product, given as file directories created by \code{\link{gc_get_option}}.
#' @param query    character or vector giving the file directories.
#' @param openFile logical, if TRUE, function opens previously downloaded file (see Details). If FALSE, files are saved to working directory (default).
#' @details Required file directories can either be generated by \code{\link{gc_get_colour}} or, if known, manually created. Note
#' that the quantity of file directories is dramatically correlated with download time.
#' @details If \code{openFile} is set TRUE the function creates a temporary file (see \code{\link[base]{tempfile}}), which is immediately opend after 
#' download (see Examples). Method requires \code{length(query) < 2}. For further details see \code{\link[ncdf4]{nc_open}} and \code{\link[png]{readPNG}}.
#' @export
#' @examples 
#' \dontrun{
#'
#' #Provide user information
#' gc_set_user("ftp_gc_DDummy", "DDummy_1234")
#' 
#' #Create directory for quick-look-file.png
#' qu <- gc_get_option(zone = "GLOB",
#'                    sens = "merged",
#'                    binp = "day",
#'                    date = "2017/08/19",
#'                    type = "KD490",
#'                    fext = "png")
#'              
#' #Download and assign file to object                          
#' test <- gc_get_colour(qu, openFile = TRUE)
#' 
#' #Plotting
#' grid::grid.raster(test)
#' }

gc_get_colour <- function(query, openFile = FALSE){
  
  ## ErrorMassaging
   
  if(Sys.getenv("ACRI_FTP_USERNAME") == "" | Sys.getenv("ACRI_FTP_PASSWORD") == ""){
    stop("Use gc_set_user() to provide required user information...")
  }
  
  if(openFile & length(query) > 1){
    stop("Can't open multiple files. Check query...")
  }

  ## CreatePaths
  
  ftppath <- paste0("ftp://ftp.hermes.acri.fr", query)
  filenic <- paste0("L3", sapply(strsplit(query, "L3"), `[`, 2))
  fileext <- paste0(".", strsplit(filenic[1], "[.]")[[1]][2])
  
  ifelse(openFile == TRUE,
         disk <- tempfile(fileext = fileext),
         disk <- paste0(getwd(), "/", filenic)
  )
  
  ## FetchingData
  
  for(i in seq_along(query)){
    if(i == 1){cat("\n")}
    writeLines(paste("  ", filenic[i]))
    suppressWarnings(
      httr::GET(
        url = ftppath[i],
        httr::authenticate(Sys.getenv("ACRI_FTP_USERNAME"), Sys.getenv("ACRI_FTP_PASSWORD")),
        httr::write_disk(disk[i], overwrite = TRUE),
        httr::progress()
      )
    )
    if(i == length(query)){cat("\n")}
  }
  
  ## ImmidiatelyOpensPngOrNetCDF4
  
  if(openFile){
    ifelse(fileext == ".nc",
           return(ncdf4::nc_open(disk)),
           return(png::readPNG(disk))
    )
  }
}