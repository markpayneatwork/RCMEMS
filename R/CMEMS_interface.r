#' Get configuration from CMEMS command-line script
#'
#' Extracts the available details from a download script provided by CMEMS and uses it
#' to create an CMEMS.config object
#'
#' @export
#' @param script A text string for a download script provided by the CMEMS website
#' @param parse.user A logical variable indicating whether the username and password should be parsed
#' or not. Typically, these are place holder values, so it's not really necessary to parse them.
#' @details The CMEMS website has subsetting functionality that can be used as a template generate a download script for use with the motu
#' client (usually accessed by clicking on the "View Script" button when preparing to download). This function
#' saves the hard work of having to figure out the parameters for use with the \code{RCMEMS} package by parsing this command and
#' extracting the relevant information.
#' @examples
#' script <- 'python -m motuclient --user <USERNAME> --pwd <PASSWORD> --motu http://nrtcmems.mercator-ocean.fr/motu-web/Motu --service-id GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS --product-id global-analysis-forecast-phy-001-024-monthly --longitude-min -180 --longitude-max 179.91667175293 --latitude-min -80 --latitude-max 90 --date-min "2007-01-16 12:00:00" --date-max "2018-01-16 12:00:00" --depth-min 186.1255 --depth-max 763.3333 --variable so -out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>'
#' cfg <- parse.CMEMS.script(script)
#' cfg
#' #Note that the error in "-out-dir" (using one minus, instead of two) has been picked up and nothing.
#' #We can correct this using update
#' cfg2 <- update(cfg,out.dir="data")
parse.CMEMS.script <- function(script,parse.user=FALSE){

  #Mapping between command line args and CMEMS.config
  #Note that we only focus here on arguments supplied by a double "--", as this is easier to handle
  arg.map <- list(module="-m",
                  user="--user",
                  pwd="--pwd",
                  motu="--motu",
                  service.id="--service-id",
                  product.id="--product-id",
                  longitude.min="--longitude-min",
                  longitude.max="--longitude-max",
                  latitude.min="--latitude-min",
                  latitude.max="--latitude-max",
                  date.min="--date-min",
                  date.max="--date-max",
                  depth.min="--depth-min",
                  depth.max="--depth-max",
                  out.dir="--out-dir",
                  out.name="--out-name")

  #Break input script into its individual elements
  script.atoms <- scan(text=script,what="character",quiet=TRUE)
  
  #Now do the extraction by looping over the list of argument names that we want to extract
  #Find their location in the list, then extract the next element as the argument
  argl <- list()
  for(a in names(arg.map)){
    arg.idx <- which(script.atoms==arg.map[[a]])
    if(length(arg.idx)==1) {
      argl[[a]] <- script.atoms[arg.idx+1]
    } else if(length(arg.idx)>1) warning(sprintf('Multiple matches for "%s" argument. None selected.',arg.map[[a]]))
  }
  
  #Handle the client configuration elements
  argl$python <- gsub("^(.*?) .*$","\\1",script)
  if(is.null(argl$module)) {
    argl$script <- gsub("^.*? (.*?) .*$","\\1",script)
  } else {
    argl$module <- NULL
  }
  
  #Handle variables explicitly, to deal with cases where 
  var.idxs <- which(script.atoms=="--variable")
  argl$variable <- script.atoms[var.idxs+1]

  #Set the options
  cfg <- do.call(CMEMS.config,argl)

  #Set user and password to null
  if(!parse.user) {
    cfg@user <- as.character(NULL)
    cfg@pwd <- as.character(NULL)
  }

  return(cfg)
}

#' Download from CMEMS
#'
#' Provides R interfaces to the Python-based Motu client developed to provide access to 
#' CMEMS data. Two interfaces are provided - the \code{CMEMS.download} interface is a simple 
#' interface taking advantage of R classes, while the \code{\link{CMEMS.download.advanced}}
#' interface provides access to the full functionality of the motu client.
#' @seealso Details and documentation on the Motu client, including releases of the software 
#' to download, can be found on the associated GitHub site, \url{https://github.com/clstoulouse/motu-client-python}
#' @name CMEMS.download
#' @export
#' @param x An object of class \code{\link{CMEMS.config}} containing the configuration parameters
#' @param ROI An extent object from the raster package specifying the region of interest to subset
#' @param date.range The maximum and minimum dates (vector of length two of class "Date")
#' @param depth.range The maximum and minimum depths specificed as a vector of length 2 (float in the interval [0 ; 2e31 ] -
#' does not accept 'Surface' as an argument)
#' @param ... Arguments to be passed on further to the \code{\link{CMEMS.download.advanced}} function. Overwrites any arguments automatically generated
#' in \code{\link{CMEMS.download}} function.
#' @param date.min The min date with optional hour resolution (string following format YYYY-MM-DD [HH:MM:SS])
#' @param date.max The max date with optional hour resolution (string following format YYYY-MM-DD [HH:MM:SS ])
#' @param latitude.min The min latitude (float in the interval [-90 ; 90 ])
#' @param latitude.max The max latitude (float in the interval [-90 ; 90 ])
#' @param longitude.min The min longitude (float in the interval [-180 ; 180 ])
#' @param longitude.max The max longitude (float in the interval [-180 ; 180 ])
#' @param depth.min The min depth (float in the interval [0 ; 2e31 ] or string 'Surface')
#' @param depth.max The max depth (float in the interval [0 ; 2e31 ] or string 'Surface')
#' @param describe.product It allows to have all updated information on a dataset. Output is in XML format
#' @param out.dir The output dir (string)
#' @param out.name The output file name (string)
#' @param quiet Logical value, indicating whether to surpress output
#' @param debug Allows debugging of the motu client command - builds the command without running it (logical)
#' @details Arguments provided to  \code{\link{CMEMS.download}} and  \code{\link{CMEMS.download.advanced}} override
#' any arguments supplied in the \code{\link{CMEMS.config}} object, x.
#' @details If the  \code{\link{CMEMS.config}} object, x, is missing either the username or the
#' password, both are dropped from the call to the MOTU client - in this case, the client will 
#' use the local configuration file. See the README.md file supplied with the MOTU client for how
#' to set this up.
#' @return If debug is TRUE, returns the full command to the motu client, ready to be run (via \code{system()}) or checked manually. If
#' debug is FALSE (the default), runs the command and returns the error code associated with the motuclient.
#' @examples 
#' \dontrun{
#' library(raster)
#' cfg <- CMEMS.config(python="python",
#'                     motu="http://cmems.isac.cnr.it/mis-gateway-servlet/Motu",
#'                     service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
#'                     product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST",
#'                     variable = "analysed_sst")
#' CMEMS.download(cfg,
#'                ROI = extent(8,13,55,59),
#'                date.range = c(ISOdate(2001,08,01),ISOdate(2001,08,10)),
#'                "test.nc",debug=FALSE)
#'}
CMEMS.download <- function(x,
                           ROI="missing",
                           date.range="missing",
                           out.path="missing",
                           depth.range="missing",
                           ...) {
  require(raster)
  #Build spatial ROI arguments
  if(missing("ROI")) {
    ROI.args <- sapply(c("longitude.min","longitude.max","latitude.min","latitude.max"),
                       slot,object=x,simplify=FALSE)
  } else { #Take it from the function argument
    ROI.args <- as.list(structure(ROI[1:4],names=c("longitude.min","longitude.max","latitude.min","latitude.max")))

  }

  #Build date variables
  if(missing(date.range)){
    date.arg <- list(date.min=x@date.min,date.max=x@date.max)
    #Ensure that we have the quote marks right here by first stripping, then reapplying
    date.arg <- lapply(date.arg,function(s) sprintf('"%s"',gsub('"',"",s)))
  } else {
    date.arg <- as.list(structure(format(range(date.range),'"%Y-%m-%d %H:%M:%S"'),
                                  names=c("date.min","date.max")))
  }

  #Build depth variables, if supplied
  if(missing(depth.range)) {
    depth.args <- list(depth.min=x@depth.min,depth.max=x@depth.max)
  } else if(!is.numeric(depth.range)) {
    stop("'depth.range' requires a numeric argument. To use the 'Surface' argument, please use the depth.min and
         depth.max arguments of CMEMS.advanced.")
  } else {
    depth.args <- list(depth.min=min(depth.range),depth.max=max(depth.range))
  }

  #Split out.path into separate file names and directories
  if(missing(out.path)) {
    path.arg <- list(out.dir=x@out.dir,out.name=x@out.name)
  } else {
    path.arg <- list(out.dir=dirname(out.path),out.name=basename(out.path))
  }

  #Combine all argument and use the ... to allow additional argument or overwriting
  arg.l <- c(list(x=x),ROI.args,date.arg,depth.args,path.arg)
  dot.l <- list(...)
  arg.l[names(dot.l)] <- dot.l

  #Do the call
  rtn <- do.call(CMEMS.download.advanced,arg.l)

  return(rtn)
}

#' @export
#' @rdname CMEMS.download
CMEMS.download.advanced <- function(x,
                                    out.dir=NULL,
                                    out.name=NULL,
                                    date.min=NULL,
                                    date.max=NULL,
                                    latitude.min=NULL,
                                    latitude.max=NULL,
                                    longitude.min=NULL,
                                    longitude.max=NULL,
                                    depth.min=NULL,
                                    depth.max=NULL,
                                    quiet=FALSE,
                                    debug=FALSE) {
  #Extract the rest of the options from the CMEMS.config object to be build into a command
  slts.rest <- slotNames(x)[-which(slotNames(x) %in% c("python","script","variable"))]
  cfg.l <- lapply(slts.rest,function(n) slot(x,n))
  names(cfg.l) <- slts.rest
  
  #Check whether both the username and the password have been supplied.
  #If not, drop these from the arguments to the command - this should 
  #encourage the client to pick up the names from the local configuration file
  if(length(x@user)==0 | length(x@pwd)==0 ) {
    if(!quiet) {
      message("RCMEMS: Username or password is missing - using local configuration file instead.")
    }
    cfg.l[c("user","pwd")] <- NULL
  }
  
  #Now take the rest of the configurations from this call and 
  #populate it from the local enviroment
  this.arg.names <- formalArgs(CMEMS.download.advanced)
  this.arg.names <- subset(this.arg.names,!(this.arg.names %in% c("x","debug","quiet")))
  this.argl <- lapply(this.arg.names,function(n) get0(n))
  names(this.argl) <- this.arg.names
  
  #The dates need special handling to ensure that we get the quotes right
  if(!is.null(date.min)) {
    this.argl[["date.min"]] <- sprintf('"%s"',date.min)
  }
  if(!is.null(date.max)) {
    this.argl[["date.max"]] <- sprintf('"%s"',date.max)
  }
  
  #Compile into a single list (allowing for overwriting from the command line) 
  all.motu.args.l <- cfg.l
  all.motu.args.l[names(this.argl)] <- this.argl
  
  #Drop the nulls, then build a list of arguments
  motu.args.l <- all.motu.args.l[sapply(all.motu.args.l,length)!=0]
  names(motu.args.l) <- gsub("\\.","-",names(motu.args.l))
  args.fmt <- sprintf("--%s=%s",names(motu.args.l),motu.args.l)
  
  #Handle variables specifically - problematic when dealing with multiple variables
  args.fmt <- c(args.fmt,paste("--variable",x@variable))

  #Add quietly, if necessary
  if(quiet) {args.fmt <- c(args.fmt,"--quiet")}
  
  #Decide how to run it
  if(is.na(x@script) ) {
    client.cmd <- "-m motuclient"
  } else  {
    client.cmd <- x@script
  }

  #Run it
  if(!debug) {
    err.code <- system2(command=x@python, 
                        args=c(client.cmd,args.fmt))
    if(err.code!=0) stop("Error in running CMEMS download command.")
    return(err.code)
  } else{
    return(list(command=x@python, args=c(client.cmd,args.fmt)))
  }
}

#' Get product details 
#'
#' Requests details from the CMEMS servers about a specific product, including temporal coverage
#' and depth layers
#'
#' @export
#' 
#' @param x An object of class \code{\link{CMEMS.config}} containing the configuration 
#' parameters. At a minimum, the \code{python}, \code{script}, \code{motu}, \code{service.id}
#' and \code{product.id} slots need to be populated.
#' @param variable Specifies the object to return. Valid values are "times" and 
#' "depths". All other values, including omission, return an xml object describing the product.
#' @param quiet Logical, indicating whether to surpress output
#' @details This function returns the description that is associated with the particular product
#' id. 

#' @examples
#' \dontrun{
#' cfg <- CMEMS.config(python="python",
#'                     script="~/Documents/common_data/motu-client-python/motu-client.py",
#'                     motu="http://cmems.isac.cnr.it/mis-gateway-servlet/Motu",
#'                     service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
#'                     product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST")
#' SST.dates <- product.description(cfg,"times")
#' range(as.Date(SST.dates))
#'}
product.description <- function(x,variable="missing",quiet=TRUE) {
  require(xml2)
  #Extract the rest of the options from the CMEMS.config object to be build into a command
  extract.these <- c("motu","service.id","product.id")
  extract.l <- lapply(extract.these,function(n) slot(x,n))
  names(extract.l) <- gsub("\\.","-",extract.these)
  
  #Build a list of arguments
  xml.fname <- tempfile(fileext=".xml")
  args.fmt <- c(sprintf("--%s=%s",names(extract.l),extract.l),
                sprintf("--out-dir=%s",dirname(xml.fname)),
                sprintf("--out-name=%s",basename(xml.fname)))
  
  #Add quietly, if necessary
  if(quiet) {args.fmt <- c(args.fmt,"--quiet")}
  
  #Decide how to run it
  if(is.na(x@script)) {
    client.cmd <- "-m motuclient"
  } else  {
    client.cmd <- x@script
  }
  
  #Retrieve description
  err.code <- system2(x@python,args=c(client.cmd,args.fmt,"--describe-product"))
  if(err.code!=0) stop("Error in retrieving CMEMS product description")
  
  #Now import the xml
  xml.obj <- xml2::read_xml(xml.fname)
  
  #Return
  valid.vars <- c("times"="availableTimes",
                  "depths"="availableDepths")
  if(variable %in% names(valid.vars)) { #Return the xml
      txt <- xml_text(xml_child(xml.obj,valid.vars[variable]))
      rtn <- scan(text=txt,sep=";",what=character(),quiet = TRUE)
  } else { #Return xml
    rtn <- xml.obj
  }
  return(rtn) 
}



#' Get Motu client version
#' 
#' Returns the version number of the motu client specified by x
#'
#' @param x An object of class \code{\link{CMEMS.config}} containing the configuration 
#' parameters. At a minimum, the \code{python} and \code{script} slots need to be populated.
#'
#' @return NULL
#' @export
#'
#' @examples
#' ## NOT RUN ## 
#' cfg <- CMEMS.config(python="python",script="motu-client.py")
#' get.motu.client.version(cfg)
#' cfg

get.motu.client.version <- function(x) {
  #Decide how to run it
  if(is.na(x@script)) {
    client.cmd <- "-m motuclient"
  } else  {
    client.cmd <- x@script
  }
  
  rtn <- system2(command=x@python, args=c(client.cmd,"--version"),stdout=TRUE)
  return(rtn)
}
