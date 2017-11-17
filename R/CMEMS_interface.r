#' Get details from CMEMS command-line script
#'
#' Extracts the available details from a download script provided by CMEMS and uses it
#' to create an CMEMS.config object
#'
#' @export
#' @param script A text string provided by the CMEMS website
#' @details The CMEMS website has subsetting functionality that can be used as a template generate a download script for use with the motu
#' client (usually accessed by clicking on the "View Script" button when preparing to download). This function
#' saves the hard work of having to figure out the parameters for use with the \code{RCMEMS} package by parsing this command and
#' extracting the relevant information.
parse.CMEMS.script <- function(script){
  #Extract elements
  extract.arg <- function(arg,x) {
    gsub(paste("^.*?",arg," (.*?) -.{1} .*$",sep=""),"\\1",paste(x,"-$ ")) }
  argl <- list()
  argl$python <- gsub("^(.*?) .*$","\\1",script)
  argl$script <- gsub("^.*? (.*?) .*$","\\1",script)

  #Mapping between command line args and CMEMS.config
  arg.map <- list(user="-u",pwd="-p",motu="-m",
                  service.id="-s",product.id="-d",
                  variable="-v",
                  longitude.min="-x",longitude.max="-X",
                  latitude.min="-y",latitude.max="-Y",
                  date.min="-t",date.max="-T",
                  out.dir="-o",out.name="-f")
  for(a in names(arg.map)){
    argl[[a]] <- extract.arg(arg.map[[a]],script)
  }

  #Set the options
  cfg <- do.call(CMEMS.config,argl)

  return(cfg)
}

#' Download from CMEMS
#'
#' Provides R interfaces to the Python-based Motu client developed to provide access to CMEMS data. Two interfaces are
#' provided - the \code{CMEMS.download} interface is a simple interface taking advantage of R classes, while the
#' \code{\link{CMEMS.download.advanced}} interface provides the full functionality of the motu client.
#'CMEMS.download.advanced
#' @seealso Details and documentation on the Motu client, including releases of the software to download, can be
#'  found on the associated GitHub site, \url{https://github.com/clstoulouse/motu-client-python}
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
#' @param debug Allows debugging of the motu client command - builds the command without running it (logical)
#' @details Arguments provided to  \code{\link{CMEMS.download}} and  \code{\link{CMEMS.download.advanced}} override
#' any arguments supplied in the \code{\link{CMEMS.config}} object, x.
#' @return If debug is TRUE, returns the full command to the motu client, ready to be run (via \code{system()}) or checked manually. If
#' debug is FALSE (the default), runs the command and returns the error code associated with the script.
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
  } else {
    date.arg <- as.list(structure(format(range(date.range),'"%Y-%m-%d %H:%M:%S"'),names=c("date.min","date.max")))
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
                                    debug=FALSE) {
  #First check for a valid configuration

  #Extract the rest of the options from the CMEMS.config object to be build into a command
  slts.rest <- slotNames(x)[-which(slotNames(x) %in% c("python","script"))]
  cfg.l <- lapply(slts.rest,function(n) slot(x,n))
  names(cfg.l) <- slts.rest

  #Now take the rest of the configurations from this call and Populate it from the local enviroment
  this.arg.names <- formalArgs(CMEMS.download.advanced)
  this.arg.names <- subset(this.arg.names,!(this.arg.names %in% c("x","debug")))
  this.argl <- lapply(this.arg.names,function(n) get0(n))
  names(this.argl) <- this.arg.names

  #Compile into a single list and drop the nulls
  all.motu.args.l <- c(cfg.l,this.argl)
  motu.args.l <- all.motu.args.l[sapply(all.motu.args.l,length)!=0]

  #Build a list of arguments
  names(motu.args.l) <- gsub("\\.","-",names(motu.args.l))
  args.fmt <- sprintf("--%s=%s",names(motu.args.l),motu.args.l)


  #Run it
  if(!debug) {
    err.code <- system2(command=x@python, args=c(x@script,args.fmt))
    if(err.code!=0) stop("Error in running CMEMS download command.")
    return(err.code)
  } else{
    return(cmd)
  }
}

#'
#' library(raster)
#' cfg <- CMEMS.config(python="python",
#'                     script="~/Documents/common_data/motu-client-python/motu-client.py",
#'                     user = "mpayne",pwd = "hDny820n",
#'                     motu="http://cmems.isac.cnr.it/mis-gateway-servlet/Motu",
#'                     service.id = "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS",
#'                     product.id = "METOFFICE-GLO-SST-L4-RAN-OBS-SST",
#'                     variable = "analysed_sst")
#' #' # set.product.from.script('path_to_your_python_bin_directory/python path_to_your_motu_python_script_directory/motu-client.py -u your_user(1) -p your_password(1) -m http://cmems.isac.cnr.it/mis-gateway-servlet/Motu -s SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001-TDS -d METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2 -x -179.97500610352 -X 179.97500610352 -y -89.974998474121 -Y 89.974998474121 -t "2007-01-01 12:00:00" -T "2017-01-04 12:00:00" -v analysed_sst -o your_output_directory(1) -f your_output_file_name(1) --proxy-server your_proxy_server_url:your_proxy_port_number(2) --proxy-user your_proxy_user_login(3) --proxy-pwd your_proxy_user_password(3) ')
#' CMEMS.download(cfg,
#'                ROI = extent(8,13,55,59),
#'                date.range = c(ISOdate(2015,08,01),ISOdate(2015,08,10)),
#'                "test.nc",debug=FALSE)
