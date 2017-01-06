# ========================================================================
# Motu Interface
# ========================================================================

#' Configure MOTU Client
#'
#' Sets parameters to configure thePython-based Motu client
#'
#' @seealso Details and documentation on the Motu client, including releases, can be found on the associated GitHub
#' site, \url{https://github.com/clstoulouse/motu-client-python}
#' @name configure.motu.client
#' @export
#' @param python Path to the local python installation
#' @param script Path to the local install of motu - normally this would point to the motu-client.py script
#' @param user user name (string)
#' @param pwd user password (string)
#' @param auth-mode the authentication mode: [default: cas]. Use "none" for no authentication, "basic" for basic authentication,
#' cas for Central Authentication Service
#' @param proxy-server  the proxy server (url)
#' @param proxy-user the proxy user (string)
#' @param proxy-pwd the proxy password (string)
#' @param socket-timeout Set a timeout on blocking socket operations (float expressing seconds)
#' @param user-agent Set the identification string (user-agent) for HTTP requests. By default this value is 'Python-urllib/x.x' (where x.x is the version of the python interpreter)
configure.motu.client<- function(python="python",
                           script=NULL,
                           user=NULL,
                           pwd=NULL,
                           auth.mode="cas",
                           proxy.server=NULL,
                           proxy.user=NULL,
                           proxy.pwd=NULL,
                           socket.timeout=NULL,
                           user.agent=NULL) {
  #Get list of arguments
  arg.names <- formalArgs(configure.motu.client)

  #Populate it from the local enviroment
  arg.l <- lapply(arg.names,function(n) get0(n))
  names(arg.l) <- arg.names

  #Drop NULL values from list
  null.items <- sapply(arg.l,is.null)
  arg.l[null.items] <- NULL

  #Write values to options
  motu.args <- as.list(getOption("motu"))
  for(n in names(arg.l)){
    motu.args[[n]] <- arg.l[[n]]
  }
  options("motu"=motu.args)

  #Build command
  return("OK")
}

#' Specify Product
#'
#' Sets the parameters to specify the product to be downloaded
#'
#' @seealso Details and documentation on the Motu client, including releases, can be found on the associated GitHub
#' site, \url{https://github.com/clstoulouse/motu-client-python}
#' @name set.product
#' @export
#' @param motu the motu server to use (url)
#' @param service.id The service identifier (string)
#' @param product.id The product (data set) to download (string)
#' @param variable The variable to download (vector of strings)
set.CMEMS.product <- function(motu=NULL,
                        service.id=NULL,
                        product.id=NULL,
                        variable=NULL){
  #Get list of arguments
  arg.names <- formalArgs(set.CMEMS.product)

  #Populate it from the local enviroment
  arg.l <- lapply(arg.names,function(n) get0(n))
  names(arg.l) <- arg.names

  #Drop NULL values from list
  null.items <- sapply(arg.l,is.null)
  arg.l[null.items] <- NULL

  #Write values to options
  motu.args <- as.list(getOption("motu"))
  for(n in names(arg.l)){
    motu.args[[n]] <- arg.l[[n]]
  }
  options("motu"=motu.args)

  #Build command
  return("OK")
}

#' @export
#' @rdname set.product
#' @param script A text string provided by the CMEMS website
#' @details The CMEMS website has subsetting functionality that can be used as a template generate a download script for use with the motu
#' client (usually accessed by clicking on the "View Script" button when preparing to download). The function \code{set.product.from.script}
#' saves the hard work of having to figure out the parameters for use with the \code{RCMEMS} package by parsing this command and
#' extracting the relevant information - are then added to the local configuration using the \code{set.CMEMS.product} command.
set.product.from.script <- function(script){
  #Extract elements
  extract.arg <- function(arg,x) {gsub(paste("^.*?",arg," (.*?) .*$",sep=""),"\\1",x) }
  argl <- list()
  argl$motu <- extract.arg("-m",script)
  argl$service.id <- extract.arg("-s",script)
  argl$product.id <- extract.arg("-d",script)
  argl$variable <- extract.arg("-v",script)

  #Set the options
  do.call(set.CMEMS.product,argl)

  return(argl)
}


#' CMEMS Interface
#'
#' Provides R interfaces to the Python-based Motu client developed to provide access to CMEMS data. Two interfaces are
#' provided - the \code{CMEMS} interface is a simple interface taking advantage of R classes, while the \code{CMEMS.advanced}
#' interface provides full functionality of the motu client.
#'
#' @seealso Details and documentation on the Motu client, including releases, can be found on the associated GitHub
#' site, \url{https://github.com/clstoulouse/motu-client-python}
#' @name CMEMS
#' @export
#' @param ROI An extent object from the raster package specifying the region of interest to subset
#' @param date.range The maximum and minimum dates (vector of length two of class "Date")
#' @param depth.range The maximum and minimum depths specificed as a vector of length 2 (float in the interval [0 ; 2e31 ] -
#' does not accept 'Surface' as an argument)
#' @param ... Arguments to be passed on further to the \code{CMEMS.advanced} function. Overwrites any arguments automatically generated
#' in \code{CMEMS} function.
#' @param date.min The min date with optional hour resolution (string following format YYYY-MM-DD [HH:MM:SS])
#' @param date.max The max date with optional hour resolution (string following format YYYY-MM-DD [HH:MM:SS ])
#' @param latitude.min The min latitude (float in the interval [-90 ; 90 ])
#' @param latitude.max The max latitude (float in the interval [-90 ; 90 ])
#' @param longitude.min The min longitude (float in the interval [-180 ; 180 ])
#' @param longitude.max The max longitude (float in the interval [-180 ; 180 ])
#' @param depth.min The min depth (float in the interval [0 ; 2e31 ] or string 'Surface')
#' @param depth.max The max depth (float in the interval [0 ; 2e31 ] or string 'Surface')
#' @param sync.mode Sets the download mode to synchronous (not recommended)
#' @param describe.product It allows to have all updated information on a dataset. Output is in XML format
#' @param out.dir The output dir (string)
#' @param out.name The output file name (string)
#' @param block.size The block used to download file (integer expressing bytes)
#' @param socket.timeout Set a timeout on blocking socket operations (float expressing seconds)
#' @param user.agent Set the identification string (user-agent) for HTTP requests. By default this value is 'Python-urllib/x.x' (where x.x is the version of the python interpreter)
#' @param debug Allows debugging of the motu client command - builds the command without running it (logical)
#' @return If debug is TRUE, returns the full command to the motu client, ready to be run (via \code{system()}) or checked manually. If
#' debug is FALSE (the default), runs the command and returns the error code associated with the script.
CMEMS <- function(ROI,
                  date.range,
                  out.path,
                  depth.range=NULL,...) {
  require(raster)
  #Build spatial ROI arguments
  ROI.args <- as.list(structure(ROI[1:4],names=c("longitude.min","longitude.max","latitude.min","latitude.max")))

  #Build date variables
  date.arg <- as.list(structure(format(range(date.range),'"%Y-%m-%d %H:%M:%S"'),names=c("date.min","date.max")))

  #Build depth variables, if supplied
  if(is.null(depth.range)) {
    depth.args <- NULL
  } else if(!is.numeric(depth.range)) {
    stop("'depth.range' requires a numeric argument. To use the 'Surface' argument, please use the depth.min and
         depth.max arguments of CMEMS.advanced.")
  } else {
    depth.args <- list(depth.min=min(depth.range),depth.max=max(depth.range))
  }

  #Split out.path into separate file names and directories
  path.arg <- list(out.dir=dirname(out.path),out.name=basename(out.path))

  #Combine all argument and use the ... to allow additional argument or overwriting
  arg.l <- c(ROI.args,date.arg,depth.args,path.arg)
  dot.l <- list(...)
  arg.l[names(dot.l)] <- dot.l

  #Do the call
  rtn <- do.call(CMEMS.advanced,arg.l)

  return(rtn)
}

#' @export
#' @rdname CMEMS
CMEMS.advanced <- function(out.dir=NULL,
                  out.name=NULL,
                  date.min=NULL,
                  date.max=NULL,
                  latitude.min=NULL,
                  latitude.max=NULL,
                  longitude.min=NULL,
                  longitude.max=NULL,
                  depth.min=NULL,
                  depth.max=NULL,
                  sync.mode=NULL,
                  describe.product=NULL,
                  block.size=NULL,
                  socket.timeout=NULL,
                  user.agent=NULL,
                  debug=FALSE) {
  #First check for a valid configuration

  #Setup the core command
  opts <- getOption("motu")
  cmd <- paste(opts$python, opts$script)

  #Extract the rest of the options to be build into a command
  opts.rest <- opts[-which(names(opts) %in% c("python","script","messages"))]

  #Now take the rest of the configurations from this call and drop non-used arguments
  this.arg.names <- formalArgs(CMEMS.advanced)
  this.arg.names <- subset(this.arg.names,!(this.arg.names %in% c("debug")))

  #Populate it from the local enviroment
  this.argl <- lapply(this.arg.names,function(n) get0(n))
  names(this.argl) <- this.arg.names

  #Drop NULL values from list
  null.items <- sapply(this.argl,is.null)
  this.argl[null.items] <- NULL

  #Compile into a single command
  cmd.argsl <- c(opts.rest,this.argl)
  names(cmd.argsl) <- gsub("\\.","-",names(cmd.argsl))
  cmd.str <- sprintf("--%s=%s",names(cmd.argsl),cmd.argsl)
  cmd <- paste(cmd,paste(cmd.str,collapse=" "))

  #Run it
  if(!debug) {
    err.code <- system(cmd)
    return(err.code)
  } else{
    return(cmd)
  }
}


# library(raster)
# configure.motu.client(script="~/Documents/common_data/motu-client-python/motu-client.py",
#                user = "mpayne",pwd = "hDny820n")
# set.CMEMS.product(motu="http://data.ncof.co.uk/mis-gateway-servlet/Motu",
#            service.id = "http://purl.org/myocean/ontology/service/database#SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001",
#            product.id = "METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2",
#            variable = "analysed_sst")
# set.product.from.script('path_to_your_python_bin_directory/python path_to_your_motu_python_script_directory/motu-client.py -u your_user(1) -p your_password(1) -m http://cmems.isac.cnr.it/mis-gateway-servlet/Motu -s SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001-TDS -d METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2 -x -179.97500610352 -X 179.97500610352 -y -89.974998474121 -Y 89.974998474121 -t "2007-01-01 12:00:00" -T "2017-01-04 12:00:00" -v analysed_sst -o your_output_directory(1) -f your_output_file_name(1) --proxy-server your_proxy_server_url:your_proxy_port_number(2) --proxy-user your_proxy_user_login(3) --proxy-pwd your_proxy_user_password(3) ')
# CMEMS(ROI = extent(8,13,55,59),date.range = c(ISOdate(2015,08,01),ISOdate(2015,08,10)),
#       "test.nc")

