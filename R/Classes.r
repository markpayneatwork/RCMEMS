#' CMEMS interface configuration parameters
#'
#' A class to store parameters used in configuring downloads from the CMEMS servers
#'
#' @seealso Details and documentation on the Motu client, including releases of the software to download, can be
#'  found on the associated GitHub site, \url{https://github.com/clstoulouse/motu-client-python}
#' @name CMEMS.config
#' @exportClass CMEMS.config
#' @export CMEMS.config
#' @slot python Path to the local python installation
#' @slot script path to local script for the motuclient. If NULL, then the client is involked via use of the
#' "-m motuclient" argument to python (under the assumption that it is already installed)
#' @slot user user name (string)
#' @slot pwd user password (string)
#' @slot auth-mode the authentication mode: [default: cas]. Use "none" for no authentication, "basic" for basic authentication,
#' cas for Central Authentication Service
#' @slot proxy-server  the proxy server (url)
#' @slot proxy-user the proxy user (string)
#' @slot proxy-pwd the proxy password (string)
#' @slot socket-timeout Set a timeout on blocking socket operations (float expressing seconds)
#' @slot user-agent Set the identification string (user-agent) for HTTP requests. By default this value is 'Python-urllib/x.x' (where x.x is the version of the python interpreter)
#' @slot sync.mode Sets the download mode to synchronous (not recommended)
#' @slot block.size The block used to download file (integer expressing bytes)
#' @slot motu the motu server to use (url)
#' @slot service.id The service identifier (string)
#' @slot product.id The product (data set) to download (string)
#' @slot variable The variable(s) to download (vector of strings)
#' @slot date.min The min date with optional hour resolution (string following format YYYY-MM-DD [HH:MM:SS])
#' @slot date.max The max date with optional hour resolution (string following format YYYY-MM-DD [HH:MM:SS ])
#' @slot latitude.min The southern limit of the spatial subset (string)
#' @slot latitude.max The northern limit of the spatial subset (string)
#' @slot longitude.min The western limit of the spatial subset (string)
#' @slot longitude.max The eastern limit of the spatial subset (string)
#' @slot depth.min The shallow limit of the spatial subset
#' @slot depth.max The deep limit of the spatial subset
#' @slot out.dir The output directory
#' @slot out.name The filename to write to

CMEMS.config <- setClass("CMEMS.config",slots=list(python="character",
                                                   script="character",
                                                   user="character",
                                                   pwd="character",
                                                   auth.mode="character",
                                                   proxy.server="character",
                                                   proxy.user="character",
                                                   proxy.pwd="character",
                                                   socket.timeout="numeric",
                                                   user.agent="character",
                                                   sync.mode="character",
                                                   block.size="character",
                                                   motu="character",
                                                   service.id="character",
                                                   product.id="character",
                                                   variable="character",
                                                   date.min="character",
                                                   date.max="character",
                                                   latitude.min="character",
                                                   latitude.max="character",
                                                   longitude.min="character",
                                                   longitude.max="character",
                                                   depth.min="character",
                                                   depth.max="character",
                                                   out.dir="character",
                                                   out.name="character"),
                         prototype = list(python="python",
                                          script=as.character(NA)))

#' Update a CMEMS.config object
#'
#' Updates a CMEMS.config object with new parameters
#'
#' @name update
#' @export
#' @param object The \code{CMEMS.config} object to serve as the basis for creating the new object
#' @param ... Slots in the new object - see documentation of \code{\link{CMEMS.config}} for more details
setMethod("update","CMEMS.config", function(object,...) {
  new("CMEMS.config",object,...)
})


#' @export
setMethod("show","CMEMS.config", function(object) {

  show.slot <- function(ob,slt) {
    obj <- slot(ob,slt)
    if(length(obj)==0) return(NULL) else {cat("- ")}
    if(class(obj) %in% c("logical","formula","character","numeric","Extent","integer","list")) {
      cat(sprintf("%-15s : ",slt))
    } else {return(NULL)}
    if(is(obj,"formula")) {
      cat(deparse(obj,width.cutoff=options()$width),"\n")
    } else if(is(obj,"Extent")){
      cat(paste(obj[],collapse=", "),"\n")
    } else if(is(obj,"numeric") & length(obj) ==1){
      cat(obj,"\n")
    } else if(is(obj,"numeric") & length(obj) >=12){
      cat(paste(range(obj),collapse="-"),"\n")
    } else if(is(obj,"numeric") & length(obj) <12){
      cat(paste(range(obj),collapse="-"),"\n")
    } else if(is(obj,"list") ){
      cat(paste(names(obj),collapse=", "),"\n")
    } else {
      cat(paste(obj,collapse=", "),"\n")
    }
  }

  product.slots <- c("motu","service.id","product.id","variable")
  subset.slots <- c("date.min","date.max","latitude.min","latitude.max",
                    "longitude.min", "longitude.max", "depth.min", "depth.max")
  output.slots <- c("out.dir","out.name")
  cat("CMEMS client configuration options\n")
  for(i in setdiff(slotNames("CMEMS.config"),c(product.slots,subset.slots,output.slots))) {
    show.slot(object,i)}

  cat("CMEMS product specification\n")
  for(i in product.slots) {
    show.slot(object,i)}

  cat("Subsetting specification\n")
  for(i in subset.slots) {
    show.slot(object,i)}

  cat("Output specification\n")
  for(i in output.slots) {
    show.slot(object,i)}


})


