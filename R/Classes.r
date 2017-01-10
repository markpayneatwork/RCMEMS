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
#' @slot script Path to the local install of motu - normally this would point to the motu-client.py script
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
#' @slot variable The variable to download (vector of strings)
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
                                                   variable="character"),
                         prototype = list(python="python"))

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
  cat("CMEMS client configuration options\n")
  for(i in setdiff(slotNames("CMEMS.config"),product.slots)) {
    show.slot(object,i)}

  cat("CMEMS product specification\n")
  for(i in product.slots) {
    show.slot(object,i)}

})


