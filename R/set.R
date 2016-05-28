#' @include styles.R

NULL

#' Attempt to Compute Console Height in Text Lines
#'
#' Returns the value of the \code{LINES} system variable if it is reasonable,
#' 48 otherwise.
#'
#' @export
#' @return integer(1L)

console_lines <- function() {
  LINES <- as.integer(Sys.getenv("LINES"))
  if(length(lines) == 1L && !is.na(LINES) && LINES > 0L) LINES else 48L
}
#' Configure Automatic Context Calculation
#'
#' Helper functions to help define parameters for selecting an appropriate
#' \code{context} value.
#'
#' @export
#' @param min integer(1L), positive, set to zero to allow any context
#' @param max integer(1L), set to negative to allow any context
#' @return S4 object containing configuration parameters, for use as the
#'   \code{context} or parameter value in \code{\link{diff_obj}} and related
#'   functions

auto_context <- function(
  min=getOption("diffobj.context.auto.min"),
  max=getOption("diffobj.context.auto.max")
){
  if(!is.int.1L(min) || min < 0L)
    stop("Argument `min` must be integer(1L) and greater than zero")
  if(!is.int.1L(max))
    stop("Argument `max` must be integer(1L) and not NA")
  new("AutoContext", min=as.integer(min), max=as.integer(max))
}
#' Control Under What Circumstances Output is Displayed Through Pager
#'
#' Pager is invoked via \code{\link{file.show}}.  This function is intended for
#' use with the \code{pager} parameter for \code{\link{etc}}.
#'
#' @export
#' @param mode character(1L) one of \itemize{
#'   \item threshold: use pager if output has more lines than \code{threshold}
#'   \item always: always use pager
#'   \item never: never use pager
#' }
#' @param threshold integer(1L) if in \code{mode} "threshold", number of lines
#'   of output that triggers the use of the pager; negative values lead to
#'   using \code{\link{console_lines}} + 1
#' @param less.flags character(1L), what flags to set with the \code{LESS}
#'   system environment variable.  This is only relevant if your system is
#'   configured to use \code{less} as the pager.  You should only provide the
#'   flag letters (e.g. \code{"RX"}, not \code{"-RX"}).  Defaults to \code{"R"}
#'   if it appears that \code{less} is the system pager and the \code{use.ansi}
#'   setting is TRUE.  The system variable is only modified for the duration of
#'   the evaluation and is reset / unset afterwards.
#' @return S4 object for use as the \code{pager} parameter to
#'   \code{link{etc}}

pager_settings <- function(
  mode=getOption("diffobj.pager.mode"),
  threshold=getOption("diffobj.pager.threshold"),
  less.flags=getOption("diffobj.less.flags")
) {
  if(!is.pager_mode(mode)) stop("Argument `mode` is not a valid pager mode")
  if(!is.less_flags(less.flags)) stop("Argument `less.flags` is not valid")
  if(!is.int.1L(threshold)) stop(
    "Argument `threshold` should be integer(1L) and not NA"
  )
  new("Pager", mode=mode, threshold=threshold, less.flags=less.flags)
}
#' Check Whether System has less as Pager
#'
#' Checks system \code{PAGER} variable and that \code{PAGER_PATH} is pointed
#' at \dQuote{R_HOME/bin/pager}.  This is an approximation and may return
#' false positives or negatives depending on your system.
#'
#' @return TRUE or FALSE
#' @export

pager_is_less <- function() {
  PAGER <- Sys.getenv("PAGER")
  PAGER_PATH <- getOption("pager")
  R_HOME <- Sys.getenv("R_HOME")
  isTRUE(grepl("/less$", PAGER)) &&
    identical(PAGER_PATH, file.path(R_HOME, "bin", "pager"))
}
# Changes the LESS system variable to make it compatible with ANSI escape
# sequences
#
# flags is supposed to be character(1L) in form "XVF" or some such
#
# Returns the previous value of the variable, NA if it was not set

set_less_var <- function(flags) {
  LESS <- Sys.getenv("LESS", unset=NA)
  LESS.new <- NA
  if(is.character(LESS) && length(LESS) == 1L) {
    if(isTRUE(grepl("^\\s*$", LESS)) || is.na(LESS) || !nzchar(LESS)) {
      LESS.new <- sprintf("-%s", flags)
    } else if(
      isTRUE(grepl("^\\s*-[[:alpha:]]++(\\s+-[[:alpha:]]+)\\s*$", LESS))
    ) {
      LESS.new <-
        sub("\\s*(-[[:alpha:]]+)\\s*$", sprintf("\\1X%s", flags), LESS)
    }
  }
  if(!is.na(LESS.new)) Sys.setenv(LESS=LESS.new) else
    warning("Unable to set `LESS` system variable")
  LESS
}
reset_less_var <- function(LESS.old) {
  if(is.na(LESS.old)) {
    Sys.unsetenv("LESS")
  } else Sys.setenv(LESS=LESS.old)
}
