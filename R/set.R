#' Generate a \code{diffobj} Settings Object
#'
#' Used for more fine grained control on the \code{diff_obj} family of
#' functions by generating a settings object to pass as the \code{etc}
#' parameter.
#'
#' @export
#' @param line.limit integer(2L) or integer(1L), if length 1 how many lines of
#'   output to show, where \code{-1} means no limit.  If length 2, the first
#'   value indicates the threshold of screen lines to begin truncating output,
#'   and the second the number of lines to truncate to, which should be fewer
#'   than the threshold.
#' @param hunk.limit integer(2L) or integer (1L), how many diff hunks to show.
#'   Behaves similarly to the \code{line.limit}.  How many hunks are in a
#'   particular diff is a function of how many differences, and also how much
#'   \code{context} is used since context can cause two hunks to bleed into
#'   each other and become one.
#' @param use.ansi TRUE or FALSE, whether to use ANSI escape sequences to color
#'   differences (TRUE by default if we detect that your terminal supports it)
#' @param ignore.white.space TRUE or FALSE, whether to consider differences in
#'   horizontal whitespace (i.e. spaces and tabs) as differences (defaults to
#'   FALSE)
#' @param convert.hz.whitespace TRUE or FALSE, whether modify input strings
#'   that contain tabs and carriage returns in such a way that they display as
#'   they would \bold{with} those characters, but without using those
#'   characters (defaults to TRUE).  If your console uses tab stops that are not
#'   eight characters apart you may specify them with \code{tab.stops}.
#' @param tab.stops integer, what tab stops to use when converting hard tabs to
#'   spaces.  If not integer will be coerced to integer (defaults to 8L).
#' @param disp.width integer(1L) number of display columns to take up; note that
#'   in \dQuote{sidebyside} mode the effective display width is half this
#'   number (defaults to \code{getOption("width")}.
#' @param frame environment the evaluation frame for the \code{print/show/str},
#'   calls, allows user to ensure correct methods are used, not used by
#'   \code{\link{diff_chr}} or \code{\link{diff_deparse}}.
#' @param max.diffs integer(1L), number of differences after which we abandon
#'   the \code{O(n^2)} diff algorithm in favor of a linear one.  Set to
#'   \code{-1L} to always stick to the original algorithm (defaults to 10000L).
#' @param max.diffs.in.hunk integer(1L), like \code{max.diffs}, but used when
#'   computing word diffs within hunks.  Used independently for each hunk.  Set
#'   to zero to turn off in-hunk word diff.
#' @param max.diffs.wrap integer(1L), like \code{max.diffs}, but used when
#'   computing word diffs on atomic vectors (defaults to 10000L), see
#'   \dQuote{Atomic Vectors} for \code{\link{diff_print}}.
#' @param tar.banner character(1L) or NULL, text to display ahead of the diff
#'   section representing the target output.  If NULL will be
#'   inferred from \code{target} and \code{current} expressions.
#' @param cur.banner character(1L) like \code{tar.banner}, but for
#'   \code{current}
#' @return a \code{diffObjSettings} S4 object for use with the
#'   \code{\link{diff_obj}} family of functions
#' @examples
#' mx1 <- mx2 <- matrix(1:80, 20)
#' mx2[sample(1:80, 5)] <- 99
#' diff_print(mx1, mx2, etc=etc(line.limit=10))

etc <- function(
  line.limit=getOption("diffobj.line.limit"),
  hunk.limit=getOption("diffobj.hunk.limit"),
  pager=pager_settings(),
  ignore.white.space=getOption("diffobj.ignore.white.space"),
  use.ansi=getOption("diffobj.use.ansi"),
  disp.width=getOption("width"),
  max.diffs=getOption("diffobj.max.diffs"),
  max.diffs.in.hunk=getOption("diffobj.max.diffs.in.hunk"),
  max.diffs.wrap=getOption("diffobj.max.diffs.wrap"),
  convert.hz.white.space=getOption("diffobj.convert.hz.white.space"),
  tab.stops=getOption("diffobj.tab.stops"),
  frame=parent.frame(),
  tar.banner=NULL, cur.banner=NULL
) {
  # Check arguments

  this.env <- environment()

  # Tab stops

  tab.stops <- as.integer(tab.stops)
  if(
    !is.integer(tab.stops) || !length(tab.stops) >= 1L || anyNA(tab.stops) ||
    !all(tab.stops > 0L)
  )
    stop(
      "Argument `tab.stops` must be integer containing at least one value and ",
      "with all values strictly positive"
    )
  # Limit vars

  hunk.limit <- check_limit(hunk.limit)
  if(!is.integer(hunk.limit)) stop(sprintf(hunk.limit, "hunk.limit", "."))
  if(!is.integer(line.limit <- check_limit(line.limit)))
    stop(
      sprintf(
        line.limit, "line.limit",
        ", or \"auto\" or the result of calling `auto_line_limit`"
    ) )
  # check T F args

  TF.vars <- c("use.ansi", "ignore.white.space", "convert.hz.white.space")
  msg.base <- "Argument `%s` must be TRUE or FALSE."
  lapply(
    TF.vars,
    function(x) if(!is.TF(this.env[[x]])) stop(sprintf(msg.base, x))
  )
  # int 1L vars

  msg.base <- "Argument `%s` must be integer(1L) and not NA."
  int1L.vars <- c(
    "disp.width", "max.diffs", "max.diffs.in.hunk", "max.diffs.wrap"
  )
  for(i in int1L.vars) this.env[[i]] <-
    if(!is.int.1L(this.env[[i]])) {
      stop(simpleError(sprintf(msg.base, i)))
    } else as.integer(this.env[[i]])

  # char or NULL vars

  chr1LorNULL.vars <- c("tar.banner", "cur.banner")
  msg.base <- "Argument `%s` must be character(1L) and not NA, or NULL"
  lapply(
    chr1LorNULL.vars,
    function(x)
      if(!is.chr.1L(this.env[[x]]) && !is.null(this.env[[x]]))
        stop(sprintf(msg.base, x))
  )
  # instantiate settings object

  set.vars <- as.list(this.env)
  do.call(
    "new",
    c(
      list("diffObjSettings"),
      set.vars[names(set.vars) %in% names(getSlots("diffObjSettings"))]
  ) )
}
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
  new("diffObjAutoContext", min=as.integer(min), max=as.integer(max))
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
  new("diffObjPager", mode=mode, threshold=threshold, less.flags=less.flags)
}
# Check whether system has less as pager; this is an approximation since we
# do not check that the pager shell script actually calls $PAGER

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
