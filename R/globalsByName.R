#' Locates and retrieves a set of global variables by their names
#'
#' @param names A character vector of global variable names.
#'
#' @param envir The environment from where to search for globals.
#"
#' @param mustExist If TRUE, an error is thrown if the object of the
#'        identified global cannot be located.  Otherwise, the global
#'        is not returned.
#'
#' @param \ldots Not used.
#'
#' @section Special "argument" globals:
#' If `names` specifies `"..."`, `"..1"`,  `"..2"`, ..., then they
#' are interpreted as arguments `...`, `..1`, `..2`, ..., respectively.
#' If specified, then the corresponding elements in the results are
#' lists of class `DotDotDotList` comprising the value of the latter.
#' If the special argument does not exist, then the value is `NA`, and
#' the corresponding `where` attributes is `NULL`.
#'
#' @return A \link{Globals} object of named elements and an attribute
#' `where` with named elements. Both of sets have names according to
#' `names`.
#'
#' @example incl/globalsByName.R
#'
#' @export
globalsByName <- function(names, envir = parent.frame(), mustExist = TRUE,
                          ...) {
  names <- as.character(names)
  nnames <- length(names)
  
  namesOrg <- names

  debug <- isTRUE(getOption("globals.debug"))
  if (debug) {
    info <- hpaste(sprintf('"%s"', names))
    if (nnames > 1L) info <- sprintf("<%s> [n=%d]", info, nnames)
    info <- sprintf("%s, mustExist = %s", info, mustExist)
    mdebugf_push("globalsByName(%s) ...", info)
    mdebug("search from environment: %s", sQuote(envname(envir)))
    on.exit(mdebugf_pop("globalsByName(%s) ... done", info))
  }

  ## Locate and retrieve the specified globals
  idxs <- grep("^[.][.]([.]|[0-9]+)$", names)
  if (length(idxs) > 0L) {
    dotdotdots <- unique(names[idxs])
    names <- names[-idxs]
    idxs <- NULL
    if (debug) mdebugf("dotdotdots: %s", commaq(dotdotdots))
  } else {
    dotdotdots <- NULL
    if (debug) mdebug("dotdotdots: <none>")
  }

  globals <- structure(vector("list", length = nnames), names = namesOrg)
  where <- structure(vector("list", length = nnames), names = namesOrg)
  for (kk in seq_along(names)) {
    name <- names[kk]
    if (debug) mdebugf("locating #%d (%s)", kk, sQuote(name))
    env <- where(name, envir = envir, inherits = TRUE)
    if (debug) mdebugf("+ found in environment: %s", sQuote(envname(env)))
    if (!is.null(env)) {
      where[[name]] <- env
      value <- get(name, envir = env, inherits = FALSE)
      if (is.null(value)) {
        globals[name] <- list(NULL)
      } else {
        globals[[name]] <- value
      }
    } else {
      globals[name] <- list(NULL)
      where[name] <- list(NULL)
      if (mustExist) {
        stop(sprintf("Failed to locate global object in the relevant environments: %s", sQuote(name))) #nolint
      }
    }
  }

  if (length(dotdotdots) > 0L) {
    where... <- NULL
    has... <- exists("...", envir = envir, inherits = TRUE)
    if (has...) {
        where... <- where("...", envir = envir, inherits = TRUE)
    }
    
    for (name in dotdotdots) {
      where[name] <- list(where...)

      ## FIXME: If '...' in environment 'envir' specifies non-existing
      ## symbols, then we must not call list(...), list(..1), etc.,
      ## because that will produce an "object not found" error.
      ## /HB 2023-05-19
      if (has...) {
        expr <- substitute(list(arg), list(arg = as.name(name)))
        ddd <- eval(expr, envir = envir, enclos = envir)
      } else {
        ddd <- NA
      }
    
      class(ddd) <- c("DotDotDotList", class(ddd))
      globals[[name]] <- ddd
    }
  }
  stop_if_not(
    length(names(globals)) == nnames,
    all(names(globals) %in% namesOrg),
    identical(names(globals), namesOrg)
  )
  
  stop_if_not(
    is.list(where),
    length(where) == length(globals),
    all(names(where) == names(globals))
  )

  attr(globals, "where") <- where
  class(globals) <- c("Globals", class(globals))

  if (debug) {
    mdebug("Globals collected:")
    mstr(globals)
  }

  globals
} ## globalsByName()
