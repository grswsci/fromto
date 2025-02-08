
# with --------------------------------------------------------------------


#' with function cluster
#'
#' @description
#' Similar to function decorators, but more flexible, the advantages are:
#' 1. No need to know the parameters of the original function;
#' 2. Allow free transfer of the raw expression
#' Actually, it's like the 'with' statement in Python.
#'
#' @param expr raw expression
#' @param env caller environment
#' @param ... Temporary variable
#'
#' @return what expr returns
#'
#' @export
withIn = function(expr, env = parent.frame(), ...) {

  permission = tolower(getOption('allowWith'))
  if (length(permission) == 0) permission = 'enable'

  if (permission == 'enable') {

    lst_param = list(...)
    env_param = list2env(lst_param, parent = env)

    invisible(eval(expr, envir = env_param))

  } else if (permission == 'disable') {

    invisible()

  } else {

    stop('Check `allowWith` option! ')

  }

}

#' withNothing
#'
#' @description
#' Do nothing, only used for testing the withIn function.
#'
#' @param expr raw expression
#' @param env caller environment
#' @param ... Temporary variable
#'
#' @return what expr returns
#'
#' @examples
#' a = 3
#' b = 4
#' fun = function() {
#' a = 1
#' b = 2
#' withNothing(a+b)
#' }
#' print(fun()) # 3
#' print(withNothing(a + b)) # 7
#' print(withNothing(a + b, a = 4, b = 5)) # 9
#'
#' @export
withNothing = function(expr, env = parent.frame(), ...) {

  expr = substitute(expr)

  withIn(expr, env, ...)

}

#' withSleep
#'
#' @description
#' First change the working directory to a new path, then change it back.
#'
#' @param expr raw expression
#' @param maxSec max sleep time(in seconds)
#' @param env caller environment
#' @param ... Temporary variable
#'
#' @export
withSleep = function(expr, maxSec = 10, env = parent.frame(), ...) {

  expr = substitute(expr)

  Sys.sleep(runif(1) * maxSec)
  cat('Ready! \n')
  on.exit(cat('Finished! \n'))

  withIn(expr, env, ...)


}

#' withPath
#'
#' @description
#' First change the working directory to a new path, then change it back.
#'
#' @param expr raw expression
#' @param path new working directory
#' @param mkdir whether to make dir when dir doesn't exist
#' @param env caller environment
#' @param ... Temporary variable
#'
#' @examples
#' dir.create('tmp')
#' withPath(write.csv(iris, 'iris.csv'), 'tmp')
#' list.files('tmp') # iris.csv
#'
#' @export
withPath = function(expr, path, mkdir = T, env = parent.frame(), ...) {

  expr = substitute(expr)

  create = F
  path_old = getwd()
  path_new = normalizePath(path, '/', F)

  if (mkdir & !dir.exists(path_new)) {
    dir.create(path_new, recursive = T)
    create = T
  }

  if (!dir.exists(path_new)) {
    stop('Path ', path_new, ' doesn\'t exist, consider using `mkdir = T`')
  }

  setwd(path_new)
  on.exit({
    setwd(path_old)
    if (length(list.files(path_new)) == 0 & create) unlink(path_new, recursive = T)
  })

  withIn(expr, env, ...)

}


#' withSessions
#'
#' @description
#' First open multiple threads, then close.
#'
#' @param expr raw expression
#' @param nWorker n of sessions
#' @param env caller environment
#' @param ... Temporary variable
#'
#' @export
withSessions = function(expr, nWorker = 4, env = parent.frame(), ...) {

  expr = substitute(expr)

  if (future::nbrOfWorkers() > 1) plan('sequential')

  future::plan('multisession', workers = nWorker)
  on.exit(future::plan('sequential'))

  withIn(expr, env, ...)

}

#' withMessage
#'
#' @description
#' Prompt message after the expression is executed.
#'
#' @param expr raw expression
#' @param text message
#' @param env caller environment
#' @param ... Temporary variable
#'
#' @export
withMessage = function(expr, text, coloredCat = cat, env = parent.frame(), ...) {

  expr = substitute(expr)

  on.exit(coloredCat(text, sep = '\n'))

  withIn(expr, env, ...)

}

#' withAssume
#'
#' @description
#' Print message after judging the expression exit status.
#'
#' @param expr raw expression
#' @param env caller environment
#' @param ... Temporary variable
#'
#' @export
withAssume = function(expr, onPass = 'Success', onError = 'Error', env = parent.frame(), ...) {

  expr = substitute(expr)

  message = ''
  coloredCat = cat_green
  exit_status = onPass

  on.exit({
    if (exit_status == onPass) {
      coloredCat(exit_status, '\n')
    } else {
      coloredCat(paste0(exit_status, ', message: \n', message), '\n')
    }
  })

  tryCatch(
    withIn(expr, env, ...),
    error = \(e) {
      message <<- e$message
      coloredCat <<- cat_red
      exit_status <<- onError
      stop()
    }
  )

}

#' withOption
#'
#' @description
#' First change the options when excuting expression, then change it back.
#'
#' @param expr raw expression
#' @param options a list. options.
#' @param env caller environment
#' @param ... Temporary variable
#'
#' @export
withOption = function(expr, options, env = parent.frame(), ...) {

  expr = substitute(expr)

  options_raw = setNames(lapply(names(options), getOption), names(options))
  do.call(base::options, options)
  on.exit(do.call(base::options, options_raw))

  lst_param = list(...)
  env_param = list2env(lst_param, parent = env)

  invisible(eval(expr, envir = env_param))

}

#' withBackground
#'
#' @description
#' Execute the expression in the background.
#'
#' @param expr raw expression
#' @param env caller environment
#'
#' @export
withBackground = function(expr) {

  invisible(
    callr::r_bg(
      \(code) eval(parse(text = code)), args = list(code = deparse(substitute(expr))),
      user_profile = T
    )
  )

}
