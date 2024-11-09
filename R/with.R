
#' with
#'
#' @description
#' Similar to function decorators, but more flexible, the advantages are:
#' 1. No need to know the parameters of the original function;
#' 2. Allow free transfer of the raw expression
#' Actually, it's like the 'with' statement in Python.
#'
#' @param expr raw expression
#' @param env caller environment
#'
#' @return what expr returns
#'
#' @examples
#' a = 3
#' b = 4
#' fun = function() {
#' a = 1
#' b = 2
#' withNull(a+b)
#' }
#' fun() # 3
#' withNull(a+b) # 7
#'
#' @export
withNull = function(expr, env = environment()) {

  invisible(eval(expr, envir = env))

}

#' withSleep
#'
#' @description
#' First change the working directory to a new path, then change it back.
#'
#' @param expr raw expression
#' @param maxSec max sleep time(in seconds)
#' @param env caller environment
#'
#' @export
withSleep = function(expr, maxSec = 10, env = environment()) {

  Sys.sleep(runif(1) * maxSec)
  cat('Ready! \n')
  on.exit(cat('Finished! \n'))

  invisible(eval(expr, envir = env))


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
#'
#' @examples
#' dir.create('tmp')
#' withPath(write.csv(iris, 'iris.csv'), 'tmp')
#' list.files('tmp') # iris.csv
#'
#' @export
withPath = function(expr, path, mkdir = T, env = environment()) {

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

  invisible(eval(expr, envir = env))

}


#' withSessions
#'
#' @description
#' First open multiple threads, then close.
#'
#' @param expr raw expression
#' @param nWorker n of sessions
#' @param env caller environment
#'
#' @export
withSessions = function(expr, nWorker = 4, env = environment()) {

  if (future::nbrOfWorkers() > 1) plan('sequential')

  future::plan('multisession', workers = nWorker)
  on.exit(future::plan('sequential'))
  invisible(eval(expr, envir = env))

}

#' withMessage
#'
#' @description
#' Prompt message after the expression is executed.
#'
#' @param expr raw expression
#' @param text message
#' @param env caller environment
#'
#' @export
withMessage = function(expr, ..., sep = ' ', coloredCat = cat, env = environment()) {

  on.exit(coloredCat(..., sep = sep))
  invisible(eval(expr, envir = env))


}

#' withAssume
#'
#' @description
#' Print message after judging the expression exit status.
#'
#' @param expr raw expression
#' @param env caller environment
#'
#' @export
withAssume = function(expr, env = environment()) {

  message = ''
  coloredCat = cat_green
  exit_status = 'Success'

  on.exit({
    if (exit_status == 'Success') {
      coloredCat(exit_status, '\n')
    } else {
      coloredCat(exit_status + ', message: \n' + message, '\n')
    }
  })

  tryCatch(
    invisible(eval(expr, envir = env)),
    error = \(e) {
      message <<- e$message
      coloredCat <<- cat_red
      exit_status <<- 'Error'
      stop()
    }
  )

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
