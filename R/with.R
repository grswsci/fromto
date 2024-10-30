
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
withNull = function(expr, env = environment()) {

  eval(expr, envir = env)

}

#' withPath
#'
#' @description
#' First change the working directory to a new path, then change it back.
#'
#' @param expr raw expression
#' @param path new working directory
#' @param env caller environment
#'
#' @examples
#' dir.create('tmp')
#' withPath(write.csv(iris, 'iris.csv'), 'tmp')
#' list.files('tmp') # iris.csv
withPath = function(expr, path, env = environment()) {

  path_old = getwd()
  path_new = path

  setwd(path_new)
  eval(expr, envir = env)
  setwd(path_old)

}

#' withSessions
#'
#' @description
#' First open multiple threads, then close.
#'
#' @param expr raw expression
#' @param nWorker n of sessions
#' @param env caller environment
withSessions = function(expr, nWorker = 4, env = environment()) {

  if (future::nbrOfWorkers() > 1) plan('sequential')

  future::plan('multisession', workers = nWorker)
  res = eval(expr, envir = env)
  future::plan('sequential')

  return(res)

}

#' withMessge
#'
#' @description
#' Prompt message after the expression is executed.
#'
#' @param expr raw expression
#' @param text message
#' @param env caller environment
withMessge = function(expr, text, env = environment()) {

  eval(expr, envir = env)
  cat(text)

}
