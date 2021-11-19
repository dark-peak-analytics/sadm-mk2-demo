

# utility functions for survival model predicitons

.get_variable_value <- function(variable, strata, fit, data = NULL){
  res <- sapply(as.vector(strata), function(x){
    x <- unlist(strsplit(x, "=|(\\s+)?,\\s+", perl=TRUE))
    # When a factor name is the same as one of its level, index is of length 2
    index <- grep(paste0("^", variable, "$"), x)[1]
    .trim(x[index+1])
  })
  res <- as.vector(res)
  var_levels <- levels(.get_data(fit, data)[, variable])
  if(!is.null(var_levels)) res <- factor(res, levels = var_levels)
  else res <- as.factor(res)
  res
}

.get_variables <- function(strata, fit, data = NULL){
  variables <- sapply(as.vector(strata),
                      function(x){
                        x <- unlist(strsplit(x, "=|,\\s+", perl=TRUE))
                        x[seq(1, length(x), 2)]
                      })
  variables <- unique(as.vector(variables))
  variables <- intersect(variables, colnames(.get_data(fit, data) ))
  variables
}

.clean_strata <- function(strata, fit){
  is_dollar_sign <- grepl("$", as.character(strata)[1], fixed=TRUE)
  if(is_dollar_sign) {
    strata <- as.character(strata)
    data_name <- unlist(strsplit(strata[1], "$", fixed =TRUE))[1]
    strata <- gsub(paste0(data_name, "$"), "", strata, fixed=TRUE)
    strata <- as.factor(strata)
  }
  else if(!missing(fit)) strata <- factor(strata, levels = names(fit$strata))
  return(strata)
}


form.model.matrix <- function (object, newdata)
{
  mfo <- model.frame(object)
  covnames <- attr(mfo, "covnames")
  missing.covs <- unique(covnames[!covnames %in% names(newdata)])
  if (length(missing.covs) > 0) {
    missing.covs <- sprintf("\"%s\"", missing.covs)
    plural <- if (length(missing.covs) > 1)
      "s"
    else ""
    stop(sprintf("Value%s of covariate%s ", plural, plural),
         paste(missing.covs, collapse = ", "), " not supplied in \"newdata\"")
  }
  tt <- attr(mfo, "terms")
  Terms <- delete.response(tt)
  mf <- model.frame(Terms, newdata,na.action = na.omit,
                    xlev = .getXlevels(tt,mfo))
  if (!is.null(cl <- attr(Terms, "dataClasses")))
    .checkMFClasses(cl, mf)
  forms <- object$all.formulae
  mml <- vector(mode = "list", length = length(object$dlist$pars))
  names(mml) <- names(forms)
  forms[[1]] <- delete.response(terms(forms[[1]]))
  for (i in names(forms)) {
    mml[[i]] <- model.matrix(forms[[i]], mf)
  }

  ## compress model matrices
  cbind.drop.intercept <- function(...) do.call("cbind", lapply(list(...),
                                                                function(x) x[, -1, drop = FALSE]))
  X <- do.call("cbind.drop.intercept", mml)
  loc.cnames <- colnames(mml[[1]])[-1]
  anc.cnames <- unlist(mapply(function(x, y) sprintf("%s(%s)",
                                                     x, y), names(mml[-1]), lapply(mml[-1], function(x) colnames(x)[-1])))
  cnames <- c(loc.cnames, anc.cnames)
  colnames(X) <- cnames

  attr(X, "newdata") <- mf
  X
}


add.covs <- function (x, pars, beta, X, transform = FALSE)
{
  nres <- nrow(X)
  if (!is.matrix(pars))
    pars <- matrix(pars, nrow = nres, ncol = length(pars),
                   byrow = TRUE)
  if (!is.matrix(beta))
    beta <- matrix(beta, nrow = 1)
  for (j in seq(along = x$dlist$pars)) {
    covinds <- x$mx[[x$dlist$pars[j]]]
    if (length(covinds) > 0) {
      pars[, j] <- pars[, j] + beta[, covinds] %*% t(X[,covinds, drop = FALSE])
    }
    if (!transform)
      pars[, j] <- x$dlist$inv.transforms[[j]](pars[, j])
  }
  colnames(pars) <- x$dlist$pars
  pars
}


predict.flexsurvreg <- function(object, newdata, times, type = 'survival', start = NULL, ...) {
  
  if (length(times)==1)
    times <- rep(x = times, times = nrow(newdata))
  stopifnot( length(times) == nrow(newdata) )
  
  x <- object
  dat <- x$data
  Xraw <- model.frame(x)[, unique(attr(model.frame(x), "covnames.orig")), drop = FALSE]
  type <- match.arg(type, c("survival", "cumhaz", "hazard"))
  X <- form.model.matrix(object, as.data.frame(newdata))
  na_omit_idx <- attr(attr(X,'newdata'),'na.action')
  if (is.null(na_omit_idx)) na_omit_idx <- -(1:nrow(newdata))
  
  t <- times[-na_omit_idx]
  omit_idx <- which(t <= 0)
  t[omit_idx] <- median(t, na.rm = TRUE) # just a filler value
  
  if (is.null(start)) {
    start <- numeric(length(t))
  } else {
    if (length(start)==1)
      start <- rep(x = start, times = nrow(newdata))
    stopifnot( length(start) == nrow(newdata) )
    start <- start[-na_omit_idx]
  }
  
  
  fn <- switch(type, survival = function(t, start, ...) {
    ret <- (1 - x$dfns$p(t, ...))/(1 - x$dfns$p(start, ...))
    ret[t < start] <- 1
    ret
  }, hazard = function(t, start, ...) {
    ret <- x$dfns$h(t, ...) * (1 - x$dfns$p(start, ...))
    ret[t < start] <- 0
    ret
  }, cumhaz = function(t, start, ...) {
    ret <- x$dfns$H(t, ...) - x$dfns$H(start, ...)
    ret[t < start] <- 0
    ret
  })
  
  ##
  summfn2 <- fn
  args <- c(alist(t = , start = ), formals(fn))
  formals(summfn2) <- args[!duplicated(names(args))]
  body(summfn2) <- body(fn)
  fn <- summfn2
  
  ##
  fncall <- list(t, start)
  beta <- if (x$ncovs == 0) 0 else x$res[x$covpars, "est"]
  if ( (x$ncovs > 0) && (ncol(X) != length(beta)) ) {
    isare <- if (length(beta) == 1)
      "is"
    else "are"
    plural <- if (ncol(X) == 1)
      ""
    else "s"
    pluralc <- if (length(beta) == 1)
      ""
    else "s"
    stop("Supplied X has ", ncol(X), " column", plural,
         " but there ", isare, " ", length(beta), " covariate effect",
         pluralc)
  }
  
  dlist <- x$dlist
  
  basepars_mat <- add.covs(x, x$res.t[dlist$pars, "est"],
                           beta, X, transform=FALSE)
  basepars <- as.list(as.data.frame(basepars_mat))
  fncall[dlist$pars] <- basepars[dlist$pars]
  fncall[names(x$aux)] <- x$aux
  y <- do.call(fn, fncall)
  if (type == 'survival') y[omit_idx] <- 1
  else y[omit_idx] <- 0
  
  # fill for NAs
  out <- numeric(nrow(newdata))
  out[-na_omit_idx] <- y
  if (!all(na_omit_idx>0))
    out[na_omit_idx] <- NA_real_
  
  return(out)
  
  
}


predict.uncertain.flexsurvreg <- function(object, newdata, times, type = 'survival', start = NULL, ...) {
  
  if (length(times)==1)
    times <- rep(x = times, times = nrow(newdata))
  stopifnot( length(times) == nrow(newdata) )
  
  x <- object
  dat <- x$data
  Xraw <- model.frame(x)[, unique(attr(model.frame(x), "covnames.orig")), drop = FALSE]
  type <- match.arg(type, c("survival", "cumhaz", "hazard"))
  X <- form.model.matrix(object, as.data.frame(newdata))
  na_omit_idx <- attr(attr(X,'newdata'),'na.action')
  if (is.null(na_omit_idx)) na_omit_idx <- -(1:nrow(newdata))
  
  t <- times[-na_omit_idx]
  omit_idx <- which(t <= 0)
  t[omit_idx] <- median(t, na.rm = TRUE) # just a filler value
  
  if (is.null(start)) {
    start <- numeric(length(t))
  } else {
    if (length(start)==1)
      start <- rep(x = start, times = nrow(newdata))
    stopifnot( length(start) == nrow(newdata) )
    start <- start[-na_omit_idx]
  }
  
  
  fn <- switch(type, survival = function(t, start, ...) {
    ret <- (1 - x$dfns$p(t, ...))/(1 - x$dfns$p(start, ...))
    ret[t < start] <- 1
    ret
  }, hazard = function(t, start, ...) {
    ret <- x$dfns$h(t, ...) * (1 - x$dfns$p(start, ...))
    ret[t < start] <- 0
    ret
  }, cumhaz = function(t, start, ...) {
    ret <- x$dfns$H(t, ...) - x$dfns$H(start, ...)
    ret[t < start] <- 0
    ret
  })
  
  ##
  summfn2 <- fn
  args <- c(alist(t = , start = ), formals(fn))
  formals(summfn2) <- args[!duplicated(names(args))]
  body(summfn2) <- body(fn)
  fn <- summfn2
  
  ##
  fncall <- list(t, start)
  beta <- if (x$ncovs == 0) 0 else x$res[x$covpars, "est"]
  if ( (x$ncovs > 0) && (ncol(X) != length(beta)) ) {
    isare <- if (length(beta) == 1)
      "is"
    else "are"
    plural <- if (ncol(X) == 1)
      ""
    else "s"
    pluralc <- if (length(beta) == 1)
      ""
    else "s"
    stop("Supplied X has ", ncol(X), " column", plural,
         " but there ", isare, " ", length(beta), " covariate effect",
         pluralc)
  }
  
  dlist <- x$dlist
  

  ##### UNCERTAINTY INPUT HERE <------
  random_input = c()
  for(i in 1:length(x$res.t[dlist$pars, "est"])){
   random_input = c(random_input, rnorm(1, x$res.t[dlist$pars, "est"][i], x$res.t[dlist$pars, "se"][i]))
  }


  basepars_mat <- add.covs(x, random_input,
    beta, X,
    transform = FALSE
  )
  
  basepars <- as.list(as.data.frame(basepars_mat))
  fncall[dlist$pars] <- basepars[dlist$pars]
  fncall[names(x$aux)] <- x$aux
  y <- do.call(fn, fncall)
  if (type == 'survival') y[omit_idx] <- 1
  else y[omit_idx] <- 0
  
  # fill for NAs
  out <- numeric(nrow(newdata))
  out[-na_omit_idx] <- y
  if (!all(na_omit_idx>0))
    out[na_omit_idx] <- NA_real_
  
  return(out)
  
  
}



.get_data <- function(fit, data = NULL, complain = TRUE) {
  if(is.null(data)){
    if (complain)
      warning ("The `data` argument is not provided. Data will be extracted from model fit.")
    data <- eval(fit$call$data)
    if (is.null(data))
      stop("The `data` argument should be provided either to ggsurvfit or survfit.")
  }
  data
}




surv_summary = function (x, data = NULL) 
{
  res <- as.data.frame(compact(unclass(x)[c("time", "n.risk", 
                                             "n.event", "n.censor")]))
  if (inherits(x, "survfitms")) {
    surv <- 1 - x$prev
    upper <- 1 - x$upper
    lower <- 1 - x$lower
    res <- cbind(res, surv = c(surv), std.err = c(x$std.err), 
                 upper = c(upper), lower = c(lower))
    res$state <- rep(x$states, each = nrow(surv))
  }
  else {
    if (is.matrix(x$surv)) {
      ncurve <- ncol(x$surv)
      res <- data.frame(time = rep(x$time, ncurve), n.risk = rep(x$n.risk, 
                                                                 ncurve), n.event = rep(x$n.event, ncurve), n.censor = rep(x$n.censor, 
                                                                                                                           ncurve))
      res <- cbind(res, surv = .flat(x$surv), std.err = .flat(x$std.err), 
                   upper = .flat(x$upper), lower = .flat(x$lower))
      res$strata <- as.factor(rep(colnames(x$surv), each = nrow(x$surv)))
    }
    else res <- cbind(res, surv = x$surv, std.err = x$std.err, 
                      upper = x$upper, lower = x$lower)
  }
  if (!is.null(x$strata)) {
    data <- .get_data(x, data = data)
    res$strata <- rep(names(x$strata), x$strata)
    res$strata <- .clean_strata(res$strata, x)
    variables <- .get_variables(res$strata, x, data)
    for (variable in variables) res[[variable]] <- .get_variable_value(variable, 
                                                                       res$strata, x, data)
  }
  structure(res, class = c("data.frame", "surv_summary"))
  attr(res, "table") <- as.data.frame(summary(x)$table)
  res
}


.trim <- function(x){gsub("^\\s+|\\s+$", "", x)}
