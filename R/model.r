#' Predator Prey Model
#'
#' Lokta-Volterra predator-prey model.
#'
predpreymod <- function(t, y, parms) {
  with(as.list(c(y, parms)), {
    dpdt <- (a * n0 * p0) / (v + n0) - c * p0
    dndt <- r * n0 * (1 - n0 / k) - (s * n0 * p0) / (v + n0)
    return(list(c(dndt, dpdt)))
  })
}

#' Run Model
#'
#' Run the Lokta-Volterra predator-prey model.
#' @param params List of model parameters.
#' @param control List of model controls.
#' @return A 3-column matrix containing the predator and prey populations 
#'   for each timestep.
#'
#' @export
run_model = function(params, control, param.file) {
  if (!missing(param.file)) {
    inyaml = yaml::read_yaml(param.file)
    params = inyaml$params
    control = inyaml$control
  }
  if(missing(params) | missing(control))
    stop('both "params" or "control" must be specified, or a YAML file.')
  times = seq(0, control$tmax, by = control$timeStep)
  deSolve::ode(c(n0 = params$n0, p0 = params$p0), times, predpreymod, params)
}

plot_model = function(d) {
  par(las = 1, bty = "l")
  matplot(d[, 1], d[, -1], type = 'l',
    xlab = "time", ylab = "population")
  invisible(NULL)
}

#' Write Model Outputs
#'
#' Write the model outputs to a file.
#'
#' @param d The model output data.
#' @param out.file Path to the output file. If the file already exists,
#'   it will be overwritten.
#'
#' @export
write_outputs = function(d, out.file) {
  unlink(out.file)
  fd = cbind(
    format(d[, 1], width = 5),
    format(d[, 2], width = 5),
    format(d[, 3], width = 5)
  )
  colnames(fd) = colnames(d)
  write.csv(fd, out.file, quote = FALSE, row.names = FALSE)
  invisible(NULL)
}
