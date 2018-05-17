#' Write Instructions File
#'
#' @param ins.file Path to the instructions file. If the file already exists,
#'   it will be overwritten.
#' @param n Number of observation rows to write.
#'
#' @export
write_instructions = function(ins.file, n) {
  unlink(ins.file)
  all.lines = c("pif #", "l1",
    sprintf("l1 [pobs%d]7:15 [nobs%d]17:24", 1:n, 1:n)
  )
  writeLines(all.lines, ins.file)
  invisible(NULL)
}
