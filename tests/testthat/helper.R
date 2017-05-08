library(magrittr)

expect_equal_to_ggplot_reference <- function(object, file, info = NULL) {

  if (!file.exists(file)) {
    saveRDS(object, file)
    succeed()
  }
  else {
    reference <- readRDS(file)
    object <- object[names(object)[names(object) != "plot_env"]]
    reference <- reference[names(reference)[names(reference) != "plot_env"]]
    comp <- compare(object, reference)
    expect(comp$equal, sprintf("Not equal.\n%s", comp$message), info = info)
  }
  invisible(object)
}
