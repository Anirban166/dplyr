test.list <- atime::atime_test_list(
  pkg.edit.fun = function(old.Package, new.Package, sha, new.pkg.path) {
    pkg_find_replace <- function(glob, FIND, REPLACE) {
      atime::glob_find_replace(file.path(new.pkg.path, glob), FIND, REPLACE)
    }
    Package_regex <- gsub(".", "_?", old.Package, fixed = TRUE)
    Package_ <- gsub(".", "_", old.Package, fixed = TRUE)
    new.Package_ <- paste0(Package_, "_", sha)
    pkg_find_replace(
      "DESCRIPTION",
      paste0("Package:\\s+", old.Package),
      paste("Package:", new.Package))
    pkg_find_replace(
      file.path("src", "Makevars.*in"),
      Package_regex,
      new.Package_)
    pkg_find_replace(
      file.path("R", "onLoad.R"),
      Package_regex,
      new.Package_)
    pkg_find_replace(
      file.path("R", "onLoad.R"),
      sprintf('packageVersion\\("%s"\\)', old.Package),
      sprintf('packageVersion\\("%s"\\)', new.Package))
    pkg_find_replace(
      file.path("src", "init.c"),
      paste0("R_init_", Package_regex),
      paste0("R_init_", gsub("[.]", "_", new.Package_)))
    pkg_find_replace(
      "NAMESPACE",
      sprintf('useDynLib\\("?%s"?', Package_regex),
      paste0('useDynLib(', new.Package_))
  },

  "." = atime::atime_test(
    N = 10^seq(3, 8),
    setup = {
      data <- dplyr:::tibble(id1 = 1:100) %>% 
              dplyr:::crossing(id2 = 1:100) %>% 
              dplyr:::crossing(obs = 1:2) %>% 
              dplyr:::mutate(value = runif(n()))
      summarise_example <- function() {
        data %>% 
        dplyr:::group_by(id1, id2) %>% 
        dplyr:::summarise(dplyr:::across(value, mean, na.rm = T), .groups = "drop")
      }
    },
    expr = summarise_example(),
    Before = "7a968663bdff9f02bf2b410f3a92ed0fbd576dba", # v1.0.5 https://github.com/tidyverse/dplyr/commit/7a968663bdff9f02bf2b410f3a92ed0fbd576dba
    Regression = "22def186ab018417574d458dd987b2cf0bf66332", # v1.0.6 https://github.com/tidyverse/dplyr/commit/22def186ab018417574d458dd987b2cf0bf66332
    # As reported in https://github.com/tidyverse/dplyr/issues/6190#issuecomment-1100616836.
)
