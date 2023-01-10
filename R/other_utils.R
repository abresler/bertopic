.pad_zeros <-
  function(x, number_zeros = 4) {
    zeros <-
      max(0, number_zeros - nchar(x))

    if (zeros > 0) {
      count_zeros <-
        rep(0 , zeros) |> str_c(collapse = "")
    }

    if (zeros == 0) {
      count_zeros <- ""
    }

    glue::glue("{count_zeros}{x}") |> as.character()

  }


.pz <-
  function(x, number_zeros = 4) {
    x |> map_chr(list(function(var) {
      .pad_zeros(x = var, number_zeros = 4)
    }))
  }


.bf <-
  function(paths = NULL) {
    if (length(paths) == 0) {
      return(invisible())
    }
    paths %>%
      walk(function(x){
        .build_folder(path = x)
      })
  }
