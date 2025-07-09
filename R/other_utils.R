# utility_funcitons -------------------------------------------------------


.pad_zeros <-
  function(x, number_zeros = 4) {
    zeros <-
      max(0, number_zeros - nchar(x))

    if (zeros > 0) {
      count_zeros <-
        rep(0, zeros) |> str_c(collapse = "")
    }

    if (zeros == 0) {
      count_zeros <- ""
    }

    glue::glue("{count_zeros}{x}") |> as.character()
  }


.pz <-
  function(x, number_zeros = 4) {
    x |> map_chr(list(function(var) {
      .pad_zeros(x = var, number_zeros = number_zeros)
    }))
  }


.bf <-
  function(paths = NULL) {
    if (length(paths) == 0) {
      return(invisible())
    }
    .build_folder <-
      function(path = "Desktop/abresler.github.io/trelliscopes/jinkie/otr/kaute") {
        oldwd <- getwd()
        setwd("~")

        folder_exists <-
          dir.exists(paths = path)

        if (folder_exists) {
          setwd(oldwd)
          return(invisible())
        }

        parts <- path %>%
          str_split("/") %>%
          flatten_chr()

        seq_along(parts) %>%
          map(function(x) {
            if (x == 1) {
              directory <- parts[x]
              if (!dir.exists(directory)) {
                dir.create(directory)
              }
              return(invisible())
            }
            directory <- parts[1:x] %>% str_c(collapse = "/")
            if (!dir.exists(directory)) {
              dir.create(directory)
            }
            return(invisible())
          })

        setwd(oldwd)
        return(invisible())
      }

    paths %>%
      walk(function(x) {
        .build_folder(path = x)
      })
  }
