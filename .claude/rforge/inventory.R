files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
inv <- list()
for (f in files) {
  src <- readLines(f, warn = FALSE)
  n <- length(src)
  # Scan for lines that START with "name <-" or "name ="
  # And check if the next 1-3 lines contain "function"
  for (i in seq_len(n)) {
    # Pattern 1: name <- function(  on same line
    m1 <- regmatches(src[i], regexec("^([a-zA-Z_.][a-zA-Z0-9_.]*)[[:space:]]*(<-|=)[[:space:]]*function", src[i]))[[1]]
    name <- NULL
    line_start <- NA
    if (length(m1) >= 2 && nchar(m1[2])) {
      name <- m1[2]
      line_start <- i
    } else {
      # Pattern 2: name <- (end of line), followed by function( on subsequent line
      m2 <- regmatches(src[i], regexec("^([a-zA-Z_.][a-zA-Z0-9_.]*)[[:space:]]*(<-|=)[[:space:]]*$", src[i]))[[1]]
      if (length(m2) >= 2 && nchar(m2[2])) {
        # Look ahead 1-3 lines for "function"
        for (lookahead in 1:3) {
          if (i + lookahead > n) break
          nxt <- src[i + lookahead]
          if (grepl("^[[:space:]]*function[[:space:]]*\\(", nxt)) {
            name <- m2[2]
            line_start <- i
            break
          }
          # Allow blank lines between
          if (!grepl("^[[:space:]]*$", nxt)) break
        }
      }
    }
    if (is.null(name)) next
    # Find end of function by tracking braces, starting from line_start
    brace_depth <- 0
    end_offset <- NA
    found_open <- FALSE
    for (j in seq(line_start, n)) {
      chars <- strsplit(src[j], "")[[1]]
      in_str <- FALSE
      str_char <- ""
      k <- 1
      while (k <= length(chars)) {
        ch <- chars[k]
        if (in_str) {
          if (ch == "\\") { k <- k + 2; next }
          if (ch == str_char) in_str <- FALSE
        } else if (ch == "\"" || ch == "'") {
          in_str <- TRUE; str_char <- ch
        } else if (ch == "#") { break
        } else if (ch == "{") { brace_depth <- brace_depth + 1; found_open <- TRUE
        } else if (ch == "}") { brace_depth <- brace_depth - 1
          if (found_open && brace_depth == 0) { end_offset <- j - line_start; break }
        }
        k <- k + 1
      }
      if (!is.na(end_offset)) break
    }
    # For functions without {} (single expression): end at the line itself
    if (is.na(end_offset)) end_offset <- 0
    inv[[length(inv) + 1]] <- list(
      name = name,
      file = basename(f),
      line_start = line_start,
      line_end = line_start + end_offset,
      n_lines = end_offset + 1
    )
  }
}
df <- do.call(rbind, lapply(inv, function(x) data.frame(
  name = x$name, file = x$file, line_start = x$line_start,
  line_end = x$line_end, n_lines = x$n_lines, stringsAsFactors = FALSE
)))
df <- df[!duplicated(df$name), ]
cat("FUNCTION COUNT:", nrow(df), "\n")
cat("\nBY FILE:\n")
tab <- sort(table(df$file), decreasing = TRUE)
print(tab)
cat("\nLINE DISTRIBUTION:\n")
cat("  STATIC (<30L):      ", sum(df$n_lines < 30), "\n")
cat("  THIN (30-80L):      ", sum(df$n_lines >= 30 & df$n_lines < 80), "\n")
cat("  REAL_LOGIC (>=80L): ", sum(df$n_lines >= 80), "\n")
cat("\nLARGEST 15:\n")
print(head(df[order(-df$n_lines), ], 15))
write.csv(df, ".claude/rforge/functions.csv", row.names = FALSE)
