df <- read.csv(".claude/rforge/functions.csv", stringsAsFactors = FALSE)
# Read all source, extract each function body
files <- unique(df$file)
src_by_file <- lapply(files, function(f) readLines(file.path("R", f), warn = FALSE))
names(src_by_file) <- files

classify <- function(row) {
  body <- src_by_file[[row$file]][row$line_start:row$line_end]
  body_txt <- paste(body, collapse = "\n")
  # Remove comments
  body_clean <- gsub("#[^\n]*", "", body_txt)
  # Signals
  has_network <- grepl("fromJSON|read_html|GET\\(|POST\\(|httr::|httr2::|curl|download\\.file|fread.*http|read_csv.*http|read_excel.*http", body_clean)
  has_file_io <- grepl("readRDS|saveRDS|writeLines|readLines|write\\.csv|read\\.csv|write_csv|read_csv|arrow::|jsonlite::write", body_clean)
  has_reticulate <- grepl("reticulate::|\\$py_|import_|py_[a-z]", body_clean)
  has_python_call <- grepl("\\$[a-zA-Z_]+\\(|py\\$|reticulate::|r_to_py", body_clean)
  has_logic <- grepl("mutate|filter|map|walk|for\\s*\\(|while|tryCatch|if[[:space:]]*\\(|purrr|future_map|case_when", body_clean)
  has_tbl_munging <- grepl("tibble\\(|as_tibble|bind_rows|pivot_|janitor|group_by|summarise|summarize", body_clean)
  # Compute logic density (non-trivial constructs per line)
  logic_count <- lengths(regmatches(body_clean, gregexpr("mutate|filter|map|walk|for\\s*\\(|while|tryCatch|if[[:space:]]*\\(|purrr::|future_map|case_when|bind_rows|group_by|summari[sz]e|pivot_|unnest|nest", body_clean)))

  n <- row$n_lines
  # Classification rules
  if (n < 10 && !has_logic && !has_network) return(list(class = "STATIC", rationale = "Short, no logic"))
  if (n < 30 && has_reticulate && !has_logic && !has_network) return(list(class = "THIN_WRAPPER", rationale = "Thin reticulate wrapper"))
  if (n < 30 && !has_logic && !has_network) return(list(class = "STATIC", rationale = "<30L, no logic, no network"))
  if (has_network) return(list(class = "REAL_LOGIC", rationale = "Has network I/O"))
  if (has_file_io) return(list(class = "REAL_LOGIC", rationale = "Has file I/O"))
  if (has_logic && has_tbl_munging && logic_count >= 3) return(list(class = "REAL_LOGIC", rationale = sprintf("Tibble munging + logic (%d constructs)", logic_count)))
  if (has_reticulate && n < 50 && logic_count <= 2) return(list(class = "THIN_WRAPPER", rationale = "Reticulate pass-through"))
  if (n >= 80 && logic_count >= 2) return(list(class = "REAL_LOGIC", rationale = sprintf("Long fn with logic (%dL, %d constructs)", n, logic_count)))
  if (n >= 30 && logic_count >= 2) return(list(class = "THIN_WRAPPER", rationale = sprintf("Mid-size (%dL, %d constructs)", n, logic_count)))
  return(list(class = "STATIC", rationale = sprintf("No clear logic signals (%dL, %d)", n, logic_count)))
}

df$class <- NA_character_
df$rationale <- NA_character_
for (i in seq_len(nrow(df))) {
  c <- classify(df[i, ])
  df$class[i] <- c$class
  df$rationale[i] <- c$rationale
}
cat("\n=== TRIAGE RESULT ===\n")
print(table(df$class))
cat("\nBY CLASS + FILE:\n")
print(table(df$class, df$file))

write.csv(df, ".claude/rforge/triage.csv", row.names = FALSE)

cat("\n=== REAL_LOGIC FUNCTIONS ===\n")
real <- df[df$class == "REAL_LOGIC", c("name", "file", "n_lines", "rationale")]
real <- real[order(-real$n_lines), ]
print(real, row.names = FALSE)

cat("\n=== SAMPLE THIN_WRAPPERS (first 10) ===\n")
thin <- df[df$class == "THIN_WRAPPER", c("name", "file", "n_lines")]
print(head(thin[order(-thin$n_lines), ], 10), row.names = FALSE)
