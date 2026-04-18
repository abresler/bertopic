# RFORGE Consolidated Proposals — bertopic

**Total findings**: 43 across 4 batches (13 CRITICAL, 17 HIGH, 10 MEDIUM, 2 LOW)

**REJECTED**: C006 (topic_number off-by-one) — false positive (`1:5 - 1 = 0..4` already correct)

**VERIFIED CRITICAL (9) — auto-fix** per user mandate:

| # | ID | File | Line | Issue | Fix |
|---|----|------|------|-------|-----|
| 1 | C003 | representation_models.R | 503-525 | Hardcoded OpenAI key in dead anon function | DELETE entire block |
| 2 | C001 | representation_models.R | 298 | `cleint` typo → undefined var | Rename to `client` |
| 3 | A002 | base_modules.R | 88 | `nltk_cobjorpus` typo → NameError on assign=TRUE | Rename to `obj` |
| 4 | D001 | utils-internal.R | 88 | `aspect_slug <- aspect_slug` self-assign on empty branch | `aspect_slug <- aspect_num_slug` |
| 5 | B001 | keybert.R | 781 | `model = obj` clobbers user's model param | `model = model` |
| 6 | A001 | bertopic.R | 1167-1169 | Trailing commas in online_count_vectorizer → params dropped silently | Pass actual values |
| 7 | A003 | bertopic.R | 660 | `as.integer(NULL)` → `integer(0)` not `NULL`; Python sees `[]` not `None` | Guarded conversion |
| 8 | B002 | keyphrase.R | 79 | Same NULL coercion bug | Guarded conversion |
| 9 | B003 | keyphrase.R | 163 | Same NULL coercion bug | Guarded conversion |

**VERIFIED HIGH (14) — auto-fix**:

| # | ID | File | Line | Issue | Fix |
|---|----|------|------|-------|-----|
| 10 | A006 | bertopic.R | 442 | `purrr::flatten_df` deprecated in 1.0 | `list_rbind()` |
| 11 | A007 | bertopic.R | 840 | Same | `list_rbind()` |
| 12 | A008 | bertopic.R | 877 | Same | `list_rbind()` |
| 13 | A009 | bertopic.R | 890 | `flatten_chr` deprecated | `list_c()` |
| 14 | BUILD1 | bert-io.R | 136 | `flatten_chr` deprecated | `list_c()` |
| 15 | BUILD2 | utils-internal.R | 65 | `flatten_chr` deprecated | `list_c()` |
| 16 | BUILD3 | utils-internal.R | 306 | `flatten_dbl` deprecated | `list_c()` |
| 17 | BUILD4 | other_utils.R | 50 | `flatten_chr` deprecated | `list_c()` |
| 18 | C005 | polyfuzz.R | 222 | `tidyr::unnest()` no cols arg | `unnest(cols = everything())` |
| 19 | C004 | bert-analysis.R | 381-393 | distinct_matches dedup is no-op | `slice_head(n=1)` |
| 20 | C002 | representation_models.R | 289 | Weak API key validation | `nzchar()` + `rlang::abort` |
| 21 | D002/D003 | bert-io.R | 35, 73 | `setwd()` without `on.exit()` rollback | `on.exit(setwd(oldwd))` |
| 22 | D004 | top2vec.R | 139 | `as.integer(max_num_chunks)` when NULL | Guarded conversion |
| 23 | D005 | bert-documents.R | 325 | `names(docs)` silent failure on unnamed | Check + abort |
| 24 | D006 | utils-internal.R | 263 | `number_word` column not created | Add `mutate(number_word = row_number(), .by = ...)` |
| 25 | D007 | bert-topic-modify.R | 108-110 | NA propagation in `is_reduced_topic` | `coalesce()` after comparison |

**MEDIUM/LOW (deferred to user approval after CRITICAL+HIGH pass)**:
- Dead statement pattern `! 'X' %>% exists() & ...` (20+ occurrences)
- Gather/one_of modernization
- Additional defensive programming (nrow checks)
