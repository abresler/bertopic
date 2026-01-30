# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

R port of Python's [BERTopic](https://maartengr.github.io/BERTopic/index.html) library with integrations for KeyBERT, PolyFuzz, Top2Vec, and other NLP/ML modules via reticulate. The package wraps Python objects directly, providing R-friendly tibble outputs through munging functions.

## Build/Test Commands

```bash
# Documentation (regenerates NAMESPACE and man/)
Rscript -e "devtools::document()"

# Install locally
Rscript -e "devtools::install()"

# Run tests
Rscript -e "devtools::test()"

# Full package check
Rscript -e "devtools::check()"
```

## Architecture

### Core Pattern: Python Module Wrapping

All Python imports handled via reticulate. The package preserves Python objects as-is, with R-friendly extraction via `tbl_*` and `extract_*` functions.

```r
# Import Python modules (sets up environment)
import_bertopic(assign_to_environment = TRUE, numba_threads = 1)

# Create model (returns Python object)
tm <- bert_topic(
  embedding_model = sentence_transformer("all-MiniLM-L6-v2"),
  vectorizer_model = sklearn_vectorizer(ngram_range = list(1L, 2L)),
  min_topic_size = 10L
)

# Fit/transform (Python operations)
topics <- bert_fit_transform(tm, documents = my_docs)

# Extract to tibble (R-friendly output)
topic_info <- bert_topic_info(tm)
```

### Module Organization

| Module | Purpose |
|--------|---------|
| **bertopic.R** | Core BERTopic API: `bert_topic()`, `bert_fit_transform()`, `bert_topic_info()`, `bert_reduce_outliers()` |
| **utils.R** | Data munging: `tbl_bert_*()`, `extract_*()`, `munge_*()` functions for tibble conversion |
| **keybert.R** | KeyBERT keyword extraction: `keybert_model()`, `keybert_keywords()` |
| **polyfuzz.R** | String matching: `polyfuzz_model()`, `polyfuzz_match()`, `polyfuzz_tfidf()` |
| **representation_models.R** | Topic representations: `zeroshot_representation()`, `open_ai_representation()`, `keybert_inspired_representation()` |
| **top2vec.R** | Top2Vec integration: `top2vec_model()`, `top2vec_document_topics()` |
| **base_modules.R** | Python imports: `import_bertopic()`, `import_sklearn()`, `import_sentence_transformers()` |
| **embeddings.R** | Embedding models: `sentence_transformer()`, `flair_embeddings()` |

### Function Naming Conventions

| Prefix | Purpose | Returns |
|--------|---------|---------|
| `bert_*` | BERTopic operations | Python objects or tibbles |
| `import_*` | Python module imports | Python module reference |
| `tbl_*` | Tibble conversion/extraction | tibble |
| `extract_*` | Parse Python output | tibble or list |
| `munge_*` | Complex output parsing | tibble |
| `*_representation()` | Representation strategies | Python representation object |
| `keybert_*`, `polyfuzz_*`, `top2vec_*` | Module-specific wrappers | varies |

### Key Patterns

**Integer Specification**: Use `as.integer()` or `L` suffix for Python compatibility:
```r
bert_topic(min_topic_size = 10L, nr_topics = 20L)
```

**Vectorizer Options**:
```r
# Keyphrase-based (POS-aware)
keyphrase_vectorizer(pos_pattern = "<J.*>*<N.*>+")

# Sklearn CountVectorizer
sklearn_vectorizer(ngram_range = list(1L, 2L))
```

**Model Persistence**:
```r
bert_save(model = tm, path = "path/to/model")
tm <- bert_load(path = "path/to/model")
```

**Parallel Processing**: Functions use `furrr::future_map()` internally. Control via `workers` parameter or `numba_threads` for numpy operations.

## Dependencies

Core: reticulate (Python bridge), dplyr, purrr, tibble, stringr, glue, janitor, tidyr, furrr, future, parallelly

Python requirements: bertopic, sentence-transformers, umap-learn, hdbscan, sklearn, and optionally keybert, polyfuzz, top2vec depending on usage.

## Code Style

- 2-space indentation, `<-` for assignment
- `%>%` pipes (magrittr)
- Internal helpers use `.dot_prefix()` (never exported)
- All data frame outputs are tibbles
- Roxygen2 documentation required

## Examples

See `examples/` directory:
- `intro.R` - Complete BERTopic workflow
- `partial_fit_example.R` - Incremental learning
- `polyfuzz_docs.R` - String matching
- `kb_test.R` - KeyBERT usage
