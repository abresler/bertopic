# bertopic Feature Roadmap
**Generated:** 2026-01-30  
**Package Version:** 0.1.2  
**Analysis Method:** PAI Algorithm + QUANTUM-COUNCIL + BAYES-DECISION

---

## Executive Summary

**TOP 5 FEATURES (by Expected Value):**

1. **bert_config() Centralization** - EV: 7.875 - Config object with basic/advanced/expert levels for progressive API disclosure
2. **bert_evaluate() Metrics Suite** - EV: 7.8 - Industry-first topic coherence, diversity, and stability evaluation for R
3. **pins Embedding Caching** - EV: 6.55 - Cache expensive embeddings to save 10+ minutes per run
4. **bertopic_quickstart()** - EV: 6.5 - One-liner workflow builder with use-case presets
5. **pkgdown Website + Vignettes** - EV: 6.5 - Comprehensive documentation to address 390 roxygen warnings

**QUICK WINS (high EV, low effort):**
- **print/autoplot Methods** - EV: 6.125, 2-3 days
- **bertopic_quickstart()** - EV: 6.5, 1 week
- **bert_explain()** - EV: 5.2, 1 week

---

## Full Feature Specifications

### FEATURE SPEC 1: bert_evaluate() - Topic Model Evaluation Suite

**Category:** 📊 EVALUATION & VALIDATION  
**Priority:** 🔴 HIGH  
**EV Score:** 7.8  
**Complexity:** M (1-2 weeks)  
**Dependencies:** None (uses model outputs)

#### DESCRIPTION

A comprehensive evaluation suite providing **industry-first** topic model quality metrics for R neural topic modeling. Enables data scientists to objectively compare models, justify hyperparameter choices, and demonstrate model quality to stakeholders.

#### USER STORY

As a **data scientist**, I want **quantitative evaluation metrics** so that **I can objectively compare topic models and demonstrate quality to stakeholders**.

#### ISC CRITERIA (8 words each)

- [ ] Topic coherence C V metric calculated for models
- [ ] Topic diversity unique words metric calculated for models
- [ ] NPMI coherence metric calculated for all fitted models
- [ ] Stability analysis via bootstrap resampling completed successfully
- [ ] Model comparison A B test framework implemented
- [ ] Results returned as tidy tibble for plotting
- [ ] Integration with yardstick metric set paradigm completed
- [ ] All metrics validated against Python BERTopic baselines

#### IMPLEMENTATION NOTES

```r
# Basic usage
bert_evaluate(model, documents, 
              metrics = c("coherence_cv", "coherence_npmi", "diversity"))

# Returns tibble:
# # A tibble: 1 × 4
#   coherence_cv coherence_npmi diversity model_id
#          <dbl>          <dbl>     <dbl> <chr>   
# 1        0.523          0.412     0.873 model_1

# Metric set pattern (yardstick-style)
my_metrics <- bert_metric_set(coherence_cv, diversity, stability)
my_metrics(model, documents)

# Model comparison
bert_compare_models(
  models = list(model_a = m1, model_b = m2),
  documents = docs,
  metrics = my_metrics
)
# Returns comparison tibble with statistical significance tests
```

**Technical Considerations:**
- Use `gensim` for coherence calculations (already a BERTopic dependency)
- Stability via bootstrap: fit model N times with resampling, measure topic overlap
- Diversity: count unique words across top-K keywords per topic
- Return structure compatible with `yardstick::metric_set()` paradigm

#### ANTI-CRITERIA (what to avoid)

- [ ] Metrics require excessive computation time blocking workflow
- [ ] Results incompatible with tidyverse plotting workflow patterns
- [ ] Metrics diverge from Python BERTopic reference implementations

---

### FEATURE SPEC 2: bert_config() - Centralized Configuration Architecture

**Category:** 🔧 USABILITY & WORKFLOWS  
**Priority:** 🔴 HIGH  
**EV Score:** 7.875  
**Complexity:** M-L (2-3 weeks refactoring)  
**Dependencies:** Requires refactoring existing functions

#### DESCRIPTION

Centralized configuration object enabling **progressive API disclosure** with basic/advanced/expert levels. Reduces parameter explosion (currently 137 functions with repeated parameters) and enables consistent defaults across all bertopic functions.

#### USER STORY

As a **package maintainer**, I want **centralized configuration with complexity levels** so that **beginners see 20 functions while power users access all 137**.

#### ISC CRITERIA (8 words each)

- [ ] bert config function creates validated configuration objects
- [ ] Config supports basic advanced expert complexity levels
- [ ] All bert functions accept config parameter consistently
- [ ] Config validate method catches common parameter mistakes
- [ ] Level basic exposes twenty core functions only
- [ ] Level expert exposes all one hundred thirty seven
- [ ] Existing API remains backward compatible with configs
- [ ] Print method shows active config with explanations

#### IMPLEMENTATION NOTES

```r
# Create config with level
config <- bert_config(
  level = "basic",  # or "advanced", "expert"
  embedding_model = "all-MiniLM-L6-v2",
  min_topic_size = 10,
  verbose = TRUE,
  seed = 42
)

# Validation
config$validate()
# Errors if: min_topic_size < 2, embedding_model unknown, etc.

# Use in all functions
model <- bert_topic(config = config)
topics <- bert_fit_transform(model, documents, config = config)

# Print shows active settings
print(config)
# bert_config (level: basic)
# ├── Embedding: all-MiniLM-L6-v2
# ├── Min topic size: 10
# ├── Verbose: TRUE
# └── Seed: 42 (reproducible)

# Level controls autocomplete
config_basic$  # only 20 core parameters
config_expert$ # all 50+ parameters
```

**Technical Considerations:**
- Implement as S3 object: `structure(list(...), class = "bert_config")`
- Level enforcement via `$` accessor that filters by level
- Backward compatibility: all functions accept both old parameters OR config
- Use `validate()` to catch: invalid models, conflicting parameters, missing deps
- Config can be saved/loaded with `saveRDS()` for reproducibility

#### ANTI-CRITERIA (what to avoid)

- [ ] Breaking changes to existing user code workflows
- [ ] Config adds complexity rather than reducing it
- [ ] Levels are rigid preventing custom parameter access

---

### FEATURE SPEC 3: pins Embedding Caching

**Category:** 💾 PERSISTENCE & CACHING  
**Priority:** 🔴 HIGH  
**EV Score:** 6.55  
**Complexity:** L (1 week)  
**Dependencies:** pins package

#### DESCRIPTION

Integration with `pins` package to **cache expensive embedding computations** (10+ minutes for large corpora). Enables versioned storage of embeddings with metadata, dramatically reducing iteration time during model development.

#### USER STORY

As a **data scientist**, I want **cached embeddings** so that **I don't wait 15 minutes every time I tune hyperparameters**.

#### ISC CRITERIA (8 words each)

- [ ] bert pin embeddings function saves to boards
- [ ] bert read embeddings loads from cached versions
- [ ] Embeddings versioned automatically with metadata tags attached
- [ ] Cache invalidation detects document set changes correctly
- [ ] Board abstraction supports local azure s three
- [ ] Integration works with bert fit transform workflows
- [ ] Metadata includes model corpus size creation date
- [ ] Performance tests show ten plus minute savings

#### IMPLEMENTATION NOTES

```r
library(pins)

# Save embeddings to pin
board <- board_local()  # or board_s3(), board_azure()

bert_pin_embeddings(
  embeddings = my_embeddings,
  board = board,
  name = "project_embeddings_v1",
  metadata = list(
    model = "all-MiniLM-L6-v2",
    corpus_size = nrow(docs),
    created = Sys.Date()
  )
)

# Load from cache
cached_emb <- bert_read_embeddings(board, "project_embeddings_v1")

# Use in workflow
model <- bert_fit_transform(
  bert_topic(),
  documents = docs,
  embeddings = cached_emb  # skip 15-minute computation!
)

# List versions
bert_embedding_versions(board, "project_embeddings_v1")
# # A tibble: 3 × 3
#   version created    corpus_size
#   <int>   <date>           <int>
# 1       1 2026-01-15      10000
# 2       2 2026-01-20      12000
# 3       3 2026-01-30      15000
```

**Technical Considerations:**
- Embeddings are matrices/arrays - use `pins::pin_write()` with type = "rds"
- Metadata should include: model name, corpus size, document hash (for invalidation)
- Cache invalidation: compute hash of document text, compare to metadata
- Support all pin boards: local, S3, Azure, GCS
- Document best practices: when to cache, cache invalidation strategies

#### ANTI-CRITERIA (what to avoid)

- [ ] Cache grows unbounded consuming disk space indefinitely
- [ ] Cache invalidation false positives cause unnecessary recomputation
- [ ] API is complex requiring understanding pin internals

---

## Implementation Roadmap

### Phase 1: Quick Wins (v0.1.3) - 2 weeks

```
├── print/autoplot methods (3 days)
├── bertopic_quickstart() (1 week)
└── bert_explain() (1 week)
```

**Deliverables:**
- Better UX with print/summary/autoplot
- One-liner workflow for 80% use case
- Educational output reducing support burden

---

### Phase 2: Core Infrastructure (v0.1.4) - 4 weeks

```
├── bert_config() architecture (2 weeks)
├── bert_evaluate() metrics (2 weeks)
└── pkgdown website + vignettes (overlapping)
```

**Deliverables:**
- Centralized configuration with levels
- Industry-first evaluation metrics
- Comprehensive documentation site

---

### Phase 3: Performance & Scale (v0.1.5) - 3 weeks

```
├── pins embedding caching (1 week)
├── bert_pipeline() checkpointing (2 weeks)
└── Performance documentation (overlapping)
```

**Deliverables:**
- 10+ minute time savings via caching
- Enterprise-grade checkpointing
- GPU setup guides

---

### Phase 4: Advanced Features (v0.2.0) - 6+ weeks

```
├── bert_dynamic() temporal modeling (3 weeks)
├── tidymodels integration (3 weeks)
└── bert_multimodal() (defer to v0.2.1+)
```

**Deliverables:**
- Topics over time analysis
- Recipe step integration
- Roadmap for multi-modal

---

## Competitive Positioning

### bertopic vs bertopicr (CRAN Competitor)

| Capability | bertopic (ours) | bertopicr |
|------------|-----------------|-----------|
| **Exports** | 137 functions | ~30 functions |
| **Evaluation metrics** | v0.1.4+ | ❌ None |
| **LLM representations** | 10+ models | ❌ None |
| **KeyBERT** | Full integration | ❌ None |
| **PolyFuzz** | Full integration | ❌ None |
| **Config system** | v0.1.4+ | Basic |
| **CRAN** | Not yet | ✅ Yes |
| **pkgdown docs** | v0.1.4+ | ✅ Yes |

**Strategy:** Position as "power user" choice with superior depth. CRAN submission after v0.1.4 when docs are complete.

---

## Success Metrics

### v0.1.3 Targets (Quick Wins)

- Time to first model: 45 min → 5 min
- GitHub stars: ~50 → ~100
- User-reported friction issues: Reduced 30%

### v0.1.4 Targets (Core Infrastructure)

- CRAN submission ready: YES
- Documentation completeness: 40% → 90%
- Academic citations: ~5/year → ~15/year
- Monthly downloads: ~100 → ~500

### v0.2.0 Targets (Advanced)

- Enterprise adoption: 3+ companies
- GitHub stars: ~200 → ~500
- Conference presentations: 2+
- CRAN downloads: ~500 → ~2000/month

---

## Contributors

**Package Maintainer:** Alex Bresler
**Feature Analysis:** Claude Opus 4.5 (PAI Algorithm)
**Date:** 2026-01-30
