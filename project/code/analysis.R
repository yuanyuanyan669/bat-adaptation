############################################################
## 0. Setup & Packages
############################################################

rm(list = ls())

library(tidyverse)    # dplyr, ggplot2, tidyr, stringr, forcats, ...
library(janitor)
library(ggthemes)
library(viridis)
library(ggrepel)
library(patchwork)
library(cowplot)

library(tidytext)
library(text2vec)
library(irlba)
library(Matrix)
library(naniar)
library(plotly)
library(uwot)
library(scales)
library(clustMixType) # (not used now, but kept if needed later)
library(klaR)         # for k-modes

theme_set(theme_minimal(base_size = 14))

# Ensure output directory exists
dir.create("project/outputs/figures_FP", recursive = TRUE, showWarnings = FALSE)


############################################################
## 1. Load Data & Clean Names
############################################################

bats_raw <- read.csv(
  "project/data/FP/bats_information.csv",
  stringsAsFactors = FALSE
)

bats <- bats_raw %>%
  clean_names()

glimpse(bats)
names(bats)


############################################################
## 2. Basic EDA
############################################################

# Missingness by variable
gg_miss_var(bats) +
  labs(title = "Missingness by Column")

# Count rows/cols
cat("Rows:", nrow(bats), "  Columns:", ncol(bats), "\n")

# Key categorical columns
cols_cat <- c(
  "species", "habitat", "average_lifespan",
  "environment", "food",
  "dna", "dna_structure",
  "protection_from_external_threats",
  "protection_from_diseases",
  "susceptible_diseases"
) %>%
  intersect(names(bats))

for (col in cols_cat) {
  cat("\n====", toupper(col), "====\n")
  print(
    bats %>%
      count(.data[[col]], sort = TRUE) %>%
      head(10)
  )
}

# Helper: bar plot of top k categories
plot_top_categories <- function(data, var, k = 10, title_prefix = "") {
  data %>%
    count(.data[[var]]) %>%
    arrange(desc(n)) %>%
    slice_head(n = k) %>%
    mutate(!!var := forcats::fct_reorder(.data[[var]], n)) %>%
    ggplot(aes(x = .data[[var]], y = n)) +
    geom_col(fill = "#3366AA") +
    coord_flip() +
    labs(
      title = paste0(title_prefix, "Top ", k, " ", stringr::str_to_title(var)),
      x = stringr::str_to_title(var),
      y = "Count"
    )
}

p_species  <- plot_top_categories(bats, "species",           k = 10)
p_habitat  <- plot_top_categories(bats, "habitat",           k = 10)
p_lifespan <- plot_top_categories(bats, "average_lifespan",  k = 10)
p_food     <- plot_top_categories(bats, "food",              k = 10)
p_env      <- plot_top_categories(bats, "environment",       k = 10)

p_combined <- (p_species + p_habitat) / (p_lifespan + p_food) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Basic Categorical Distributions in Bat Dataset"
  )

ggsave(
  filename = "project/outputs/figures_FP/fig1_categorical_distributions.png",
  plot     = p_combined,
  width    = 12,
  height   = 8,
  dpi      = 300
)


############################################################
## 3. Co-occurrence Heatmaps
############################################################

# 1. Food vs Habitat
p_food_habitat <- bats %>%
  count(habitat, food) %>%
  group_by(habitat) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(food, habitat, fill = prop)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Food Strategy vs Habitat",
    x = "Food",
    y = "Habitat"
  )

# 2. DNA vs Protection from Diseases
p_dna_protdisease <- bats %>%
  count(dna, protection_from_diseases) %>%
  group_by(dna) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(protection_from_diseases, dna, fill = prop)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "DNA Traits vs Protection from Diseases",
    x = "Protection from Diseases",
    y = "DNA Trait"
  )

# 3. Environment vs Protection from External Threats
p_env_extprot <- bats %>%
  count(environment, protection_from_external_threats) %>%
  group_by(environment) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(protection_from_external_threats, environment, fill = prop)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Environment vs Protection from External Threats",
    x = "Protection from External Threats",
    y = "Environment"
  )

# 4. Environment vs Susceptible Diseases
p_env_suscdisease <- bats %>%
  count(environment, susceptible_diseases) %>%
  group_by(environment) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(susceptible_diseases, environment, fill = prop)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Environment vs Susceptible Diseases",
    x = "Susceptible Diseases",
    y = "Environment"
  )

p_combined_heatmaps <- (p_food_habitat + p_dna_protdisease) /
  (p_env_extprot + p_env_suscdisease) +
  plot_annotation(
    title = "Co-occurrence Heatmaps of Bat Adaptation Traits",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave(
  filename = "project/outputs/figures_FP/fig2_heatmaps_combined.png",
  plot     = p_combined_heatmaps,
  width    = 14,
  height   = 12,
  dpi      = 300
)


############################################################
## 4. Text Tokenization
############################################################

data("stop_words")

desc_cols <- c(
  "shape", "habitat",
  "environment", "food",
  "dna", "dna_structure",
  "protection_from_external_threats",
  "protection_from_diseases",
  "susceptible_diseases"
) %>%
  intersect(names(bats))

text_long <- bats %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(
    cols      = all_of(desc_cols),
    names_to  = "field",
    values_to = "text"
  ) %>%
  filter(!is.na(text), text != "")

tokens <- text_long %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]"))

# Overall word counts
word_counts_all <- tokens %>%
  count(word, sort = TRUE)

head(word_counts_all, 20)

# Top words per field
top_words_by_field <- tokens %>%
  count(field, word, sort = TRUE) %>%
  group_by(field) %>%
  slice_max(n, n = 15, with_ties = FALSE) %>%
  ungroup()

p_topword <- ggplot(
  top_words_by_field,
  aes(x = n, y = tidytext::reorder_within(word, n, field))
) +
  geom_col(fill = "#1f78b4") +
  scale_y_reordered() +
  facet_wrap(~ field, scales = "free_y") +
  labs(
    title = "Top 15 Words by Field",
    x     = "Count",
    y     = NULL
  )

ggsave(
  filename = "project/outputs/figures_FP/fig3_topwords_combined.png",
  plot     = p_topword,
  width    = 14,
  height   = 12,
  dpi      = 300
)


############################################################
## 5. TF–IDF per Field
############################################################

tfidf_by_field <- tokens %>%
  count(field, word, sort = FALSE) %>%
  bind_tf_idf(term = word, document = field, n = n)

top_tfidf_field <- tfidf_by_field %>%
  arrange(desc(tf_idf)) %>%
  group_by(field) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup()

p_tfidf <- ggplot(
  top_tfidf_field,
  aes(x = tf_idf, y = tidytext::reorder_within(word, tf_idf, field))
) +
  geom_col(fill = "#33a02c") +
  scale_y_reordered() +
  facet_wrap(~ field, scales = "free_y") +
  labs(
    title = "High TF–IDF Words by Field",
    x     = "TF–IDF",
    y     = NULL
  )

ggsave(
  filename = "project/outputs/figures_FP/fig3b_tfidf_by_field.png",
  plot     = p_tfidf,
  width    = 14,
  height   = 12,
  dpi      = 300
)


############################################################
## 6. Document-level TF–IDF Embedding + Truncated SVD
############################################################

bats_text <- bats %>%
  mutate(row_id = row_number())

text_mat <- bats_text %>%
  dplyr::select(dplyr::all_of(desc_cols)) %>%
  as.matrix()

bats_text$text_all <- apply(
  text_mat,
  1,
  function(x) paste(x, collapse = " ")
)

docs   <- bats_text$text_all
doc_id <- bats_text$row_id

it <- itoken(
  docs,
  ids        = doc_id,
  tokenizer  = word_tokenizer,
  progressbar = FALSE
)

vocab <- create_vocabulary(it) %>%
  prune_vocabulary(term_count_min = 3)

vectorizer <- vocab_vectorizer(vocab)
dtm        <- create_dtm(it, vectorizer)

tfidf_transformer <- TfIdf$new()
dtm_tfidf         <- tfidf_transformer$fit_transform(dtm)

set.seed(123)
svd_res <- irlba(dtm_tfidf, nv = 30, nu = 30)

doc_emb <- svd_res$u %*% diag(svd_res$d)

emb <- as.data.frame(doc_emb)
colnames(emb) <- paste0("SVD", 1:ncol(emb))
emb$row_id    <- bats_text$row_id

bats_plot <- bats %>%
  mutate(row_id = row_number())

emb_full <- emb %>%
  left_join(bats_plot, by = "row_id") %>%
  mutate(
    clade = case_when(
      str_detect(species, "Yinpterochiroptera") ~ "Yinpterochiroptera",
      str_detect(species, "Yangochiroptera")    ~ "Yangochiroptera",
      TRUE                                      ~ "Other/Unknown"
    )
  )



############################################################
## 7. Clustering in Text-Embedding Space (SVD + UMAP)
############################################################

set.seed(123)
km_svd <- kmeans(doc_emb, centers = 3, nstart = 25)

emb_full <- emb_full %>%
  mutate(cluster_svd30 = factor(km_svd$cluster))

set.seed(123)
umap_svd <- umap(
  doc_emb,
  n_components = 2,
  n_neighbors  = 50,
  min_dist     = 0.3,
  metric       = "cosine"
)

umap_svd_df <- data.frame(
  U1             = umap_svd[, 1],
  U2             = umap_svd[, 2],
  cluster_svd30  = emb_full$cluster_svd30
)

p_svd_cluster <- ggplot(umap_svd_df,
                        aes(U1, U2, color = cluster_svd30)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_d(name = "Cluster") +
  labs(
    title    = "UMAP Projection of SVD Embedding",
    subtitle = "Parameters: n_neighbors = 50, min_dist = 0.3, metric = cosine",
    x        = "UMAP1",
    y        = "UMAP2"
  )

ggsave(
  filename = "project/outputs/figures_FP/fig4_umap_clusters.png",
  plot     = p_svd_cluster,
  width    = 7,
  height   = 5,
  dpi      = 300
)

# Rebuild clade label once more explicitly (in case species changed)
emb_full <- emb_full %>%
  mutate(
    clade = case_when(
      grepl("Yangochiroptera", species)    ~ "Yangochiroptera",
      grepl("Yinpterochiroptera", species) ~ "Yinpterochiroptera",
      TRUE                                 ~ "Other"
    )
  )

umap_clade_df <- data.frame(
  U1    = umap_svd[, 1],
  U2    = umap_svd[, 2],
  clade = emb_full$clade
)

p_svd_clade <- ggplot(umap_clade_df, aes(U1, U2, color = clade)) +
  geom_point(alpha = 0.75, size = 1.8) +
  scale_color_viridis_d(option = "plasma", name = "Clade") +
  labs(
    title = "UMAP Projection of SVD Embeddings (Colored by Clade)",
    x     = "UMAP1",
    y     = "UMAP2"
  )

ggsave(
  filename = "project/outputs/figures_FP/fig5_svd_clade.png",
  plot     = p_svd_clade,
  width    = 7,
  height   = 5,
  dpi      = 300
)


############################################################
## 8. Cluster Profile Bar Plots
############################################################

cluster_profile_bar <- function(data, cluster_var, feature_var, title) {
  data %>%
    count(
      cluster = .data[[cluster_var]],
      feature = .data[[feature_var]]
    ) %>%
    group_by(cluster) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    mutate(feature = forcats::fct_reorder(feature, prop, .fun = max)) %>%
    ggplot(aes(x = feature, y = prop, fill = cluster)) +
    geom_col(position = position_dodge(width = 0.8)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_viridis_d(name = "Cluster") +
    theme(
      axis.text.x       = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank()
    ) +
    labs(
      title = title,
      x     = stringr::str_to_title(feature_var),
      y     = "Within-cluster proportion"
    )
}

cluster_dat <- emb_full %>%
  dplyr::select(cluster_svd30, environment, food, susceptible_diseases)

p_env_bar <- cluster_profile_bar(
  data        = cluster_dat,
  cluster_var = "cluster_svd30",
  feature_var = "environment",
  title       = "Environmental Profile of Text-based Clusters"
)

p_food_bar <- cluster_profile_bar(
  data        = cluster_dat,
  cluster_var = "cluster_svd30",
  feature_var = "food",
  title       = "Feeding Strategy Profile of Text-based Clusters"
)

p_disease_bar <- cluster_profile_bar(
  data        = cluster_dat,
  cluster_var = "cluster_svd30",
  feature_var = "susceptible_diseases",
  title       = "Disease Susceptibility Profile of Text-based Clusters"
)

p_cluster_bars <- p_env_bar + p_food_bar + p_disease_bar

ggsave(
  filename = "project/outputs/figures_FP/fig6_cluster_trait_barprofiles.png",
  plot     = p_cluster_bars,
  width    = 16,
  height   = 6,
  dpi      = 300
)


############################################################
## 9. K-modes Clustering on Categorical Traits + UMAP
############################################################

# Reuse desc_cols defined above
bats_cat <- bats %>%
  mutate(across(all_of(desc_cols), as.factor))

set.seed(123)
k_kmodes <- 3

km_cat <- kmodes(
  bats_cat[desc_cols],
  modes    = k_kmodes,
  iter.max = 20,
  weighted = FALSE
)

bats_cat <- bats_cat %>%
  mutate(cluster_km = factor(km_cat$cluster))

# One-hot encoding for UMAP geometry
X_onehot <- model.matrix(~ . - 1, data = bats_cat[desc_cols])

set.seed(123)
umap_km <- umap(
  X_onehot,
  n_neighbors  = 30,
  min_dist     = 0.1,
  metric       = "manhattan",  
  n_components = 2
)

umap_km_df <- data.frame(
  U1      = umap_km[, 1],
  U2      = umap_km[, 2],
  cluster = bats_cat$cluster_km
)

p_cluster_km <- ggplot(umap_km_df, aes(U1, U2, color = cluster)) +
  geom_point(alpha = 0.6, size = 1.8) +
  scale_color_viridis_d(name = "Cluster") +
  labs(
    title = "UMAP of Categorical Encodings (k-modes clusters)",
    x     = "UMAP1",
    y     = "UMAP2"
  )

ggsave(
  filename = "project/outputs/figures_FP/fig7_kmodes_umap.png",
  plot     = p_cluster_km,
  width    = 7,
  height   = 5,
  dpi      = 300
)
