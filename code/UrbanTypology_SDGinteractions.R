rm(list = ls())

# Packages used across the script
library(readxl); library(dplyr); library(tidyr); library(tibble); library(writexl); library(openxlsx); library(vegan); library(circlize)
library(DMwR2); library(Metrics); library(ggplot2); library(ggrepel); library(corrplot); library(Hmisc); library(igraph); library(grid)
library(here)

# Project directories
project_dir <- here()
analysis_dir <- here("data", "Original_data")
park_dir <- here("data", "ParkServe_fgd_DataShare")
result_dir <- here("results")

if (!dir.exists(result_dir)) dir.create(result_dir, recursive = TRUE)

#==========================================================================================================
#-------------------------------------- Data prepossessing ------------------------------------------------
#==========================================================================================================
#----------------------------------------------------------
#--------Impute missing data of SDG15_1 Park area----------
# Read MSA list and 2025 ParkServe data
msa_df <- read_excel(file.path(park_dir, "MSA.xlsx")) %>% dplyr::select(num, MSA)
msa_names <- msa_df$MSA
park_df <- read_excel(file.path(park_dir, "CityParkland_2025.xlsx")) %>% mutate(PL_TYPE = tolower(PL_TYPE))

park_city <- park_df %>% filter(PL_TYPE == "city", ParkServe_UrbanArea %in% msa_names)
found_city <- unique(park_city$ParkServe_UrbanArea)
missing_after_city <- setdiff(msa_names, found_city)

park_town <- park_df %>% filter(PL_TYPE == "town", ParkServe_UrbanArea %in% missing_after_city)
found_town <- unique(park_town$ParkServe_UrbanArea)
missing_after_town <- setdiff(missing_after_city, found_town)

park_county <- park_df %>% filter(PL_TYPE == "county", ParkServe_UrbanArea %in% missing_after_town)
found_county <- unique(park_county$ParkServe_UrbanArea)
missing_final <- setdiff(missing_after_town, found_county)

park_selected <- bind_rows(park_city, park_town, park_county)

df_summary <- park_selected %>% group_by(ParkServe_UrbanArea) %>%
  summarise(PL_TYPE_used = first(PL_TYPE), Total_Park_sum = sum(Total_Park, na.rm = TRUE),
            Total_City_sum = sum(Total_City, na.rm = TRUE), .groups = "drop") %>%
  mutate(Value2025 = Total_Park_sum / Total_City_sum * 100) %>%
  left_join(msa_df, by = c("ParkServe_UrbanArea" = "MSA")) %>%
  arrange(num) %>% dplyr::select(num, ParkServe_UrbanArea, PL_TYPE_used, Total_Park_sum, Total_City_sum, Value2025)
write_xlsx(df_summary, file.path(park_dir, "CityParkland_105city_2025.xlsx"))


# Use 2025 and 2016 data to impute cities without 2016 data using single linear regression
data_wide <- read_excel(file.path(park_dir, "CityParkland_2016_2025.xlsx"))
train <- data_wide %>% filter(!is.na(Value2016), !is.na(Value2025)); pred_idx <- is.na(data_wide$Value2016) & !is.na(data_wide$Value2025)

park_model <- lm(Value2016 ~ Value2025, data = train); train_pred <- predict(park_model, newdata = train)
rmse_train <- rmse(train$Value2016, train_pred); mae_train <- mae(train$Value2016, train_pred)
cat("Training RMSE (in-sample):", rmse_train, "\n"); cat("Training MAE (in-sample):", mae_train, "\n")

data_wide$Value2016[pred_idx] <- predict(park_model, newdata = data_wide[pred_idx, , drop = FALSE])

output_file <- file.path(park_dir, "CityData_Imputed.xlsx")
model_summary <- capture.output(summary(park_model))
accuracy_text <- c(paste0("Training RMSE: ", round(rmse_train, 4)), paste0("Training MAE: ", round(mae_train, 4)), "", model_summary)

wb <- createWorkbook()
addWorksheet(wb, "Sheet 1"); writeData(wb, "Sheet 1", data_wide)
addWorksheet(wb, "model accuracy"); writeData(wb, "model accuracy", accuracy_text, colNames = FALSE)

setColWidths(wb, "Sheet 1", cols = 1:ncol(data_wide), widths = "auto")
setColWidths(wb, "model accuracy", cols = 1, widths = 100)
saveWorkbook(wb, output_file, overwrite = TRUE)


#-------------------------------------------------------------------------------
#--------Impute SDG indicators with missing data by using KNN imputation--------
# Sensitivity analysis added to determine optimal k

# 1. File paths
input_path <- file.path(analysis_dir, "1_raw data_55indi_unsigned.xlsx")

# 2. Read data
df <- read_excel(input_path); city_names <- df$city
df_indicators <- df %>% dplyr::select(-city) %>% mutate(across(everything(), as.numeric)); original_missing <- is.na(df_indicators)
cat("Total cities:", nrow(df_indicators), "| Indicators:", ncol(df_indicators), "| Missing values:", sum(original_missing),
    "| Missing rate:", round(mean(original_missing) * 100, 2), "%\n")

# 3. Complete cases for KNN sensitivity analysis
df_complete <- as.data.frame(df_indicators[complete.cases(df_indicators), ]); cat("Complete rows for validation:", nrow(df_complete), "\n")

# 4. Sensitivity analysis settings
set.seed(42); k_values <- c(3, 5, 7, 10, 15); n_rep <- 100; test_fraction <- 0.20; mask_rate <- 0.10; results <- data.frame()

# 5. Repeated train-test masking validation
for (rep in seq_len(n_rep)) {
  test_id <- sample(seq_len(nrow(df_complete)), round(test_fraction * nrow(df_complete)))
  train <- df_complete[-test_id, , drop = FALSE]; test <- df_complete[test_id, , drop = FALSE]
  mask <- matrix(FALSE, nrow(test), ncol(test)); mask[sample(length(mask), round(mask_rate * length(mask)))] <- TRUE
  test_masked <- test; test_masked[mask] <- NA
  train_mean <- sapply(train, mean, na.rm = TRUE); train_sd <- sapply(train, sd, na.rm = TRUE); train_sd[is.na(train_sd) | train_sd == 0] <- 1
  variable_id <- arrayInd(which(mask), dim(mask))[, 2]; true_values <- as.matrix(test)[mask]; true_z <- (true_values - train_mean[variable_id]) / train_sd[variable_id]
  
  for (k in k_values) {
    if (k >= nrow(train)) next
    imputed_test <- knnImputation(test_masked, k = k, scale = TRUE, distData = train); predicted_values <- as.matrix(imputed_test)[mask]
    predicted_z <- (predicted_values - train_mean[variable_id]) / train_sd[variable_id]
    results <- rbind(results, data.frame(repetition = rep, k = k,
                                         RMSE = sqrt(mean((true_z - predicted_z)^2, na.rm = TRUE)), MAE = mean(abs(true_z - predicted_z), na.rm = TRUE)))
  }
  if (rep %% 10 == 0) cat("Completed repetition", rep, "of", n_rep, "\n")
}

# 6. Summarize sensitivity results
summary_results <- results %>% group_by(k) %>% summarise(Mean_RMSE = mean(RMSE), SD_RMSE = sd(RMSE), Mean_MAE = mean(MAE), SD_MAE = sd(MAE), .groups = "drop")
print(summary_results %>% mutate(across(where(is.numeric), ~round(.x, 5))))

# 7. Select optimal k
best_k_rmse <- summary_results$k[which.min(summary_results$Mean_RMSE)]; best_rmse <- min(summary_results$Mean_RMSE)
cat("Lowest mean RMSE at k =", best_k_rmse, "\n")

# 8. Plot sensitivity analysis
p_knn <- ggplot(summary_results, aes(k, Mean_RMSE)) + geom_line(color = "#4A90D9", linewidth = 1) + geom_point(color = "#4A90D9", size = 3) +
  geom_errorbar(aes(ymin = Mean_RMSE - SD_RMSE, ymax = Mean_RMSE + SD_RMSE), width = 0.5, color = "#4A90D9") +
  geom_point(data = filter(summary_results, k == best_k_rmse), color = "#D93F49", size = 4) +
  annotate("text", x = best_k_rmse, y = best_rmse, label = paste0("Lowest RMSE: k = ", best_k_rmse), vjust = -1.5, color = "#D93F49", fontface = "bold") +
  labs(title = "KNN Imputation Sensitivity Analysis", x = "Number of neighbors (k)", y = "Mean standardized RMSE") +
  theme_minimal(base_family = "sans") + theme(text = element_text(size = 12), plot.title = element_text(hjust = 0.5))
print(p_knn); ggsave(file.path(analysis_dir, "KNN_sensitivity_analysis.png"), p_knn, width = 6, height = 4, dpi = 300)
write.csv(summary_results, file.path(analysis_dir, "KNN_sensitivity_results.csv"), row.names = FALSE)
write.csv(results, file.path(analysis_dir, "KNN_sensitivity_all_repetitions.csv"), row.names = FALSE)

# 9. Final KNN imputation using optimal k
final_k <- 10; df_imputed <- knnImputation(as.data.frame(df_indicators), k = final_k, scale = TRUE)
df_final <- data.frame(city = city_names, df_imputed, check.names = FALSE)

# 10. Save final imputed dataset and highlight imputed values
output_file <- file.path(analysis_dir, paste0("raw_data_55indi_KNN_imputed_k", final_k, ".xlsx"))
wb <- createWorkbook(); addWorksheet(wb, "KNN_imputed"); writeData(wb, "KNN_imputed", df_final); imputed_style <- createStyle(fontColour = "#FF0000")
for (j in seq_len(ncol(original_missing))) { missing_rows <- which(original_missing[, j]); if (length(missing_rows) > 0) addStyle(wb, "KNN_imputed", imputed_style, rows = missing_rows + 1, cols = j + 1, gridExpand = TRUE, stack = TRUE) }
freezePane(wb, "KNN_imputed", firstActiveRow = 2, firstActiveCol = 2); setColWidths(wb, "KNN_imputed", 1:ncol(df_final), widths = "auto"); saveWorkbook(wb, output_file, overwrite = TRUE)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------Normalize SDG indicators based on the best and worst values set by 2019 city report for calculating SDG scores, as we modify the value of indicators--------
raw_path <- output_file; ref_path <- file.path(analysis_dir, "2_raw data_unsigned_reference.xlsx")
df <- read_excel(raw_path); ref <- read_excel(ref_path); best_vals <- setNames(ref$best, ref$Indicator); worst_vals <- setNames(ref$worst, ref$Indicator)
normalize_fixed <- function(x, ind_name) pmin(100, pmax(0, (x - worst_vals[ind_name]) / (best_vals[ind_name] - worst_vals[ind_name]) * 100))
for (ind in names(df)[-1]) df[[ind]] <- normalize_fixed(df[[ind]], ind)

# Calculate 15 SDG scores (SDG14 is not included in the dataset)
sdg_ids <- c(1:13, 15, 16)
score_mat <- sapply(sdg_ids, function(g) rowMeans(df[, grepl(paste0("^G", g, "\\."), names(df)), drop = FALSE], na.rm = TRUE)); colnames(score_mat) <- paste0("SDG", sdg_ids)
df_scores <- data.frame(city = df$city, score_mat, check.names = FALSE); output_path <- file.path(analysis_dir, "SDG_scores_imputed.xlsx"); write_xlsx(df_scores, output_path)




#==========================================================================================================
#-------------------- Variations of overall SDG scores and each SDG at national scales --------------------
#==========================================================================================================
data_path <- file.path(analysis_dir, "SDG_scores_imputed.xlsx"); data_SDG <- read_excel(data_path); data_SDG$overall <- rowMeans(data_SDG[, -1], na.rm = TRUE)

p_density <- ggplot(data_SDG, aes(overall)) + geom_density(fill = "grey20", alpha = 0.5, color = "black", linewidth = 0.5) + labs(x = "SDG scores", y = "Density") +
  theme_minimal(base_family = "sans") + theme(text = element_text(size = 12, color = "black"), axis.text = element_text(color = "black"), axis.title = element_text(color = "black"),
                                              axis.ticks = element_line(color = "black", linewidth = 0.5), axis.ticks.length = unit(0.15, "cm"), panel.grid.major = element_line(linetype = "dashed", color = "grey70"), panel.grid.minor = element_blank())
ggsave(file.path(analysis_dir, "SDG_score_density.png"), p_density, width = 4, height = 4, dpi = 1200)

sdg_only <- data_SDG %>% dplyr::select(starts_with("SDG")); data_avg <- colMeans(sdg_only, na.rm = TRUE); data_sd <- sapply(sdg_only, sd, na.rm = TRUE)
radar_data_long <- tibble(SDG = names(data_avg), avg = as.numeric(data_avg), upper_bound = as.numeric(data_avg + data_sd), lower_bound = as.numeric(data_avg - data_sd))
sdg_levels <- paste0("SDG", c(1:13, 15, 16)); radar_data_long$SDG <- factor(radar_data_long$SDG, levels = sdg_levels)
sdg_colors <- c("SDG1"="#E5243B","SDG2"="#DDA63A","SDG3"="#4C9F38","SDG4"="#C5192D","SDG5"="#FF3A21","SDG6"="#26BDE2","SDG7"="#FCC30B","SDG8"="#A21942","SDG9"="#FD6925","SDG10"="#DD1367","SDG11"="#FD9D24","SDG12"="#BF8B2E","SDG13"="#3F7E44","SDG15"="#56C02B","SDG16"="#00689D")

p_nation <- ggplot(radar_data_long, aes(x = SDG, y = avg, fill = SDG)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                width = 0.2, position = position_dodge(0.9), color = "black", linewidth = 0.8) +
  coord_polar(start = 0) +
  scale_fill_manual(values = sdg_colors) +
  theme_minimal(base_family = "sans") +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 16),
    axis.title = element_text(color = "black", size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(color = "black"),
    panel.grid = element_line(color = "darkgrey", linewidth = 0.5),
    panel.grid.major = element_line(color = "darkgrey", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  labs(y = "SDG Scores", x = "", title = "")

print(p_nation)

ggsave(file.path(output_dir, "radar_plot_nation.png"), plot = p_nation, width = 7, height = 7, dpi = 1200)



#=================================
#--------Calcultion of GCI--------
#=================================
GIC_file <- file.path(analysis_dir, "5_governance_capacity_GCI_calculation.xlsx")
dat <- read_excel(GIC_file)

# Governance indicators
gov_vars <- c("VOTER_TURNOUT_ratio", "Nonprofit per 10k pop", "AVG_ANNUAL_WAGE",
              "EXP_GDP_RATIO", "establishment entry rate", "Fed Cor Conv per 100k")

# Min-max normalization: higher values = higher governance capacity
dat_norm <- dat %>%
  mutate(across(all_of(gov_vars), ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))))

# Reverse-code corruption: highest corruption = 0, lowest corruption = 1
dat_norm <- dat_norm %>%
  mutate(`Fed Cor Conv per 100k` = 1 - `Fed Cor Conv per 100k`)

# Mean of six normalized indicators for each city
dat_norm <- dat_norm %>%
  rowwise() %>%
  mutate(Governance_Capacity = mean(c_across(all_of(gov_vars)), na.rm = TRUE)) %>%
  ungroup()

write_xlsx(dat_norm, file.path(analysis_dir, "Governance_capacity_normalized.xlsx"))



#===============================================================================
#--------Apply effect directions to generate signed SDG indicators--------------
#===============================================================================
direction_file <- file.path(output_dir, "3_Effects directions of 55 indicators.xlsx")
imputed_file <- file.path(output_dir, "raw_data_55indi_KNN_imputed_k10.xlsx")

df_imputed <- read_excel(imputed_file)
direction_df <- read_excel(direction_file) %>% mutate(Indicators = as.character(Indicators), Directions = as.numeric(Directions))
directions <- setNames(direction_df$Directions, direction_df$Indicators)

indicator_cols <- names(df_imputed)[-1]
missing_direction <- setdiff(indicator_cols, names(directions))
if (length(missing_direction) > 0) stop("Missing effect directions for: ", paste(missing_direction, collapse = ", "))

df_signed <- df_imputed %>% mutate(across(all_of(indicator_cols), ~ .x * directions[cur_column()]))

write_xlsx(df_signed, file.path(output_dir, "Indicator_55_signed_imputed.xlsx"))

cat("Signed indicators saved successfully.\n")
cat("Indicators multiplied by -1:", sum(direction_df$Directions == -1), "\n")
cat("Indicators multiplied by 1:", sum(direction_df$Directions == 1), "\n")

#===========================================================================================
#--------K-means clustering analysis and Centro values of indicators in each typology--------
#===========================================================================================
file_path <- file.path(analysis_dir, "Indicator_55_signed_imputed.xlsx"); data_cluster <- read_excel(file_path, sheet = "Sheet1")
data_normalized <- scale(data_cluster[, -1])

X <- as.matrix(data_normalized)
results <- data.frame()

for (k in 2:6) {
  set.seed(123); km <- kmeans(X, centers = k, nstart = 25)
  crit <- clusterCrit::intCriteria(X, as.integer(km$cluster), c("Silhouette", "Calinski_Harabasz", "Davies_Bouldin", "Dunn"))
  results <- rbind(results, data.frame(K = k, Silhouette = crit[["silhouette"]], CH = crit[["calinski_harabasz"]],
                                       Davies_Bouldin = crit[["davies_bouldin"]], Dunn = crit[["dunn"]],
                                       Cluster_sizes = paste(sort(table(km$cluster), decreasing = TRUE), collapse = ", ")))
}

set.seed(123)
gap <- cluster::clusGap(X, FUNcluster = function(x, k) kmeans(x, centers = k, nstart = 25), K.max = 6, B = 500)

gap_table <- data.frame(K = 1:6, Gap = gap$Tab[, "gap"], Gap_SE = gap$Tab[, "SE.sim"])
gap_best_k <- cluster::maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method = "Tibs2001SEmax")

results$Gap <- gap_table$Gap[match(results$K, gap_table$K)]
results$Gap_SE <- gap_table$Gap_SE[match(results$K, gap_table$K)]
results <- results[, c("K", "Silhouette", "CH", "Davies_Bouldin", "Dunn", "Gap", "Gap_SE", "Cluster_sizes")]

best_k <- data.frame(
  Index = c("Silhouette", "Calinski-Harabasz", "Davies-Bouldin", "Dunn", "Gap Statistic"),
  Direction = c("Maximum", "Maximum", "Minimum", "Maximum", "Tibs2001SEmax"),
  Best_K = c(results$K[which.max(results$Silhouette)], results$K[which.max(results$CH)],
             results$K[which.min(results$Davies_Bouldin)], results$K[which.max(results$Dunn)], gap_best_k)
)

output_file <- file.path(analysis_dir, "Kmeans_K2_K6_5_Indices.xlsx") 
write.xlsx(list("K2_K6_Comparison" = results, "Best_K" = best_k, "Gap_Statistic" = gap_table), file = output_file, overwrite = TRUE)


optimal_clusters <- 3; set.seed(123); kmeans_result <- kmeans(data_normalized, centers = optimal_clusters, nstart = 25)
data_cluster$Typology <- factor(kmeans_result$cluster, labels = paste0("Typology", 1:optimal_clusters))
centroids <- as.data.frame(kmeans_result$centers); centroids$Typology <- paste0("Typology", 1:nrow(centroids)); centroids <- centroids %>% dplyr::select(Typology, everything())
write_xlsx(centroids, file.path(analysis_dir, "Indicator_centroids.xlsx"))

clustered_file <- file.path(analysis_dir, "Indicator_55_signed_imputed_with_typology.xlsx")
write_xlsx(data_cluster, clustered_file)



# =============================================================================
# PCA + 90% cumulative variance + multidimensional envfit
# =============================================================================
# -----------------------------------------------------------------------------
# 1. Paths and data
# -----------------------------------------------------------------------------
file_path <- file.path(analysis_dir, "55Indicator_13factor_typology.xlsx")
df <- read_excel(file_path); names(df) <- trimws(names(df))

# SDG indicators and factors
sdg_cols <- grep("^G\\d+\\.\\d+$", names(df), value=TRUE)
id_cols <- c("city", "Typology")
factor_cols <- setdiff(names(df), c(id_cols, sdg_cols))
factor_cols <- factor_cols[sapply(df[factor_cols], is.numeric)]

if (!all(id_cols %in% names(df))) stop("The data must contain 'city' and 'Typology' columns.")
if (anyNA(df[sdg_cols])) stop("SDG indicator columns still contain missing values.")

df$Typology <- factor(df$Typology, levels=c("Typology1", "Typology2", "Typology3"))

cat("SDG indicators:", length(sdg_cols), "\n")
cat("Factors:", length(factor_cols), "\n")
cat("Factor names:\n", paste(factor_cols, collapse="\n"), "\n\n")
cat("Typology sizes:\n"); print(table(df$Typology))

if (length(sdg_cols) != 55) warning("Expected 55 SDG indicators, but found ", length(sdg_cols), ".")
if (length(factor_cols) != 13) warning("Expected 13 factors, but found ", length(factor_cols), ".")

# -----------------------------------------------------------------------------
# 2. Standardize SDG indicators and perform PCA
# -----------------------------------------------------------------------------
Xz <- scale(df[, sdg_cols])
pca <- prcomp(Xz, center=FALSE, scale.=FALSE)

eig <- pca$sdev^2
var_pct <- eig / sum(eig) * 100
cum_pct <- cumsum(var_pct)

# Number of PCs required to explain >=90% of total variance
k90 <- which(cum_pct >= 90)[1]

cat(sprintf("\nPC1 explained %.2f%%\n", var_pct[1]))
cat(sprintf("PC2 explained %.2f%%\n", var_pct[2]))
cat(sprintf("PC1-2 cumulative = %.2f%%\n", cum_pct[2]))
cat(sprintf("First %d PCs explain %.2f%% of total variance.\n\n", k90, cum_pct[k90]))

var_tbl <- data.frame(PC=seq_along(var_pct), Variance_percent=var_pct, Cumulative_percent=cum_pct)

# -----------------------------------------------------------------------------
# 3. Plot PCA variance explained
# -----------------------------------------------------------------------------
axis_breaks <- sort(unique(c(1, seq(5, length(var_pct), by=5), k90)))

p_var <- ggplot(var_tbl, aes(x=PC)) +
  geom_col(aes(y=Variance_percent), width=0.72, fill="grey55") +
  geom_line(aes(y=Cumulative_percent), linewidth=1, color="black") +
  geom_point(aes(y=Cumulative_percent), size=1.7, color="black") +
  geom_hline(yintercept=90, linetype="dashed", linewidth=0.6, color="#D64045") +
  geom_vline(xintercept=k90, linetype="dashed", linewidth=0.6, color="#D64045") +
  annotate("text", x=k90 + 0.6, y=92.5, label=sprintf("90%% reached at PC%d", k90),
           hjust=0, size=4.3, color="#D64045") +
  scale_x_continuous(breaks=axis_breaks) +
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 20), expand=expansion(mult=c(0, 0.02))) +
  labs(x="Principal component", y="Explained variance (%)") +
  theme_classic(base_size=15)

ggsave(file.path(analysis_dir, "PCA_explained_variance.png"), p_var, width=10, height=6.5, dpi=400)

# -----------------------------------------------------------------------------
# 4. PCA coordinates
# -----------------------------------------------------------------------------
coords <- as.data.frame(pca$x)
names(coords) <- paste0("PC", seq_len(ncol(coords)))

ord <- coords[, 1:2]
ord$city <- df$city
ord$Typology <- df$Typology

# -----------------------------------------------------------------------------
# 5. envfit using PCs explaining >=90% of total variance
# -----------------------------------------------------------------------------
factor_df <- as.data.frame(df[, factor_cols])

set.seed(123)
ef <- envfit(coords[, 1:k90, drop=FALSE], factor_df, choices=1:k90, permutations=999, na.rm=TRUE)

# Full multidimensional vector scores
vec_scores <- as.data.frame(scores(ef, display="vectors", choices=1:k90))

env_tbl <- data.frame(Factor=rownames(vec_scores), R2=ef$vectors$r, P=ef$vectors$pvals, stringsAsFactors=FALSE)
env_tbl$Significance <- ifelse(env_tbl$P <= 0.001, "***", ifelse(env_tbl$P < 0.01, "**", ifelse(env_tbl$P < 0.05, "*", "ns")))

# First two PC vector coordinates used only for plotting
env_tbl$PC1 <- vec_scores[, 1]
env_tbl$PC2 <- vec_scores[, 2]
env_tbl <- env_tbl %>% arrange(P)

# -----------------------------------------------------------------------------
# 6. Short factor labels
# -----------------------------------------------------------------------------
short_labels <- c(
  "POP density (pop/km2)"="Pop. Density",
  "GDP per capita (thousands)"="GDP per capita",
  "Female Population%"="Female Pop. %",
  "Under 15 years old%"="Under-15 Pop. %",
  "Over 65 years old%"="Over-65 Pop. %",
  "GDP_Primary %"="GDP Primary %",
  "GDP_Secondary %"="GDP Secondary %",
  "VOTER_TURNOUT_ratio"="Voter Turnout",
  "Nonprofit per 10k pop"="Nonprofit Orgs. per 10k",
  "AVG_ANNUAL_WAGE"="Avg. Gov. Annual Wage",
  "EXP_GDP_RATIO"="Direct Exp. / GDP",
  "establishment entry rate"="Estab. Entry Rate",
  "Fed Cor Conv per 100k"="Fed. Corruption per 100k"
)

env_tbl$Display <- ifelse(env_tbl$Factor %in% names(short_labels), unname(short_labels[env_tbl$Factor]), env_tbl$Factor)
env_tbl$Label <- ifelse(env_tbl$P <= 0.001, paste0(env_tbl$Display, " ***"),
                        ifelse(env_tbl$P < 0.01, paste0(env_tbl$Display, " **"),
                               ifelse(env_tbl$P < 0.05, paste0(env_tbl$Display, " *"), env_tbl$Display)))

ord$PC2 <- -ord$PC2
env_tbl$PC2 <- -env_tbl$PC2

vec_sig <- env_tbl %>% filter(P < 0.05)

# -----------------------------------------------------------------------------
# 7. 2-SD covariance ellipses by Typology
# -----------------------------------------------------------------------------
TYPOLOGY_PT <- c("Typology1"="#A93226", "Typology2"="#3498DB", "Typology3"="#1F3A5F")
TYPOLOGY_FILL <- c("Typology1"="#E8B4B0", "Typology2"="#9CC4E0", "Typology3"="#7891B5")
TYPOLOGY_EDGE <- c("Typology1"="#C0392B", "Typology2"="#5DADE2", "Typology3"="#2C3E7A")

ellipse_2sd <- function(x, y, typology, n_std=1.8, n=300) {
  xy <- cbind(x, y); cv <- cov(xy); eg <- eigen(cv)
  theta <- seq(0, 2*pi, length.out=n); circle <- rbind(cos(theta), sin(theta))
  A <- eg$vectors %*% diag(n_std * sqrt(pmax(eg$values, 0)), nrow=2); pts <- t(A %*% circle)
  data.frame(PC1=pts[,1] + mean(x), PC2=pts[,2] + mean(y), Typology=factor(typology, levels=levels(df$Typology)))
}

ellipse_df <- bind_rows(lapply(levels(ord$Typology), function(tp) {
  d <- ord[ord$Typology == tp, ]; ellipse_2sd(d$PC1, d$PC2, tp)
}))

# -----------------------------------------------------------------------------
# 8. Scale envfit vectors for PC1-PC2 visualization
# -----------------------------------------------------------------------------
xr <- diff(range(ord$PC1)); yr <- diff(range(ord$PC2)); arrow_scale <- 0.6 * min(xr, yr)

vec_sig <- vec_sig %>% mutate(
  xend=PC1 * arrow_scale, yend=PC2 * arrow_scale,
  label_x=xend, label_y=yend, label_hjust=ifelse(xend < 0, 1, 0)
)

# -----------------------------------------------------------------------------
# 9. PCA + envfit figure
# -----------------------------------------------------------------------------
p_env <- ggplot() +
  geom_polygon(data=ellipse_df, aes(PC1, PC2, group=Typology, fill=Typology), alpha=0.1, color=NA) +
  geom_path(data=ellipse_df, aes(PC1, PC2, group=Typology, color=Typology), linewidth=0.9, alpha=0.80) +
  geom_point(data=ord, aes(PC1, PC2, fill=Typology), shape=21, size=3.3, color="white", stroke=0.65) +
  geom_text(data=ord, aes(PC1, PC2, label=city, color=Typology), size=3, vjust=-0.85, alpha=0.90, show.legend=FALSE) +
  geom_hline(yintercept=0, color="grey55", linewidth=0.40, linetype="dashed", alpha=0.55) +
  geom_vline(xintercept=0, color="grey55", linewidth=0.40, linetype="dashed", alpha=0.55) +
  geom_segment(data=vec_sig, aes(x=0, y=0, xend=xend, yend=yend), inherit.aes=FALSE,
               color="black", linewidth=0.9, arrow=arrow(length=unit(0.18, "cm"), type="open")) +
  geom_label_repel(data=vec_sig, aes(label_x, label_y, label=Label, hjust=label_hjust), inherit.aes=FALSE,
                   direction="y", size=6.5, color="black", fill=scales::alpha("white", 0.6), label.size=0,
                   label.r=unit(0.18, "lines"), box.padding=0.22, point.padding=0.10,
                   min.segment.length=Inf, segment.color=NA, max.overlaps=Inf, seed=123) +
  scale_color_manual(values=TYPOLOGY_EDGE, guide="none") +
  scale_fill_manual(values=TYPOLOGY_PT, breaks=c("Typology1","Typology2","Typology3"),
                    labels=c("Typology 1","Typology 2","Typology 3"), name=NULL) +
  labs(x=sprintf("PC1 (%.2f%%)", var_pct[1]), y=sprintf("PC2 (%.2f%%)", var_pct[2])) +
  theme_classic(base_size=18) +
  theme(panel.grid.major=element_line(color="#D9D9D9", linewidth=0.35, linetype="dashed"),
        panel.grid.minor=element_blank(), axis.line=element_line(color="#333333", linewidth=0.55),
        axis.title=element_text(size=20), axis.text=element_text(size=18, color="black"),
        legend.position=c(0.89, 0.93), legend.key.height=unit(0.9, "cm"),
        legend.background=element_rect(fill="white", color="#CCCCCC", linewidth=0.5),
        legend.key=element_blank(), legend.text=element_text(size=18),
        plot.margin=margin(12,14,10,10)) +
  guides(fill=guide_legend(override.aes=list(shape=21, size=4, color="white"))) +
  coord_cartesian(clip="off")

ggsave(file.path(analysis_dir, "PCA_envfit_90pct.png"), p_env, width=14, height=11, dpi=400, bg="white")

# -----------------------------------------------------------------------------
# 10. Export results to Excel
# -----------------------------------------------------------------------------
pca_coord_tbl <- cbind(city=df$city, Typology=df$Typology, coords)

write_xlsx(
  list(
    PCA_variance=var_tbl,
    Envfit_results=env_tbl,
    PCA_coordinates=pca_coord_tbl
  ),
  file.path(analysis_dir, "PCA_envfit_90pct_results.xlsx")
)





#=============================================================================================
#----------Spearman analysis at indicator level for overall level and typology levels----------
#=============================================================================================
clustered_file <- file.path(analysis_dir, "Indicator_55_signed_imputed_with_typology.xlsx")
data_all <- read_excel(clustered_file)

level_data <- list(
  overall = data_all,
  typology1 = data_all %>% filter(Typology == "Typology1"),
  typology2 = data_all %>% filter(Typology == "Typology2"),
  typology3 = data_all %>% filter(Typology == "Typology3")
)

level_suffix <- c(overall = "", typology1 = "_typology1", typology2 = "_typology2", typology3 = "_typology3")
addcol <- colorRampPalette(c("#032E61", "#FDFDFD", "darkred")); spearman_results <- list(); spearman_summary <- list()

for (lvl in names(level_data)) {
  dat <- level_data[[lvl]] %>% dplyr::select(-city, -Typology)
  names(dat) <- sub("^G", "SDG", names(dat))
  sp <- rcorr(as.matrix(dat), type = "spearman"); cor_mat <- sp$r; p_mat <- sp$P
  
  plot_path <- file.path(analysis_dir, paste0("Spearman_corrplot", level_suffix[lvl], ".png"))
  png(plot_path, width = 11, height = 10, units = "in", res = 1200)
  
  corrplot(cor_mat, method = "circle", type = "upper", col = addcol(500), tl.pos = "n", cl.pos = "r", cl.cex = 1.2,
           p.mat = p_mat, sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.5, diag = FALSE, insig = "label_sig", order = "original")
  
  corrplot(cor_mat, method = "number", type = "lower", col = addcol(500), tl.col = "black", tl.srt = 45, tl.cex = 0.5,
           tl.pos = "lt", number.cex = 0.4, cl.pos = "n", order = "original", add = TRUE, diag = TRUE, insig = "blank")
  dev.off()
  
  idx <- which(upper.tri(cor_mat), arr.ind = TRUE)
  res <- tibble(Var1 = rownames(cor_mat)[idx[, 1]], Var2 = colnames(cor_mat)[idx[, 2]],
                Correlation = cor_mat[idx], P_value = p_mat[idx]) %>%
    filter(!is.na(P_value), P_value < 0.05)
  
  spearman_results[[lvl]] <- res
  
  total_pairs <- choose(ncol(dat), 2)
  
  spearman_summary[[lvl]] <- bind_rows(
    tibble(Type = "Synergy", Group = recode(lvl, overall = "Overall", typology1 = "Typology1", typology2 = "Typology2", typology3 = "Typology3"),
           Ratio = sum(res$Correlation > 0) / total_pairs * 100,
           Average = mean(res$Correlation[res$Correlation > 0])),
    tibble(Type = "Tradeoff", Group = recode(lvl, overall = "Overall", typology1 = "Typology1", typology2 = "Typology2", typology3 = "Typology3"),
           Ratio = sum(res$Correlation < 0) / total_pairs * 100,
           Average = mean(res$Correlation[res$Correlation < 0]))
  )
  write.xlsx(res, file.path(analysis_dir, paste0("significant_spearman", level_suffix[lvl], ".xlsx")), overwrite = TRUE)
}


spearman_summary <- bind_rows(spearman_summary) %>%
  mutate(Type = factor(Type, levels = c("Synergy", "Tradeoff")),
         Group = factor(Group, levels = c("Overall", "Typology1", "Typology2", "Typology3"))) %>%
  arrange(Type, Group) %>%
  mutate(Type = as.character(Type), Group = as.character(Group))

write_xlsx(spearman_summary, file.path(analysis_dir, "Spearman_summary.xlsx"))



#====================================================================================================
#-----------------Network analysis at SDG level for overall level and typology level------------------
#====================================================================================================
# -------------- Calculate correlation intensity among SDG pairs ----------------
# --- Step1: Align indicators with SDGs---
sdg_file <- file.path(analysis_dir, "4_indicator_to_SDG.xlsx"); indicator_to_SDG <- read_excel(sdg_file) %>% transmute(Indicator = as.character(Indicator), SDG = as.character(SDG))
sdgs_available <- indicator_to_SDG %>% distinct(SDG) %>% mutate(n = as.numeric(gsub("SDG", "", SDG))) %>% arrange(n) %>% pull(SDG)

normalize_pairs <- function(df) df %>% mutate(n1 = as.numeric(gsub("SDG", "", Object1)), n2 = as.numeric(gsub("SDG", "", Object2)),
                                              Object1 = paste0("SDG", pmin(n1, n2)), Object2 = paste0("SDG", pmax(n1, n2))) %>% dplyr::select(-n1, -n2)

# Possible indicator pairs are identical for all levels, so calculate them once.
ind_pair_idx <- t(combn(seq_len(nrow(indicator_to_SDG)), 2))
possible_pairs <- tibble(Var1 = indicator_to_SDG$Indicator[ind_pair_idx[, 1]], Var2 = indicator_to_SDG$Indicator[ind_pair_idx[, 2]],
                         Object1 = indicator_to_SDG$SDG[ind_pair_idx[, 1]], Object2 = indicator_to_SDG$SDG[ind_pair_idx[, 2]]) %>% normalize_pairs() %>%
  dplyr::count(Object1, Object2, name = "Possible_Pairs")

sdg_grid <- expand.grid(Object1 = sdgs_available, Object2 = sdgs_available, stringsAsFactors = FALSE) %>% normalize_pairs() %>% distinct(Object1, Object2)
network_results <- list()

for (lvl in names(spearman_results)) {
  correlation_long <- spearman_results[[lvl]]
  final_data <- correlation_long %>%
    left_join(indicator_to_SDG %>% rename(Var1 = Indicator, Object1 = SDG), by = "Var1") %>%
    left_join(indicator_to_SDG %>% rename(Var2 = Indicator, Object2 = SDG), by = "Var2") %>%
    dplyr::select(Object1, Var1, Object2, Var2, Correlation) %>% normalize_pairs()
  write_xlsx(final_data, file.path(analysis_dir, paste0("correlation", ifelse(lvl == "overall", "", sub("typology", "", lvl)), "_with_SDG.xlsx")))
  
  # --- Step2: Calculate average correlation per SDG using correlation_long---
  sdg_pair_scores <- final_data %>% group_by(Object1, Object2) %>% summarise(
    Synergy_Score = if (any(Correlation > 0, na.rm = TRUE)) mean(Correlation[Correlation > 0], na.rm = TRUE) else 0,
    Tradeoff_Score = if (any(Correlation < 0, na.rm = TRUE)) abs(mean(Correlation[Correlation < 0], na.rm = TRUE)) else 0, .groups = "drop")
  Synergy_tradeoff_score <- sdg_grid %>% left_join(sdg_pair_scores, by = c("Object1", "Object2")) %>%
    mutate(Synergy_Score = replace_na(Synergy_Score, 0), Tradeoff_Score = replace_na(Tradeoff_Score, 0))
  write_xlsx(Synergy_tradeoff_score, file.path(analysis_dir, paste0("Synergy_tradeoff_score", ifelse(lvl == "overall", "", sub("typology", "", lvl)), ".xlsx")))
  
  # --- Calculate ratio per indicator using correlation_long. Count how many significant pairs involve each indicator---
  sig_counts_sign <- final_data %>% group_by(Object1, Object2) %>% summarise(Positive_Pairs = sum(Correlation > 0), Negative_Pairs = sum(Correlation < 0), .groups = "drop")
  merged_data <- possible_pairs %>% left_join(sig_counts_sign, by = c("Object1", "Object2")) %>%
    mutate(Positive_Pairs = replace_na(Positive_Pairs, 0L), Negative_Pairs = replace_na(Negative_Pairs, 0L),
           Positive_Ratio = Positive_Pairs / Possible_Pairs, Negative_Ratio = Negative_Pairs / Possible_Pairs)
  
  # --- Calculate intensity per SDG pair using correlation_long and merge data. Ratio * Score fr synergy and trade-off ---
  Intensity_results <- Synergy_tradeoff_score %>% left_join(merged_data, by = c("Object1", "Object2")) %>%
    mutate(Positive_Ratio = replace_na(Positive_Ratio, 0), Negative_Ratio = replace_na(Negative_Ratio, 0),
           Intensity_Synergy = Synergy_Score * Positive_Ratio, Intensity_Tradeoff = Tradeoff_Score * Negative_Ratio)
  network_results[[lvl]] <- Intensity_results
  write_xlsx(Intensity_results, file.path(analysis_dir, paste0("Intensity_results", ifelse(lvl == "overall", "", sub("typology", "", lvl)), ".xlsx")))
}

#----------------Chord diagrams of SDG interaction intensity---------------------
sankey_dir <- file.path(analysis_dir, "NetworkScore_Sankey")
if (!dir.exists(sankey_dir)) dir.create(sankey_dir, recursive = TRUE)

sdg_order <- c(paste0("SDG", 1:13), "SDG15", "SDG16")
sdg_colors <- c("#E5243B", "#DDA63A", "#4C9F38", "#C5192D", "#FF3A21",
                "#26BDE2", "#FCC30B", "#A21942", "#FD6925", "#DD1367",
                "#FD9D24", "#BF8B2E", "#3F7E44", "#56C02B", "#00689D")
names(sdg_colors) <- sdg_order

plot_chord <- function(dat, intensity, panel_label) {
  df_plot <- dat %>% filter(.data[[intensity]] > 0) %>%
    transmute(Object1 = as.character(Object1), Object2 = as.character(Object2),
              Intensity = .data[[intensity]])
  
  plot_order <- sdg_order[sdg_order %in% unique(c(df_plot$Object1, df_plot$Object2))]
  circos.clear()
  chordDiagram(df_plot, grid.col = sdg_colors, order = plot_order, transparency = 0.5)
  mtext(panel_label, side = 3, adj = 0, line = -1, cex = 1.4, font = 2)
}

out_file <- file.path(sankey_dir, "ChordDiagram_All_Typologies.png")
png(out_file, width = 8000, height = 4000, res = 600)
par(mfrow = c(2, 4), mar = c(1, 1, 1.8, 1))

plot_chord(network_results$overall,   "Intensity_Synergy",  "a.")
plot_chord(network_results$typology1, "Intensity_Synergy",  "b.")
plot_chord(network_results$typology2, "Intensity_Synergy",  "c.")
plot_chord(network_results$typology3, "Intensity_Synergy",  "d.")

plot_chord(network_results$overall,   "Intensity_Tradeoff", "e.")
plot_chord(network_results$typology1, "Intensity_Tradeoff", "f.")
plot_chord(network_results$typology2, "Intensity_Tradeoff", "g.")
plot_chord(network_results$typology3, "Intensity_Tradeoff", "h.")

dev.off()
circos.clear()


# --- Combine dominance scores from three typologies ---
dom_list <- lapply(1:3, function(i) network_results[[paste0("typology", i)]] %>% transmute(Object1, Object2, !!paste0("dominance", i) := Intensity_Synergy - Intensity_Tradeoff))
Dominance_results <- Reduce(function(x, y) full_join(x, y, by = c("Object1", "Object2")), dom_list) %>%
  mutate(SDG1_num = as.numeric(gsub("SDG", "", Object1)), SDG2_num = as.numeric(gsub("SDG", "", Object2))) %>% arrange(SDG1_num, SDG2_num) %>%
  mutate(num = row_number()) %>% dplyr::select(num, Object1, Object2, dominance1, dominance2, dominance3)
write_xlsx(Dominance_results, file.path(analysis_dir, "SDG_pair_dominance_typologies.xlsx"))



#--------------------------------------------------------------
#-----------Comparing hub scores among typologies----------------
# Use an undirected, weighted network (no duplicated/bidirectional edges) and compute weighted HITS scores because correlation is symmetric.
# Within-SDG pairs (e.g., SDG1-SDG1) are retained as self-loops so that interactions among indicators within the same SDG contribute to hub scores.
calc_scores <- function(dat, weight_col, all_sdgs) {
  edges <- dat %>% filter(.data[[weight_col]] > 0) %>%
    transmute(from = Object1, to = Object2, weight = .data[[weight_col]])
  
  if (nrow(edges) == 0) return(data.frame(SDG = all_sdgs, HubScore = 0))
  
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = data.frame(name = all_sdgs))
  hs <- hub_score(g, weights = E(g)$weight)$vector
  
  data.frame(SDG = names(hs), HubScore = unname(hs))
}

hub_summary <- list()

for (lvl in names(network_results)) {
  synergy_results <- calc_scores(network_results[[lvl]], "Intensity_Synergy", sdgs_available)
  tradeoff_results <- calc_scores(network_results[[lvl]], "Intensity_Tradeoff", sdgs_available)
  
  hub_summary[[lvl]] <- synergy_results %>%
    select(SDG, Synergy = HubScore) %>%
    left_join(tradeoff_results %>% select(SDG, Tradeoff = HubScore), by = "SDG") %>%
    mutate(Level = recode(lvl, overall = "Overall", typology1 = "Typology1", typology2 = "Typology2", typology3 = "Typology3")) %>%
    rename(Object = SDG) %>%
    select(Level, Object, Synergy, Tradeoff)
}

hub_summary <- bind_rows(hub_summary) %>%
  mutate(SDG_num = as.numeric(gsub("SDG", "", Object))) %>%
  arrange(factor(Level, levels = c("Overall", "Typology1", "Typology2", "Typology3")), SDG_num) %>%
  select(-SDG_num)

write.xlsx(hub_summary, file.path(analysis_dir, "HubScore_summary.xlsx"), overwrite = TRUE)


