rm(list = ls())

# Packages used across the script
library(readxl); library(dplyr); library(tidyr); library(tibble); library(writexl); library(openxlsx)
library(DMwR2); library(Metrics); library(ggplot2); library(NbClust); library(corrplot); library(Hmisc); library(igraph); library(grid)
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

set.seed(123)
nb_result <- NbClust(data = as.data.frame(data_normalized), min.nc = 2, max.nc = 12, method = "ward.D2", index = "all")
nb_support <- as.data.frame(table(nb_result$Best.nc[1, ])); names(nb_support) <- c("Clusters", "N_indices"); nb_support$Clusters <- as.numeric(as.character(nb_support$Clusters))
b_support <- nb_support %>% filter(!is.na(Clusters)); best_nc_mode <- nb_support$Clusters[which.max(nb_support$N_indices)]; cat("NbClust recommended clusters:", best_nc_mode, "\n")
write.csv(nb_support, file.path(analysis_dir, "NbClust_cluster_support.csv"), row.names = FALSE)

optimal_clusters <- best_nc_mode; set.seed(123); kmeans_result <- kmeans(data_normalized, centers = optimal_clusters, nstart = 25)
data_cluster$Typology <- factor(kmeans_result$cluster, labels = paste0("Typology", 1:optimal_clusters))
centroids <- as.data.frame(kmeans_result$centers); centroids$Typology <- paste0("Typology", 1:nrow(centroids)); centroids <- centroids %>% dplyr::select(Typology, everything())
write_xlsx(centroids, file.path(analysis_dir, "Indicator_centroids.xlsx"))

city_clusters <- data.frame(City = data_cluster[[1]], Typology = kmeans_result$cluster); write_xlsx(city_clusters, file.path(analysis_dir, "Cities_by_typology.xlsx"))
clustered_file <- file.path(output_dir, "Indicator_55_signed_imputed_with_typology.xlsx")
write_xlsx(data_cluster, clustered_file)



#=============================================================================================
#----------Spearman analysis at indicator level for overall level and typology levels----------
#=============================================================================================
# Read one dataset containing city, 54 indicators, and Typology
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
  sp <- rcorr(as.matrix(dat), type = "spearman"); cor_mat <- sp$r; p_mat <- sp$P
  
  plot_path <- file.path(result_dir, paste0("Spearman_corrplot", level_suffix[lvl], ".png"))
  png(plot_path, width = 10, height = 10, units = "in", res = 1200)
  
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


