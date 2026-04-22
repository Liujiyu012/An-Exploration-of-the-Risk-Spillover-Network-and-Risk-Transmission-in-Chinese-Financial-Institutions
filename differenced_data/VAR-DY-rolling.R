library(readxl)
library(tidyverse)
library(zoo)
library(vars)
library(ConnectednessApproach)
# 路径
file_path <- "/Users/liujiyu/Documents/RUC/2025课程学习/2025-金融理论与政策-马勇/刘及雨-中国金融机构风险溢出网络与风险传染探究/differenced_data/tvpdata_log.xlsx"

# 读取 Excel
data_raw <- read_excel(file_path)

# 确保 Date 正确
data_raw <- data_raw %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

# 检查重复日期（稳妥）
data_raw <- data_raw %>%
  group_by(Date) %>%
  summarise(across(everything(), mean), .groups = "drop")

# 转成 zoo（时间索引）
zoo_data <- zoo(
  x = as.matrix(data_raw[, -1]),
  order.by = data_raw$Date
)
window_size <- 300   # 滚动窗口长度（300 个交易日）
step_size   <- 30    # 步长（30 天）

n_obs <- nrow(zoo_data)

spillover_list <- list()
date_list <- c()
counter <- 1

for (start_idx in seq(1, n_obs - window_size + 1, by = step_size)) {
  
  end_idx <- start_idx + window_size - 1
  window_data <- zoo_data[start_idx:end_idx, ]
  
  window_end_date <- index(window_data)[nrow(window_data)]
  
  cat(
    "Processing window ending at:",
    as.Date(window_end_date, origin = "1970-01-01"),
    "\n"
  )
  
  # ---------- ① 删零方差 ----------
  var_sd <- apply(window_data, 2, sd, na.rm = TRUE)
  window_data <- window_data[, var_sd > 0]
  
  if (ncol(window_data) < 3) {
    cat("Skip: too few variables\n")
    next
  }
  
  # ---------- ② 标准化（关键！） ----------
  window_data <- scale(window_data)
  window_data <- zoo(window_data, order.by = index(zoo_data)[start_idx:end_idx])
  
  # ---------- ③ 安全计算（允许失败） ----------
  result <- tryCatch({
    
    conn <- ConnectednessApproach(
      x = window_data,
      model = "VAR",
      connectedness = "Time",
      nlag = 1,     # ⭐ 非常重要：p=1
      nfore = 10
    )
    
    list(success = TRUE, table = conn$TABLE)
    
  }, error = function(e) {
    
    cat("❌ Window failed, skipped\n")
    list(success = FALSE)
    
  })
  
  if (!result$success) next
  
  spillover_list[[counter]] <- result$table
  date_list[counter] <- as.character(
    as.Date(window_end_date, origin = "1970-01-01")
  )
  
  counter <- counter + 1
}

spillover_df <- map2_df(
  spillover_list,
  date_list,
  ~ as.data.frame(.x) %>%
    mutate(Date = .y, From = rownames(.x))
)

write.csv(
  spillover_df,
  "dynamic_spillover_matrices.csv",
  row.names = FALSE
)



