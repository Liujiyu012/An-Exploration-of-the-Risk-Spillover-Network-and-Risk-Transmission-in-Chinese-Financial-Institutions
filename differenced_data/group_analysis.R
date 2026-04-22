# 加载必要的包
library(tidyverse)
library(readxl)
library(dplyr)

# 读取风险溢出矩阵
file_path <- "/Users/liujiyu/Documents/RUC/2025课程学习/2025-金融理论与政策-马勇/刘及雨-中国金融机构风险溢出网络与风险传染探究/data/spoillover_data.csv"  # 替换为实际文件路径
risk_matrix <- read_csv(file_path)

# 手动设置行名（假设第一列为行名列）
rownames(risk_matrix) <- risk_matrix[[1]]  # 将第一列设为行名
risk_matrix <- risk_matrix[, -1]  # 删除第一列，因为它已作为行名

# 确认矩阵大小
print(dim(risk_matrix))

# 读取机构分类信息
file_path <- "/Users/liujiyu/Documents/RUC/2025课程学习/2025-金融理论与政策-马勇/刘及雨-中国金融机构风险溢出网络与风险传染探究/data/金融机构对照.xlsx"  # 替换为实际文件路径
data <- read_excel(file_path)

# 提取分类数据
categories_bank <- data[, 3]   # 第三列：银行、证券、保险的分类
categories_louvain <- data[, 4]  # 第四列：Louvain的分类

# 创建一个分类数据框
category_df <- data.frame(
  机构 = data[[1]],  # 机构名称
  英文缩写 = data[[2]],  # 英文缩写
  类别_银行证券保险 = categories_bank,
  类别_Louvain = categories_louvain
)

# 确保分类信息与风险溢出矩阵中的机构对应
# 将中文名称（机构）转换为英文缩写
category_df <- category_df[match(colnames(risk_matrix), category_df$英文缩写), ]

# 计算组别间的总溢出强度函数
calculate_total_spillover <- function(matrix, category_df, category_col) {
  unique_categories <- unique(category_df[[category_col]])
  spillover_matrix <- matrix(0, nrow = length(unique_categories), ncol = length(unique_categories))
  rownames(spillover_matrix) <- unique_categories
  colnames(spillover_matrix) <- unique_categories
  
  for (i in 1:length(unique_categories)) {
    for (j in 1:length(unique_categories)) {
      # 获取类别i和类别j的所有机构
      group_i <- category_df[category_df[[category_col]] == unique_categories[i], ]
      group_j <- category_df[category_df[[category_col]] == unique_categories[j], ]
      
      # 计算从类别i到类别j的总溢出强度
      indices_i <- match(group_i$英文缩写, rownames(matrix))
      indices_j <- match(group_j$英文缩写, colnames(matrix))
      
      # 检查是否有NA值（即找不到匹配的机构）
      if (any(is.na(indices_i)) | any(is.na(indices_j))) {
        warning(paste("Warning: Some institutions in category", unique_categories[i], "or", unique_categories[j], "could not be found in the matrix."))
      }
      
      # 只保留有效的索引，确保没有NA值
      valid_indices_i <- indices_i[!is.na(indices_i)]
      valid_indices_j <- indices_j[!is.na(indices_j)]
      
      # 如果至少有一个有效索引，计算溢出强度
      if (length(valid_indices_i) > 0 && length(valid_indices_j) > 0) {
        spillover_matrix[i, j] <- sum(matrix[valid_indices_i, valid_indices_j], na.rm = TRUE)
      } else {
        spillover_matrix[i, j] <- NA  # 如果没有有效的索引，设置为NA
      }
    }
  }
  
  return(spillover_matrix)
}


# 按银行、证券、保险分类计算总溢出强度
spillover_by_bank_sec_ins <- calculate_total_spillover(risk_matrix, category_df, "类别_银行证券保险")

# 按Louvain分类计算总溢出强度
spillover_by_louvain <- calculate_total_spillover(risk_matrix, category_df, "类别_Louvain")

# 输出结果
print("按银行、证券、保险分类计算的总溢出强度：")
print(spillover_by_bank_sec_ins)

print("按Louvain分类计算的总溢出强度：")
print(spillover_by_louvain)
