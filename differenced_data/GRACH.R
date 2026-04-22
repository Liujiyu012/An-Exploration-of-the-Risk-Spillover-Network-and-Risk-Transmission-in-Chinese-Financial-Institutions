###导入数据###
#options(pkgType = "binary")
install.packages("tidyverse", type = "binary")
install.packages('ggthemes')
install.packages('forecast')
install.packages('tseries')
install.packages('gridExtra')
install.packages('rugarch')
install.packages('FinTS')
install.packages('zoo')
install.packages('base')
install.packages('moments')
install.packages("quantmod")
install.packages("xts", type = "binary")
install.packages("rugarch", repos = "https://cran.rstudio.com/")
install.packages("e1071")


library(openxlsx)
data=read.xlsx('/Users/liujiyu/Documents/RUC/2025课程学习/2025-金融理论与政策-马勇/刘及雨-中国金融机构风险溢出网络与风险传染探究/data/filtered_workdata_log.xlsx')
library(tidyverse)
library(ggthemes)
library(forecast)
library(tseries)
library(gridExtra)
library(rugarch)
library(FinTS)
library(moments)
library(rugarch)
library(e1071)

# 转为时间序列对象（ts）
data_ts <- list(
  ABC = ts(data$ABC), 
  AJG = ts(data$AJG), 
  BJB = ts(data$BJB), 
  BOCOM = ts(data$BOCOM),
  CIB = ts(data$CIB),
  CITICS = ts(data$CITICS),
  CLC = ts(data$CLC),
  CMB = ts(data$CMB),
  CMBC = ts(data$CMBC),
  COC = ts(data$COC),
  CSC = ts(data$CSC),
  CYS = ts(data$CYS),
  DBS = ts(data$DBS),
  DFC = ts(data$DFC),
  ECP = ts(data$ECP),
  GFS = ts(data$GFS),
  GHS = ts(data$GHS),
  GIC = ts(data$GIC),
  GJS = ts(data$GJS),
  GYS = ts(data$GYS),
  HCX = ts(data$HCX),
  HDS = ts(data$HDS),
  HJC = ts(data$HJC),
  HXB = ts(data$HXB),
  HXG = ts(data$HXG),
  JDI = ts(data$JDI),
  JLG = ts(data$JLG),
  JYT = ts(data$JYT),
  MWC = ts(data$MWC), 
  NBB = ts(data$NBB),
  NJB = ts(data$NJB),
  PAB = ts(data$PAB),
  PING = ts(data$PING),
  SGXA = ts(data$SGXA),
  SPDB = ts(data$SPDB),
  SWZ = ts(data$SWZ),
  SXZ = ts(data$SXZ),
  TPAC = ts(data$TPAC),
  XCF = ts(data$XCF),
  XGD = ts(data$XGD),
  XLF = ts(data$XLF),
  XYR = ts(data$XYR),
  YTG = ts(data$YTG),
  YXC = ts(data$YXC),
  ZDO = ts(data$ZDO)
)




#绘制收益率时间序列图
#install.packages('zoo')
#install.packages('base')
#library(zoo)
#plot(ABC)

#计算相关系数矩阵
#pcor=cor(data_ts)
#pcor

### 创建循环对每个变量进行处理 ###
for (var_name in names(data_ts)) {
  cat("\nProcessing variable:", var_name, "...\n")
  
  # 提取当前变量
  current_var <- data_ts[[var_name]]
  
  ### 描述性统计 ###
  summary_stats <- summary(current_var)
  var_sd <- sd(current_var)
  var_skew <- skewness(current_var)
  var_kurtosis <- kurtosis(current_var)
  var_var <- var(current_var)
  
  ### Shapiro正态性检验 ###
  shapiro_test <- tryCatch(
    shapiro.test(current_var),
    error = function(e) list(p.value = NA)
  )
  
  ### ADF单位根检验 ###
  adf_result <- tryCatch(
    adf.test(current_var),
    error = function(e) list(p.value = NA)
  )
  
  ### 自动ARIMA定阶 ###
  p <- 1
  q <- 1
  
  ### 构建并拟合GARCH模型 ###
  model.spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(p, q))
  )
  
  garch_fit <- tryCatch(
    ugarchfit(spec = model.spec, data = current_var, solver = "solnp"),
    error = function(e) {
      cat("Error fitting GARCH model:", e$message, "\n")
      return(NULL)
    }
  )
  
  # 如果GARCH模型拟合成功，提取波动率值
  if (!is.null(garch_fit)) {
    fitted_values <- fitted(garch_fit)
    
    # 检查 fitted_values 的维度
    if (length(dim(fitted_values)) == 2) {
      # 如果是二维对象，给它添加列名
      colnames(fitted_values) <- "sigma"
    } else {
      # 如果是单列对象，直接转为数据框并命名列
      fitted_values <- data.frame(sigma = fitted_values)
    }
    
    # 从拟合结果中提取 sigt（正向波动率）这一列
    volatility_values <- fitted_values$sigma
    dates <- index(fitted_values)  # 获取日期
    
    # 构建一个数据框，包含日期和波动率
    volatility_data <- data.frame(Date = dates, Volatility = volatility_values)
    
    # 打印一些描述性统计信息
    cat("\nSummary statistics for", var_name, ":\n")
    print(summary_stats)
    cat("\nStandard deviation:", var_sd, "\n")
    cat("Skewness:", var_skew, "\n")
    cat("Kurtosis:", var_kurtosis, "\n")
    cat("Variance:", var_var, "\n")
    
    # Shapiro-Wilk正态性检验结果
    cat("\nShapiro-Wilk normality test p-value:", shapiro_test$p.value, "\n")
    
    # ADF单位根检验结果
    cat("\nADF test p-value:", adf_result$p.value, "\n")
    
    # 保存波动率数据为 Excel 文件
    write.xlsx(volatility_data, 
               paste("/Users/liujiyu/Documents/RUC/2025课程学习/2025-金融理论与政策-马勇/刘及雨-中国金融机构风险溢出网络与风险传染探究/differenced_data/GARCH_result/", var_name, ".xlsx"))
  } else {
    cat("\nGARCH model fit failed for", var_name, "\n")
  }
}


cat("\nProcessing completed.\n")

