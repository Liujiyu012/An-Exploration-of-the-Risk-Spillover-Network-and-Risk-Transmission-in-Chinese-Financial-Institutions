install.packages("devtools")  
install_github("cran/ConnectednessApproach")
install.packages("knitr") 
library(devtools) 
library(knitr) 
library(ConnectednessApproach)
library(openxlsx)
library(zoo)

path = file.path("/Users/liujiyu/Documents/RUC/2025课程学习/2025-金融理论与政策-马勇/刘及雨-中国金融机构风险溢出网络与风险传染探究/differenced_data/tvpdata_log.xlsx") # select dy2012.csv
volatility = read.xlsx(path)
head(volatility)
time_index <- as.Date(as.character(volatility[[1]]), format = "%Y%m%d")
data_matrix <- as.matrix(volatility[, -1])
zoo_data <- zoo(data_matrix, order.by = time_index)
kable(head(zoo_data))

dca = ConnectednessApproach(zoo_data,
                            nlag=4,
                            nfore=10,
                            model="VAR")

fit = VAR(zoo_data, configuration=list(nlag=4))
#dca = TimeConnectedness(fit$B, fit$Q, nfore=10)
kable(dca$TABLE)
write.csv(dca$TABLE, "/Users/liujiyu/Documents/RUC/2025课程学习/2025-金融理论与政策-马勇/刘及雨-中国金融机构风险溢出网络与风险传染探究/differenced_data/output.csv")
# 稳健性检验：不同滞后阶数
dca_lag2 = ConnectednessApproach(zoo_data, nlag=2, nfore=10, model="VAR")
dca_lag6 = ConnectednessApproach(zoo_data, nlag=6, nfore=10, model="VAR")

# 比较结果
cat("滞后2阶总连通性:", ifelse(!is.null(dca_lag2$TCI), dca_lag2$TCI, "N/A"), "\n")
cat("滞后4阶总连通性:", ifelse(!is.null(dca$TCI), dca$TCI, "N/A"), "\n")
cat("滞后6阶总连通性:", ifelse(!is.null(dca_lag6$TCI), dca_lag6$TCI, "N/A"), "\n")
