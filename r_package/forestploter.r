library(forestploter)
library(tidyverse)

# 设置工作目录为当前文件夹
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 导入数据集
data_simple <- read_csv("./data/PFS_forest_simple.csv")

# 合成HR (LCI, UCI)
data_simple$`NMA Hazard Ratio (95% CI)` <-
  paste(rep(" ", 40), collapse = " ")  # 生成空白占位符

# 创建两列空格命名的空白列
data_simple$` ` <-
  paste(rep(" ", 40), collapse = " ")
data_simple$`  ` <-
  paste(rep(" ", 20), collapse = " ")

forest <- forest(
  data_simple[, c("Label", " ", "NMA HR", "  ", "NMA Hazard Ratio (95% CI)")],  # 选择列
  est = data_simple$HR,                # 效应量列
  lower = data_simple$LCI,           # CI下限列
  upper = data_simple$UCI,           # CI上限列
  ci_column = 5,                # 空白列的位置（第3列绘制CI条）
  ref_line = 1,                 # 参考线（HR=1）
  xlim = c(0.5, 2.0),           # X轴范围
  ticks_at = c(0.5, 1.0, 2.0),  # X轴刻度
)

print(forest)