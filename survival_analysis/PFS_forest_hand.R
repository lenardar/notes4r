# 导入必要的包
library(tidyverse)
library(grid)

# 导入数据
PFS_forest <- read_csv("./PFS_forest.csv")
data <- PFS_forest

# 定义x轴下限与上限
lower_lim <- 0.5
upper_lim <- 2.0

# 数据处理
data <- data %>% 
  mutate(
    index = -row_number(),    # 保证一行一个CI条，去负数保证从上到下排列
    HR = as.numeric(HR),      # 将NA转化为NA_real
    LCI = as.numeric(LCI),    # 将NA转化为NA_real
    UCI = as.numeric(UCI),    # 将NA转化为NA_real
    tr_LCI = if_else(LCI < lower_lim, lower_lim, LCI),  # 小于下限设为下限
    tr_UCI = if_else(UCI > upper_lim, upper_lim, UCI),  # 大于上限设为上限
    # 设置CI条形状
    CI_colors = case_when(
      Treat == "IC/PC" ~ "#015493",
      Treat == "Sacituzumab govitecan" ~ "#019092",
      Treat == "Trastuzumab deruxtecan" ~ "#F4A99B"
    ),
    # 设置CI条颜色
    CI_shapes = case_when(
      Treat == "IC/PC" ~ 16,
      Treat == "Sacituzumab govitecan" ~ 17,
      Treat == "Trastuzumab deruxtecan" ~ 18
    ),
    # 设置CI条点大小
    CI_point_sizes = 3
  )

# 根据截断设置数据集（用于绘制箭头等）
data_normal <- data %>% filter(LCI >= lower_lim, UCI <= upper_lim)
data_left   <- data %>% filter(LCI < lower_lim)
data_right  <- data %>% filter(UCI > upper_lim)

# 设置参数
CI_width <- 0.2        # CI条竖线长度
CI_linewidth <- 0.3    # CI条横线粗细
arrow_size <- 0.07     # 截断箭头大小
backgroud_size <- 10   # 背景色条高度
# 设置背景颜色
backgroud_colors <- c(
  "white", "white", "grey", "white", "grey",
  "white", "white", "grey", "white",
  "white", "white", "grey", "white", "grey",
  "white", "white", "grey", "white"
)
# 设置标签列字体
Label_fontface <- c(
  "bold", "bold", "plain", "plain", "plain",
  "bold", "bold", "plain", "plain",
  "bold", "bold", "plain", "plain", "plain",
  "bold", "bold", "plain", "plain"
)
# 标签列位置
Label_position = -5
# HR列位置
HR_posiotion <- -2.3


# 绘制森林图
data %>% 
  ggplot(aes(y = index, x = HR, xmin = tr_LCI, xmax = tr_UCI)) +
  # 设置背景
  geom_hline(
    aes(yintercept = index),
    linewidth = backgroud_size, 
    color = backgroud_colors
  ) +
  # 绘制CI数据点
  geom_point(
    aes(shape = shapes),
    size = data$CI_point_sizes,
    color = data$CI_colors,
    shape = data$CI_shapes,
    show.legend = FALSE
    ) +
  # 绘制正常误差条（未截断）
  geom_errorbar(
    data = data_normal,
    aes(y = index, x = HR, xmin = tr_LCI, xmax = tr_UCI),
    width = CI_width, linewidth = CI_linewidth,
    color = data_normal$CI_colors,
    show.legend = FALSE
  ) +
  # 下端截断的误差条：使用geom_segment添加左侧箭头
  geom_segment(
    data = data_left,
    aes(y = index, yend = index, x = tr_LCI, xend = tr_UCI),
    linewidth = CI_linewidth,
    color = data_left$CI_colors,
    arrow = arrow(angle = 20, length = unit(arrow_size, "inches"),
                  ends = "first", type = "closed")
  ) +
  geom_errorbar(
    data = data_left,
    aes(y = index, x = HR, xmin = HR, xmax = tr_UCI),
    width = CI_width, linewidth = CI_linewidth,
    color = data_left$CI_colors,
    show.legend = FALSE
  ) +
  # 上端截断的误差条：使用geom_segment添加右侧箭头
  geom_segment(
    data = data_right,
    aes(y = index, yend = index, x = tr_LCI, xend = tr_UCI),
    linewidth = CI_linewidth,
    color = data_right$CI_colors,
    arrow = arrow(angle = 20, length = unit(arrow_size, "inches"),
                  ends = "last", type = "closed")
  ) +
  geom_errorbar(
    data = data_right,
    aes(y = index, x = HR, xmin = tr_LCI, xmax = HR),
    width = CI_width, linewidth = CI_linewidth,
    color = data_right$CI_colors,
    show.legend = FALSE
  ) +
  # 标签列
  geom_text(
    aes(label = Label),
    x = log(2^(Label_position)),
    hjust = 0,
    size = 3,
    color = "black",
    fontface = Label_fontface 
  ) +
  # HR列
  # 添加标签列标题
  annotate(
    "text", 
    label = "NMA HR", 
    x = log(2^0.35), 
    y = -0.75,
    size = 3
  ) +
  geom_text(
    aes(label = `NMA HR`),
    x = log(2^(HR_posiotion)),
    hjust = 0,
    size = 3,
    color = "black",
    fontface = "bold"
  ) +
  # CI条标题
  annotate(
    "text", 
    label = "NMA Hazard Ratio (95% CI)", 
    x = 1, 
    y = -0.75, 
    size = 3
  ) +
  # 设置x轴（注意负数标签在对数坐标中无效，但通过coord_cartesian(clip="off")和plot.margin可以显示在外部）
  scale_x_continuous(
    limits = c(2^(-5), 2.0),
    breaks = c(0.5, 1.0, 2.0),
    expand = expansion(mult = 0.02),
    guide = guide_axis_truncated(trunc_lower = 0.5, trunc_upper = 2),
    trans = "log"
  ) +
  # 设置主题并增大左边距以避免注释被裁剪
  theme(
    axis.text = element_text(color = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    # aspect.ratio = 6/8,
    plot.margin = margin(t = 20, r = 10, b = 20, l = 10)  # 大幅增加底部边距
  ) +
  coord_cartesian(clip = "off") +
  # 添加坐标轴标签
  annotation_custom(
    grob = textGrob("favor datopotamab deruxtecan <", gp = gpar(fontsize = 8)),
    xmin = log(0.25), xmax = log(2),
    ymin = unit(-20, "lines"), ymax = unit(-20, "lines")
  ) +
  annotation_custom(
    grob = textGrob("> favor comparator", gp = gpar(fontsize = 8)),
    xmin = log(0.8), xmax = log(2),
    ymin = unit(-20, "lines"), ymax = unit(-20, "lines")
  ) +
  # 添加参考线
  geom_segment(
    x = log(1),
    xend = log(1),
    y = -19,
    yend = -1.5,
    linetype = "dashed", linewidth = 0.3)