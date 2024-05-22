

# 加载必要的包
library(vegan)
library(ggplot2)
# 创建OTUCer.hel数据框（假设有5个样本和3个物种）
OTUCer.hel <- data.frame(
  Sample1 = c(10, 20, 30),
  Sample2 = c(15, 25, 35),
  Sample3 = c(20, 30, 40),
  Sample4 = c(25, 35, 45),
  Sample5 = c(30, 40, 50)
)
rownames(OTUCer.hel) <- c("Species1", "Species2", "Species3")
OTUCer.hel=data.frame(t(OTUCer.hel))

# 创建SampleMetadata数据框
SampleMetadata <- data.frame(
  Microhabitat = c("A", "B", "A", "B", "A"),
  Season = c("Spring", "Spring", "Autumn", "Autumn", "Spring"),
  TreeSpecies = c("Oak", "Oak", "Oak", "Maple", "Maple")
)
rownames(SampleMetadata) <- paste0("Sample", 1:5)

# 进行变异分区分析
selected.feature <- "Microhabitat"
Others.features <- c("Season", "TreeSpecies")

# Cervarp.all <- varpart(OTUCer.hel, ~ selected.fecture,  ~Others.features, data = SampleMetadata)

Others.features.all <- c("Season", "TreeSpecies","Microhabitat")
# 定义变量
contributions = list()
for(i in Others.features.all){
  print(i)
  # selected.feature <- "Microhabitat"
  selected.feature <- i
  
  Others.features <- Others.features.all[!Others.features.all %in% selected.feature]
  print(Others.features)
  
  # 进行变异分区分析
  OTUCer.hel=data.frame(OTUCer.hel)
  Cervarp.all <- varpart(OTUCer.hel, as.formula(paste("~", selected.feature)), as.formula(paste("~", paste(Others.features, collapse = "+"))), data = SampleMetadata)
  contribution = Cervarp.all$part[["fract"]]$Adj.R.squared[1]
  
  # Cervarp.all$part[["indfract"]]$Adj.R.squared = Cervarp.all$part[["indfract"]]$Adj.R.squared *100 
  contributions[i]=contribution
  
  # 查看结果
  # print(Cervarp.all)
  venny.tables= Cervarp.all$part[["indfract"]] 
  # 筛选venny.tables中Testable为TRUE的行
  venny.tables = venny.tables[venny.tables$Testable, ]
  # 为venny.tables添加一列，用于显示百分比
  # venny.tables$Adj.R.squared = paste0(round(venny.tables$Adj.R.squared, 2), "%")
  pdf(paste0("venn_", selected.feature, ".pdf"), width = 6, height = 6)
  plot(Cervarp.all, digits = 2, Xnames = c(selected.feature, "Others"), bg = c('navy', 'tomato', 'green'))
  dev.off()
  # 保存venn图
  ggsave(paste0("venn_", selected.feature, ".png"), device = "png", dpi = 300, width = 6, height = 6)  
  
}


# 创建数据框
df <- data.frame(
  Feature = names(contributions),
  Contribution = unlist(contributions)
)


# 使用ggplot绘制条形图
barplot.contri <- ggplot(df, aes(y = reorder(Feature, Contribution), x = Contribution)) +
  geom_bar(stat = "identity", fill = "red", width = 0.3) +
  xlab("Explanatory(%)") +
  ylab("") +
  ggtitle("Environmental Contribution Analysis") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("barplot_contribution.png", plot = barplot.contri, device = "png", dpi = 300, width = 6, height = nrow(df)*0.5+2)
