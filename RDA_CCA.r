
library(vegan)
library(ggplot2)
library(dplyr)

data(varespec)
data(varechem)

## or correlation-based scores in PCA/RDA

###########################CCA##########################################
mod.cca=cca(varespec~N+P, varechem)
vare.cca <- scores(mod.cca, scaling = "sites", correlation = TRUE)

# 使用anova.cca函数进行Permutation Test并获得p值
anova_result <- anova(mod.cca, permutations = 999)
anova_result.p=anova_result$`Pr(>F)`[1]


vare.cca.dot=vare.cca$sites%>% data.frame()
vare.cca.dot$dot.name=rownames(vare.cca.dot)
vare.cca.dot$Group=c(rep("A", 10),rep("B", 14) )


vare.cca.arrow=vare.cca$biplot%>% data.frame()
vare.cca.arrow$arrow.name=rownames(vare.cca.arrow)
  
maxlen = max(nchar(vare.cca.arrow$arrow.name))


# 创建图层 p1 并在其上追加散点图
p1 <- ggplot() +
  # 添加箭头图层，并将箭头颜色设置为红色
  geom_segment(data = vare.cca.arrow, aes(x = 0, y = 0, xend = CCA1, yend = CCA2, label = arrow.name), 
               arrow = arrow(length = unit(0.1, "inches")), color = "red") +
  geom_text(data = vare.cca.arrow, aes(x = CCA1, y = CCA2, label = arrow.name), 
            hjust = 0.2, vjust = 0, nudge_x = 0.05, nudge_y = 0.05, check_overlap = TRUE, show.legend = FALSE) +
  # 添加散点图层
  geom_point(data = vare.cca.dot, aes(x = CCA1, y = CCA2, color = Group), size = 3) +
  geom_text(data = vare.cca.dot, aes(x = CCA1, y = CCA2, label = dot.name), 
            hjust = 0.2, vjust = 0, nudge_x = 0.05, nudge_y = 0.05, check_overlap = TRUE, show.legend = FALSE) +
  theme_bw(base_family = 'serif', base_size = 12, base_line_size = 0.5) +
  labs(x = "CCA1", y = "CCA2", title = "",subtitle  =paste0("Permutation Test P-value:",anova_result.p)) 

# 显示组合后的图
print(p1)

###########################RDA##########################################
mod.rda=rda(varespec~N+P, varechem)
vare.rda <- scores(mod.rda, scaling = "sites", correlation = TRUE)

# 使用anova.cca函数进行Permutation Test并获得p值
anova_result <- anova(mod.rda, permutations = 999)
anova_result.p=anova_result$`Pr(>F)`[1]

vare.rda.dot=vare.rda$sites%>% data.frame()
vare.rda.dot$dot.name=rownames(vare.rda.dot)
vare.rda.dot$Group=c(rep("A", 10),rep("B", 14) )

vare.rda.arrow=vare.rda$biplot%>% data.frame()
vare.rda.arrow$arrow.name=rownames(vare.rda.arrow)

maxlen = max(nchar(vare.rda.arrow$arrow.name))

# 创建图层 p1 并在其上追加散点图
p2 <- ggplot() +
  # 添加箭头图层，并将箭头颜色设置为红色
  geom_segment(data = vare.rda.arrow, aes(x = 0, y = 0, xend = RDA1, yend = RDA2, label = arrow.name), 
               arrow = arrow(length = unit(0.1, "inches")), color = "red") +
  geom_text(data = vare.rda.arrow, aes(x = RDA1, y = RDA2, label = arrow.name), 
            hjust = 0.2, vjust = 0, nudge_x = 0.05, nudge_y = 0.05, check_overlap = TRUE, show.legend = FALSE) +
  # 添加散点图层
  geom_point(data = vare.rda.dot, aes(x = RDA1, y = RDA2, color = Group), size = 3) +
  geom_text(data = vare.rda.dot, aes(x = RDA1, y = RDA2, label = dot.name), 
            hjust = 0.2, vjust = 0, nudge_x = 0.05, nudge_y = 0.05, check_overlap = TRUE, show.legend = FALSE) +
  theme_bw(base_family = 'serif', base_size = 12, base_line_size = 0.5) +
  labs(x = "RDA1", y = "RDA2", title = "",subtitle  =paste0("Permutation Test P-value:",anova_result.p)) 


library(patchwork)
p1+p2






