# 安装和加载car包
# install.packages("car")

library(car)
# 设置随机种子以确保结果可重复
set.seed(123)
# 生成50个特征，每个特征包含100个样本
num_features <- 50
num_samples <- 100
data <- as.data.frame(matrix(rnorm(num_samples * num_features), nrow = num_samples, ncol = num_features))
# 为特征命名
colnames(data) <- paste0("V", 1:num_features)
# 生成响应变量
data$Y <- rnorm(num_samples)
# 构建线性回归模型
model <- lm(Y ~ ., data = data)
# 计算VIF值
vif_values <- vif(model)
# 打印VIF值
print(vif_values)

