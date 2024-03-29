load('/Users/mbp/Documents/DDS/DS/DVUL/Assignment 1/plastic.Rda')
View(plastic)

# Country: Country name, text.
# Code: 3 Letter country code abbreviation from the Country variable, text.
# Region: Region of the world, categorical with 7 levels.
# GDP: GDP per capita in USD from 2016 figures, numerical.
# IncomeStatus: World Bank income category, categorical with levels LIC (low income), LMC (low-middle income), UMC (high-middle income), HIC (high-income).
# Population: 2016 Population, numerical.
# CoastalPopPC: Percentage of population living in areas where elevation is below 5 metres, numerical in the range [0,100].
# UrbanPopPC: Percentage of population living in urban areas, numerical in the range [0,100].
# PWaste: Amount of plastic waste per capita in kg/day, numerical. 
# MPWaste: Amount of mismanaged* plastic waste per capita in kg/day, numerical. 

############################################################################
# Q1
hist(plastic$PWaste,freq=FALSE,breaks = 20,main='Histogram of PWaste')
rug(plastic$PWaste)
hist(plastic$MPWaste,freq=FALSE,breaks = 20,main='Histogram of MPWaste')
rug(plastic$MPWaste)

# boxplot(plastic$PWaste)
# boxplot(plastic$MPWaste)

# Q1-1
library(ggplot2)
ggplot(plastic, aes(x = PWaste)) + 
  geom_histogram(binwidth = 20, fill = "green", color = "black") + 
  ggtitle("Histogram of PWaste") + 
  xlab("Amount of plastic waste (kg/day)") + 
  ylab("Frequency")

ggplot(plastic, aes(x = MPWaste)) + 
  geom_histogram(binwidth = 10, fill = "red", color = "black") + 
  ggtitle("Histogram of MPWaste") + 
  xlab("Amount of plastic waste (kg/day)") + 
  ylab("Frequency")

# Density of mismanaged plastic waste
ggplot(plastic, aes(x = PWaste)) + 
  geom_density(fill = "green", color = "black") + 
  ggtitle("Density of mismanaged plastic waste") + 
  xlab("Amount of mismanaged plastic waste (kg/day)") + 
  ylab("Density")

ggplot(plastic, aes(x = MPWaste)) + 
  geom_density(fill = "red", color = "black") + 
  ggtitle("Density of mismanaged plastic waste") + 
  xlab("Amount of mismanaged plastic waste (kg/day)") + 
  ylab("Density")
#--------------------------------------------------------------------------------
# Boxplot with individual data points

# library(tidyverse)
# library(hrbrthemes)
# library(viridis)

# ggplot(plastic, aes(x=PWaste, y = PWaste)) +
#   geom_boxplot() +
#   scale_fill_viridis(discrete = TRUE, alpha=0.6) +
#   geom_jitter(color="black", size=0.4, alpha=0.9) +
#   theme_ipsum() +
#   theme(
#     legend.position="none",
#     plot.title = element_text(size=11)
#   ) +
#   ggtitle("A boxplot with jitter") +
#   xlab("")

#--------------------------------------------------------------------------------

# install.packages("tidyverse")
library(tidyverse)
library(dplyr)
# 选择需要探索的变量
pwaste <- plastic %>% select(PWaste, MPWaste)
# 使用 pivot_longer() 把 PWaste 和 MPWaste 两列数据合并到一个列中，并把变量类型记录在 type 列中
pwaste_long <- pwaste %>% pivot_longer(cols = everything(), names_to = "type", values_to = "value")

# 绘制直方图
ggplot(pwaste_long, aes(x = value, fill = type)) + 
  geom_histogram(binwidth = 5, alpha = 0.7) +
  labs(title = "Distribution of Plastic Waste", x = "Plastic Waste (kg/day)", y = "Frequency") +
  scale_fill_discrete(labels = c("MPWaste", "PWaste"))

###-------------------------------------------------------------------final choice----
ggplot(plastic, aes(x=x) ) +
  geom_histogram( aes(x = PWaste, y = after_stat(count)), fill="#69b3a2" ) +
  geom_label( aes(x=100, y=60, label="PWaste"), color="#69b3a2") +
  geom_histogram( aes(x = MPWaste, y = -after_stat(count)), fill= "#404080") +
  geom_label( aes(x=100, y=-60, label="MPWaste"), color="#404080") +
  theme_ipsum() +
  ggtitle("Distribution of Waste") +
  theme(plot.title = element_text(hjust = 0.5, size = 15), axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15)) +
  xlab("Waste (kg/day)")

###-----------------------------------------------------------------------
ggplot(plastic, aes(x=x) ) +
  geom_histogram( aes(x = PWaste, y = after_stat(density)), fill="#69b3a2" ) +
  geom_label( aes(x=100, y=0.02, label="PWaste"), color="#69b3a2") +
  geom_histogram( aes(x = MPWaste, y = -after_stat(density)), fill= "#404080") +
  geom_label( aes(x=100, y=-0.05, label="MPWaste"), color="#404080") +
  theme_ipsum() +
  ggtitle("Waste Density") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  xlab("Waste (kg/day)")

################################################
# Q1
ggplot(plastic, aes(x = PWaste)) +
  geom_histogram(bins = 20, fill = "#0072B2", alpha = 0.5,na.rm = FALSE) +
  labs(title = "Distribution of PWaste",
       x = "PWaste (kg/day)",
       y = "Count") 

ggplot(plastic, aes(y = PWaste)) +
  geom_boxplot(fill = "#0072B2", alpha = 0.5) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Distribution of PWaste",
       x = "",
       y = "PWaste (kg/day)")

ggplot(plastic, aes(x = MPWaste)) +
  geom_histogram(bins = 20, fill = "#0072B2", alpha = 0.5) +
  labs(title = "Distribution of MPWaste",
       x = "MPWaste (kg/day)",
       y = "Count")

ggplot(plastic, aes(y = MPWaste)) +
  geom_boxplot(fill = "#0072B2", alpha = 0.5) +
  labs(title = "Distribution of MPWaste",
       x = "",
       y = "MPWaste (kg/day)")

library(visdat)
# 创建交互式缺失值可视化图 #56B4E9 -- final choice
vis_miss(plastic)

#Q1 - Violin
plastic_v = plastic[, 9:10]

plastic_v %>% 
  gather(key="MesureType", value="Val") %>%
  ggplot( aes(x=MesureType, y=Val, fill=MesureType)) +
  geom_violin()

#Q1 - Boxplot with individual data points -- final choice

library(tidyverse)
library(hrbrthemes)
library(viridis)

plastic_v %>%
  gather(key="WasteType", value="WasteValue") %>%
  ggplot( aes(x=WasteType, y=WasteValue, fill=WasteType)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Waste boxplot and distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("")

############################################################################################
# Q2
# 按地区制作箱线图
ggplot(plastic, aes(x=Region, y=PWaste)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Plastic Waste by Region", y = "Plastic Waste (kg/day)")

ggplot(plastic, aes(x=Region, y=MPWaste)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Mismanaged Plastic Waste by Region", y = "Mismanaged Plastic Waste (kg/day)")

# 按收入水平制作箱线图
ggplot(plastic, aes(x=IncomeStatus, y=PWaste)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Plastic Waste by Income Status", y = "Plastic Waste (kg/day)")

ggplot(plastic, aes(x=IncomeStatus, y=MPWaste)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Mismanaged Plastic Waste by Income Status", y = "Mismanaged Plastic Waste (kg/day)")

############################################################################################
# Q2-1

# Create the scatter plot
ggplot(plastic, aes(x = PWaste, y = MPWaste, color = Region)) +
  geom_point(size = 3) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "PWaste (kg/day)", y = "MPWaste (kg/day)") +
  ggtitle("Association between PWaste and MPWaste by Region") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(plastic, aes(x = PWaste, y = MPWaste, color = IncomeStatus)) +
  geom_point(size = 3) + 
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "PWaste (kg/day)", y = "MPWaste (kg/day)") +
  ggtitle("Association between PWaste and MPWaste by IncomeStatus") +
  theme(plot.title = element_text(hjust = 0.5))

library(ggridges)
ggplot(plastic, aes(PWaste, y=factor(Region), fill=Region)) + 
  geom_density_ridges(scale = 1.5)

ggplot(plastic, aes(MPWaste, y=factor(Region), fill=Region)) + 
  geom_density_ridges(scale = 1.5)

ggplot(plastic, aes(PWaste, y=factor(IncomeStatus), fill=IncomeStatus)) + 
  geom_density_ridges(scale = 1.5)

ggplot(plastic, aes(MPWaste, y=factor(IncomeStatus), fill=IncomeStatus)) + 
  geom_density_ridges(scale = 1.5)

############################################################################################
# Q3

ggplot(plastic, aes(x = PWaste, y = MPWaste, color = Region)) +
  geom_point() +
  labs(x = "Plastic Waste (kg/day)",
       y = "Mismanaged Plastic Waste (kg/day)") +
  scale_color_discrete(name = "Region")

ggplot(plastic, aes(x = PWaste, y = MPWaste, color = IncomeStatus)) +
  geom_point() +
  labs(x = "Plastic Waste (kg/day)",
       y = "Mismanaged Plastic Waste (kg/day)") +
  scale_color_discrete(name = "Income Status")

############################################################################################
# Q4

library(corrplot)
new_data <- na.omit(plastic)
corrplot.mixed(cor(new_data[,c(4,6:10)]), lower = 'number')

library(psych)
pairs.panels(cor(new_data[,c(4,6:10)]))

# 将 Region 变量转换成数值变量
plastic$Region <- as.integer(plastic$Region)
# 将 IncomeStatus 变量转换成数值变量
plastic$IncomeStatus <- as.integer(plastic$IncomeStatus)
pairs(plastic[,3:10])




############################################################################################
# Q5
# 导入所需的包
library(ggplot2)
library(scales)

# 绘制人均GDP与人均塑料垃圾的散点图
ggplot(data = plastic, aes(x = GDP, y = PWaste)) +
  geom_point(alpha = 0.5, color = "blue") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(x = "GDP", y = "PWaste(kg/day)") +
  theme_minimal()
# 添加趋势线
ggplot(data = plastic, aes(x = GDP, y = PWaste)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(x = "GDP", y = "PWaste(kg/day)") +
  theme_minimal()

# 绘制海岸人口百分比与人均塑料垃圾的散点图
ggplot(data = plastic, aes(x = CoastalPopPC, y = PWaste)) +
  geom_point(alpha = 0.5, color = "green") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(x = "CoastalPopPC", y = "PWaste(kg/day)") +
  theme_minimal()
# 添加趋势线
ggplot(data = plastic, aes(x = CoastalPopPC, y = PWaste)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(x = "CoastalPopPC", y = "PWaste(kg/day)") +
  theme_minimal()



# 绘制人均GDP与MPWaste人均塑料垃圾的散点图
ggplot(data = plastic, aes(x = GDP, y = MPWaste)) +
  geom_point(alpha = 0.5, color = "blue") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(x = "GDP", y = "MPWaste(kg/day)") +
  theme_minimal()
# 添加趋势线
ggplot(data = plastic, aes(x = GDP, y = MPWaste)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(x = "GDP", y = "MPWaste(kg/day)") +
  theme_minimal()

# 绘制海岸人口百分比与人均塑料垃圾的散点图
ggplot(data = plastic, aes(x = CoastalPopPC, y = MPWaste)) +
  geom_point(alpha = 0.5, color = "green") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(x = "CoastalPopPC", y = "MPWaste(kg/day)") +
  theme_minimal()
# 添加趋势线
ggplot(data = plastic, aes(x = CoastalPopPC, y = MPWaste)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(x = "CoastalPopPC", y = "MPWaste(kg/day)") +
  theme_minimal()







plastic_sub <- subset(plastic, select = c(IncomeStatus, PWaste, MPWaste))
plastic_sub <- na.omit(plastic_sub)

ggplot(plastic_sub, aes(x = PWaste, y = MPWaste)) +
  geom_point() +
  facet_grid(.~IncomeStatus) +
  labs(x = "PW (kg/day)", y = "MPW (kg/day)") +
  ggtitle("Relationship")


# 创建一个包含 IncomeStatus、PWaste 和 MPWaste 变量的数据框，并删除缺失值
plastic_sub <- subset(plastic, select = c(IncomeStatus, PWaste, MPWaste))
plastic_sub <- na.omit(plastic_sub)

# 使用 facet_grid() 函数将四个散点图放在一个2*2的图片中。x轴是人均塑料垃圾(PWaste)，y轴是管理不善的塑料垃圾(MPWaste)
ggplot(plastic_sub, aes(x = PWaste, y = MPWaste)) +
  geom_point() +
  facet_grid(.~IncomeStatus) +
  labs(x = "PW (kg/day)", y = "MPW (kg/day)") +
  ggtitle("Relationship between IncomeStatus and Waste") 

# 创建一个包含 IncomeStatus、PWaste 和 MPWaste 变量的数据框，并删除缺失值
plastic_sub <- subset(plastic, select = c(Region, PWaste, MPWaste))
plastic_sub <- na.omit(plastic_sub)

# 使用 facet_grid() 函数将四个散点图放在一个2*2的图片中。x轴是人均塑料垃圾(PWaste)，y轴是管理不善的塑料垃圾(MPWaste)
ggplot(plastic_sub, aes(x = PWaste, y = MPWaste)) +
  geom_point() +
  facet_grid(.~Region) +
  labs(x = "PW (kg/day)", y = "MPW (kg/day)") +
  ggtitle("Relationship between Region and Waste") 
