###作业4

# 设置文件夹路径
Idaaccuracy_Aminer<- "C:/Users/W10/Dropbox/bigdata_econ_2023/data/assignment_idaccuracy/Aminer"
# 获取指定路径下所有的 CSV 文件名
Aminer_files <- list.files(Idaaccuracy_Aminer, pattern = "\\.csv$", full.names = TRUE)

# 初始化一个空的数据框
Aminer_data <- data.frame()
# 使用循环读取所有 CSV 文件并合并数据
for (file in Aminer_files) {
  # 逐个读取 CSV 文件
  data <- read.csv(file, header = TRUE) 
  # 获取作者ID
  ID2 <- str_extract(file,"0_[0-9+]*")
  # 将作者ID写入到data.frame中
  data$unique_ID <- ID2
  Aminer_data <- rbind(Aminer_data, data)
}
# 将列转换为小写
Aminer_data$标题 <- tolower(Aminer_data$标题)
#删除重复行
Aminer_data_unique <- Aminer_data[!duplicated(Aminer_data$标题), ]

#读取scientist_pub.csv 文件
scientist_pub <- read.csv("C:/Users/W10/Dropbox/bigdata_econ_2023/data/assignment_idaccuracy/scientist_pub.csv")  
#删除重复行
scientist_pub_unique <- scientist_pub[!duplicated(scientist_pub$title), ]
# 选择多列
scientist_pub2 <- scientist_pub_unique[, c("uniqueID", "title","pub_year")]
Aminer_data2 <- Aminer_data_unique[, c("unique_ID", "标题","年份")]
names(Aminer_data2) <- c("uniqueID","title","pub_year")
#使用intersect获取两个dataframe的交集
unique_data <- intersect(Aminer_data2,scientist_pub2)
a <- table(Aminer_data2$uniqueID)
b <- table(scientist_pub_unique$uniqueID)
c <- table(unique_data$uniqueID)

a_df <- as.data.frame(a)
b_df <- as.data.frame(b)
c_df <- as.data.frame(c)
names(a_df) <- c("ID","AminerFreq")
names(b_df) <- c("ID","sciFreq")
names(c_df) <- c("ID","filterFreq")
# 使用 merge() 合并两个数据框
precision_df <- merge(a_df, b_df, by = "ID", all = TRUE) 
# 合并三个dataframe
df <- merge(precision_df,c_df,by = 'ID',all = TRUE)
df <- mutate(df, precision = filterFreq/AminerFreq)
df <- mutate(df, recall = filterFreq/sciFreq)
df$precision[is.na(df$precision)] <- 0
df$recall[is.na(df$recall)] <- 0
###计算aminer数据库的平均精准度和查全率
ave_pre <- mean(df$precision)
ave_recall <- mean(df$recall)