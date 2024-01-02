#作业2-1.1

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
# 将 DataFrame 内容输出为 CSV 文件
write.csv(Aminer_data, file = "combined_file.csv", row.names = FALSE)
#删除重复行
Aminer_data_unique <- Aminer_data[!duplicated(Aminer_data$标题), ]



#作业2-1.2

# 使用lapply读取所有 CSV 文件并存储为数据框列表
data_list <- lapply(Aminer_files,read.csv, header = TRUE)
# 将数据框列表中的所有数据框合并为一个大的数据框
Aminer_data_apply <- do.call(rbind, data_list)
# 输出合并后的数据框
print(data_list)
print(Aminer_data_apply)

#作业2-1.3
args <- commandArgs(T)
m <- as.numeric(args[1])
Idaaccuracy_Aminer<- "C:/Users/W10/Dropbox/bigdata_econ_2023/data/assignment_idaccuracy/Aminer"
Aminer_files <- list.files(Idaaccuracy_Aminer, pattern = "\\.csv$", full.names = TRUE)
combine_list <- lapply(Aminer_files,read.csv, header = TRUE)
combined_df <- do.call(rbind, data_list)
#Rscript aminer_papers.R C:/Users/W10/Dropbox/bigdata_econ_2023/data/assignment_idaccuracy/Aminer


#作业2-2.1

#读取scientist_pub.csv 文件
scientist_pub <- read.csv("C:/Users/W10/Dropbox/bigdata_econ_2023/data/assignment_idaccuracy/scientist_pub.csv")  
#删除重复行
scientist_pub_unique <- scientist_pub[!duplicated(scientist_pub$title), ]
ID_list <- vector()
#提取出aminer文件名中的作者ID，并生成一个list
for (file in Aminer_files){
  ID <- str_extract(file,"0_[0-9+]*")
  ID_list <- c(ID_list,ID)
}
### 从aminer中筛选出也在scientist_pub中的数据
# 筛选 scientist_pub.csv 中 doi 在列表中的数据
filtered_data <- Aminer_data_unique[Aminer_data_unique$unique_ID %in% scientist_pub_unique$uniqueID & 
                                      str_to_lower(Aminer_data_unique$标题) %in% scientist_pub_unique$title, ]
write.csv(filtered_data, file = "filtered_data.csv", row.names = FALSE)

#作业2-2.2

a <- table(Aminer_data_unique$unique_ID)
b <- table(filtered_data$unique_ID)
a_df <- as.data.frame(a)
b_df <- as.data.frame(b)
names(a_df) <- c("ID","Freq_aminer")
names(b_df) <- c("ID","Freq_filter")
# 使用 merge() 合并两个数据框
precision_df <- merge(a_df, b_df, by = "ID", all = TRUE) # all = TRUE 表示保留所有行
# 计算 precision
precision_df$precision <- precision_df$Freq_filter / precision_df$Freq_aminer
precision_df$precision[is.na(precision_df$precision)] <- 0
c <- table(scientist_pub_unique$uniqueID)
c_df <- as.data.frame(c)
names(c_df) <- c("ID","sciFreq")
# 使用 merge() 合并两个数据框
recall_df <- merge(c_df, b_df, by = "ID", all = TRUE) # all = TRUE 表示保留所有行
# 计算 recall
recall_df$recall <- recall_df$Freq_filter / recall_df$sciFreq
recall_df$recall[is.na(recall_df$recall)] <- 0
recall_df

#作业2-2.3

###计算aminer数据库的平均精准度和查全率
ave_pre <- mean(precision_df$precision)
ave_recall <- mean(recall_df$recall)
