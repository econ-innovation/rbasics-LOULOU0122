v1 <- c(1,2,3,4)
as.character(3.141592657)
as.logical(3.141592657)
is.character("hello world")
is.na(NA)
v1[c(1,2)]
v1 <- c(4,3,2,1)
v1[c(2,3)]
v1[length(v1)]
v1[length(v1)-1]
v1[v1 %% 3 == 0] 
v1[-1] #v1[-1]返回v1中除了第一个元素之外的其他元素。与python不同，python中负数索引通常用于从倒数第一个元素开始计数，取出该索引处的值，而r中负数索引表示删除该索引处的值，取出其余值。
v1[5]#v1[5]返回NA。R中的向量索引超出范围，通常不会报错。
v1[] <- 99
v1 <- 99
#v1[] <- 99会替换v1中的所有元素为99
#v1 <- 99会将v1替换为99这个数字