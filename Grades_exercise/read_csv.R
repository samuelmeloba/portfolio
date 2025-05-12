#library(readcsv)
student_por <- read.csv("data/student-por.csv", header = TRUE, sep = ";")
View(student_por)

student_mat <- read.csv("data/student-mat.csv", header = TRUE, sep = ";")
View(student_mat)

d3 = merge(student_mat, student_por,by = c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet"))
print(nrow(d3))