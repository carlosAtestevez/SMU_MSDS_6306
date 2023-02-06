
#Basic Matches
x1 = c("apple","bananas","pear")


#Collapse Strings... put them together
str_c(x1, collapse = ",")

#Counting
str_count(x1,"apple")
sum(str_count(x1,"apple"))


#If you want to find elements with AN
str_view(x1,"an")

#str_subset
str_subset(x1,"an")

#str_extract
str_extract(x1,"an")

#str_match
str_match(x1,"an")


#Any character except a new line
str_view(x1,".a.")
str_subset(x1,".a.")

#Scape dot \\.
x2 = c("abc","a.c","bef")
str_view(x2,"a\\.c")
str_subset(x2,"a\\.c")

#Scape backslash \\\\.
str_subset(x2,"a\\\\c")

#Anchors
#^ The start of the string
#$ The end of the string
x3 = c("apple","banana","pear")
str_subset(x3,"^a")
str_view(x3,"^a")
str_subset(x3,"a$")
str_view(x3,"a$")
str_extract(x3,"a$")

