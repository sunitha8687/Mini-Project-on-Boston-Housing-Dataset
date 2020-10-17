# some simple data analysis commands on the IRIS dataset

data(iris)              # load data frame
iris                    # print data frame
names(iris)             # which columns has data frame?
summary(iris)           # show a textual summary of the data frame

# Box plots for the numerical variables 
par(mfcol=c(2,2))                          # 2*2=4 plot areas
for (i in 1:4) boxplot(iris[,i],main=names(iris)[i])
# TEST IT: what happens with the 3rd box plot, if we set iris[1,3]=10 or =100?

# scatter plot for each  pair of input variables, colored with 'Species'
win.graph()
pairs(iris[,1:4], cex=1.0, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)], 
      main="Iris Data: Predictors")
