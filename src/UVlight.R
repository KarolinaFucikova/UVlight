# read in data from a file in that directory 

UVdata <- read.csv(file = "data/BOL5-4data2.csv")
# there may be some extra columns and rows, so just get the ones needed
# the following gets rows 1-8 and columns 1-5
UVdata <- UVdata[c(1:8),c(1:5)]
#names(UVdata) <- as.character(c("treatment","UV24","UV48","UV96","UV144"))
names(UVdata) <- as.character(c("treatment","24","48","96","144"))

# if the table is in the "wide" format, we need to transform it first
# from wide shape to long shape; tidyr package is having a lot of trouble with the column headings
# tutorial here: datasciencemadesimple.com/reshape-in-r-from-wide-to-long-from-long-to-wide/

#UVdata<- reshape(data=UVdata, idvar="treatment",
#                         varying = c("UV24","UV48","UV96","UV144"),
#                         v.name=c("Survival"),
#                         times=c("UV24","UV48","UV96","UV144"),
#                         new.row.names = 1:1000,
#                         direction="long")

UVdata<- reshape(data=UVdata, idvar="treatment",
                 varying = c("24","48","96","144"),
                 v.name=c("Survival"),
                 times=c("24","48","96","144"),
                 new.row.names = 1:1000,
                 direction="long")

names(UVdata) <- c("treatment","Uvlevel", "Survival")

names(UVdata)
# this just shows you the names of your variables, so that you can correctly enter them into the analysis script below

# visualize your data in a simple box plot, the formula is as follows:
# boxplot(response_variable ~ independent_variable, data=data_object)
# for 2 factors, it is a little more complicated:
boxplot(Survival~Uvlevel*treatment, data=UVdata)
# if it looks about right, make it pretty by adding colors
# and swap the variables so that the paired light and dark are next to each other
# setting the colors
colors <- c("orange", "turquoise")
# plotting
# annoyingly, the uv levels are alphabetized now because of the table transformation
boxplot(Survival~treatment*Uvlevel, data=UVdata, col=colors, xlab = "UV-C level (J/m2)", ylab = "% survival")
legend("topright", inset=.02, title="Treatment",
       c("dark","light"), fill=colors, horiz=TRUE, cex=0.8)

# I can fix that but not sure it's worth the trouble since these aren't figures to be used anyway

# now the actual ANOVA - set it up similarly to setting up the box plot
UV.aov = aov(Survival~treatment*Uvlevel, data=UVdata)
summary(UV.aov)
# in this case it appears there are significant differences between UV levels
# but not between the light/dark treatments
# interaction also isn't significant

# post hoc test is in order
# problem is Tukey only works with categorical variables, so we have to redo:
fUvlevel<-factor(UVdata$Uvlevel)
Uv.aov2 <- aov(UVdata$Survival~UVdata$treatment*fUvlevel)
TukeyHSD(Uv.aov2, conf.level = 0.95)
# we can reorganize those same results to just focus on UV level
TukeyHSD(Uv.aov2, ordered = TRUE, which = "fUvlevel", conf.level = 0.95)
# looks like there are significant differences between the following UV levels:
# 24-144, 24-96, and 24-48

#######
# trying some more graphing
# problem now is the values of UV levels are treated as text, not numbers
# fix that
UVdata$Uvlevel <- as.numeric(UVdata$Uvlevel)

# load ggplot package
library(ggplot2)
# plot
fancyplot<-ggplot(UVdata, aes(x=Uvlevel, y=Survival, color=treatment, shape=treatment)) + 
  geom_point(size=5) + geom_smooth(method=lm, aes(fill=treatment))
fancyplot 
# confidence intervals definitely overlap here, but perhaps fitting a straight line isn't ideal
