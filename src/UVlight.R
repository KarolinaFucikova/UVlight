# read in data from a file in that directory 

UVdata <- read.csv(file = "data/BOL5-4data.csv")

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
boxplot(Survival~treatment*Uvlevel, data=UVdata, col=colors, xlab = "UV-C level (J/m2)", ylab = "% survival")
legend("topright", inset=.02, title="Treatment",
       c("dark","light"), fill=colors, horiz=TRUE, cex=0.8)

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
fancyplot<-ggplot(UVdata, aes(x=Uvlevel, y=Survival, color=treatment, shape=treatment)) + 
  geom_point(size=5) + geom_smooth(method=lm, aes(fill=treatment))
fancyplot 
# confidence intervals definitely overlap here, but perhaps fitting a straight line isn't ideal
