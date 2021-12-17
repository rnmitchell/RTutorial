### This exercise is intended to show you some examples of advanced data visualization.
### 

library(lubridate)
library(ggplot2)
library(UpSetR)
library(magrittr)

# Setting up our colors code:
# This will be relevant later in the exercise
bright <- c(yellow=rgb(255,222,13, maxColorValue=255), #ffde0d
            orange=rgb(232,121,12, maxColorValue=255),   #e8790c
            red=rgb(222,45,38, maxColorValue=255), #de2d26
            green=rgb(12,189,24, maxColorValue=255),  #0cbd18
            purple=rgb(148,12,232, maxColorValue=255), #940ce8
            blue=rgb(58,87,149, maxColorValue=255), #3a9eea
            pink=rgb( 255, 102, 153, maxColorValue=255), #ff6699
            teal=rgb(59,196,199, maxColorValue=255), #3bc4c7
            brown=rgb(98,74,46, maxColorValue=255))  #624a2e


# First, we will obviously need to import the dataset.
my.file <- read.csv(file="viz_data_battle_groups.csv", header=TRUE)

for (group in c("british", "greeks", "dutch", "belgians", "polish", "russians", "french", "americans")) { 
  my.file[,group] = ifelse(grepl(group, tolower(my.file$Groups), fixed=TRUE), 1, 0)
  my.file[,paste0(group,"_dur")]=ifelse(grepl(group, tolower(my.file$Groups), fixed=TRUE), my.file$Duration, 0)
}

# let's take a look at our file so far, using the head command, which we learned earlier in the workshop
head(my.file)

my.file2 <- my.file %>% mutate(NewStart = mdy(as.character(Start)))
my.file2 <- my.file2 %>% mutate(NewEnd = mdy(as.character(End)))
my.file2$Year <- year(my.file2$NewStart)

#visualizing the intersections between groups
intersection.df <- data.frame(my.file[,5], my.file[,7], my.file[,9], my.file[,11], my.file[,13], my.file[,15], my.file[,17], my.file[,19], my.file2$Duration, my.file2$Year)
colnames(intersection.df) <- c("British", "Greeks", "Dutch", "Belgians", "Polish", "Russians", "French", "Americans", "Duration", "Year")

#visualizing the distribution of battle duration (in days) across years
p <- ggplot(intersection.df, aes(Duration)) 
p + geom_histogram(colour="black", fill="purple",binwidth = 10) + xlim(c(0,350)) + ylab("Number of battles") + xlab("Battle duration (days)")
ggsave(file="histogram_battle_duration_year.png")

# Let's break out battle duration by year using a boxplot
# A boxplot (or 'box-and-whiskers plot' is a way to visually represent a distribution within a sample. The box itself shows the 25-75th percentile distribution, and the solid line in the box indicates the median value)
p <- ggplot(intersection.df, aes(x=factor(Year), y=Duration, fill=factor(Year)))
p + geom_boxplot() + ylim(0,250) + xlab("Battle Year") + ylab("Battle duration (days)") + scale_fill_manual(values=as.character(bright[c("green", "brown", "teal", "pink", "blue", "purple", "orange", "red")]))
ggsave(file="boxplot_Battle_duration_year.png")

# We can see that there is a big range of battle duration, though it's hard from this plot to say if it's changing by year
# Let's consider other ways of visualizing battle length using this dataset

#visualizing the number of 'battle intensity' ie those with three or more countries
new.df <- intersection.df[,1:8]
intersection.df$nGroups <- rowSums(new.df == 1)
my.vec <- rep(0, nrow(intersection.df))
my.index <- which(intersection.df$nGroups>=3)
my.vec[my.index] <- 1
intersection.df$threeOrMoreGroups <- my.vec
q <- ggplot(intersection.df, aes(x=factor(Year), y=threeOrMoreGroups), fill=factor(Year))
q + geom_bar(stat="identity") + xlab("Battle year") + ylab("Battles involving three or more countries")
ggsave(file="barplot_battles_with_3_or_more_groups.png")

# This plot shows us that 'battle intensity', as we have defined it has increased over the years, peaking in 2020. But this isn't the only we can capture our workload. Let's look at another.

#representing the fraction of battle with three or more countries
r <- ggplot(intersection.df, aes(threeOrMoreGroups, group=Year)) + geom_bar(aes(y=..prop.., fill=factor(..x..)), stat="count") + scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + facet_grid(~Year)
r + scale_fill_discrete(name = "# of countries/\nbattle", labels = c("< 3", ">= 3")) + theme(axis.ticks.x = element_blank(),
                                                                             axis.text.x = element_blank())
ggsave(file="barplot_battles_with_3_or_more_fraction.png")

# Now, let's visualize the interactions between differenct groups. To do this, we will use a new type of plot, which is called the UpSet. 
# The UpSet is a quite recent development in data visualization. It was presented by Alexander Lex and colleagues in 2016: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4720993/
# An UpSet is a way to display overlaps between sets. In this way it's similar to the much more well-known Venn Diagram. 
# However, the UpSet is (at least in my view) demonstrably superior to the Venn Diagram when the overlaps between more than three sets is displayed.

# We run this code using our intersection data frame, as follows:
png(filename="upSetPlot.png")
upset(new.df, nsets=8) #using the data frame we made earlier
dev.off()
# Note: we had to use a slightly different approach to save this file programmatically (ie with code). This is because we used a different plotting package.

# Here we see the distribution of different interactions between countries

### Our challenge is to display our data in a succinct way that shows our increased workload.

## A violin plot of # of active battles/day for each year
## A violin plot is similar to a box-and-whiskers plot, but it shows the distribution within a group more precisely
daily.battles <- read.csv(file="dailybattles_df.csv", header=TRUE) 
# Note: we had to manipulate the data frame to obtain this daily.battles object, but we're not going to focus on that during this tutorial.
# Instead, we're just reading in the data from a csv file in the intrest of clarity.
ggplot(data=daily.battles, aes(fill=as.factor(Year))) +
  geom_violin(aes(x=plot, y=Battles), size=1, scale="width") +
  xlab("Year")+
  ylab("# of Active Battles/Day")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous("**1946 through Sept. 15", labels=c("1939", "1940", "1941", "1942", "1943", "1944", "1945", "**1946"), breaks=c(183, 548, 913,1278, 1643, 2008, 2373, 2690))+
  scale_fill_manual(values=as.character(bright[c("green", "brown", "teal", "pink", "blue", "purple", "orange", "red")])) +
  labs(fill="Year")+
  theme(axis.title.x=element_text(size=8))
ggsave("daily_battles_violin_plot.png")

### Let's see if we can represent the battle interactions in another way. 
### To do this, here we're creating what is called a stacked barplot. This shows the number or proportion of a given category within a single bar.
battles.tibble <- daily.battles %>% count(Battles, Year) %>% group_by(Year) %>% mutate(prop=prop.table(n))
battles.tibble$Battles <- reorder(battles.tibble$Battles, sort(battles.tibble$Battles, decreasing=TRUE))
battles.tibble$Battles <- ordered(battles.tibble$Battles, levels = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1", "0"))
ggplot(battles.tibble, aes(x=factor(Year), y=prop)) + geom_col(aes(fill=factor(Battles))) + labs(fill="Number of Battles") + xlab("Battle Year") + ylab("Proportion of all days")
ggsave(file="updated_daily_battles_stacked_barplot.png")

### Like the violin plot we made previously, this is telling us that the the battle interactions we were seeing, as measured by simultaneous battles, was increasing over time. 

### This concludes this part of the workshop. Please let us know if you have any questions or need help.
