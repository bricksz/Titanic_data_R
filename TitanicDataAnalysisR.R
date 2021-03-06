# Session > Set working dir

# Load raw data
train <- read.csv('train.csv'. header = TRUE)
test <- read.csv('test.csv', header = TRUE)

# Add a 'Survived' variable to the test set to allow for combing data sets
test.survived <- data.frame(Survived = rep('None', nrow(test)), test[,])

# data.frame: add variable called survived
# rep('None', nrow(test)) : replicate values, repeat the values of 'None',
#                         and the number of times to repeat is the value of nrow(test), 418 times.
# test[,]: indexing all values (rows, columns)
# test.survived: combines the 418 rows of 'None' with the index of the entire df.

# Combine data sets
data.combined <- rbind(train, test.survived)

# rbind: row bind, cbind:column bind
# data.combined: appended the datasets row by row.

# data types in R
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived) # turn Survived char into factor
data.combined$Pclass <- as.factor(data.combined$Pclass) # turn Pclass int into factor

# Pclass should not be an int value, should be a factor

# factor: denotes categorical or numeric 
#       encoding numeric or character data into a discrete datatype.

# Take a look at gross survival rates
table(data.combined$Survived)

# Distrubtion across classes
table(data.combined$Pclass)

# load up ggplot2 package to use for visualization
libaray(ggplot2)

# Hypothesis - Rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)   # because in train set, Pclass is still int
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab('Pclass') +
  ylab('Total Count') +
  labs(fill = 'Survived')

# aes: aesthetic, X-axis: Pclass, fill(optional): factor(Survived) in the bar (Survived was int, now factor)
# xlab, ylab: x,y labels
# fill = factor(Survived): generated the green and red bar graph depedent on 0 and 1.

# Examine the first few names in the training data set
head(as.character(train$Name))

# names in train dataset was previously a factor, now viewed in string.


# How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))
# 1307 unique names

# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), 'Name'])

# duplicated: determine duplicate elements, 1309 character string, and return which one are the dupe rows.
# which: like sql WHERE, so it is grabbing from column 'Name'.

# Next, take a lok at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]

# if they happen to be %in% the set of names in dup.names, then pull that record out using which, across all index.
# we find out that there is no dupes.

# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

# Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, 'Miss.')), ]

# grab every single name in data.combined$Name, and detect to see if 'Miss.' is in that string.
# if so, (which), grab every record in data.combined WHERE 'Miss.' is in the name, and store it in misses variable

misses[1:5,]
# [1:5, ]: give me the first 5 values and all of columns.

# is there anything interesting going on? 4/5 survived. Most of the folks were also 3rd class.
# look at the age. SibSp: Num of sibling or spouses onboard.
# parch: num of parents or children on board.
# the 4 year old girl is traveling with one parent and 1 sibling on board.
# this is signifigant, because the others has all zero in Parch. 
# Miss denotes a non married women, generally speaking. 

# Hypothesis - Name title correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, 'Mrs.')), ]
mrses[1:5, ]
# husband names first, maiden name after. Mrs indicate older female. All 5 survived.

# Check out males to see if pattern continues
males <- data.combined[which(train$Sex == 'male'), ]
males[1:5, ]
# most males died, 2 year old with title Master w/ 3 sibling and 1 parent.

# expand upon the relationship between 'Survived' and 'Pclass' by adding the new 'Title' variable
# to the data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep('Miss.', Name)) > 0) {
    return ('Miss.')
  } else if (length(grep('Master.', Name)) > 0) {
    return ('Master.')
  } else if (length(grep('Mrs.', Name)) > 0) {
    return ('Mrs.')
  } else if (length(grep('Mr.', Name)) > 0) {
    return ('Mr.')
  } else {
    return ('Other')
  }
}

Titles <- NULL
for (i in 1:nrow(data.combined)) {
  Titles <- c(Titles, extractTitle(data.combined[i, 'Name']))
}
data.combined$Title <- as.factor(Titles)

# since we have only survived labels for the train set, only use the first 891 rows.

ggplot(data.combined[1:891, ], aes(x= Title, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle('Pclass') +
  xlab('Title') +
  ylab('Total Count') +
  labs(fill = 'Survived')

# distribution of F to M 
table(data.combined$Sex)

# Viz the 3-way relationship of sex, Pclass, and survival, compare to analysis
ggplot(data.combined[1:891, ], aes(x = Sex, fill = Survived)) + 
  geom_bar(width = .5) +
  facet_wrap(~Pclass) +
  ggtitle('Pclass') +
  xlab('Sex') +
  ylab('Total Count') +
  labs(fill = 'Survived')

# age distribution
summary(data.combined$Age)
# some of the training set is included in the model, so lets only include the test set.
summary(data.combined[1:891, 'Age'])

# Survival rates broken down by sex, pclass, and age
ggplot(data.combined[1:891, ], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_bar(width = 1) +
  xlab('Age') +
  ylab('Total Count')

# Validate that "Master." is a good proxy for male children.
boys <- data.combined[which(data.combined$Title == 'Master.') ,]
summary(boys$Age)

# We know that "Miss." is more complicated, examine further
misses <- data.combined[which(data.combined$Title == "Miss."), ]
summary(misses$Age)

ggplot(misses[misses$Survived != 'None', ], aes(x=Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_bar(width = 1) +
  ggtitle("Age for 'Miss.' by Pclass") +
  xlab('Age') +
  ylab('Total Count')

# female children have different survival rate
# examine further
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0), ]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

# Move onto the sibsp var, summarize the var
summary(data.combined$SibSp)

# Can we treat as a factor?
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# We believe title is predictive. Viz survival rates by sipbs, pclass and title.
ggplot(data.combined[1:891, ], aes(x = SibSp, fill = Survived)) +
  geom_bar(width = 1) + 
  facet_wrap(~Pclass + Title) +
  ggtitle('Pclass, Title') +
  xlab('SibSp') +
  ylab('Total Count') +
  ylim(0,300) +
  labs(fill = 'Survived')