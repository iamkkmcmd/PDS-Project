# Cleaning R-studio
rm(list = ls())
graphics.off()

# Create main data file ------------------------------------------------------------------

# Set the working 
setwd("C:/Users/iamkkmcmd/Desktop/PDS Project/data/")

# Give short name to the column name
col_names <- c('ts','age','sex','work','phy_ff','phy_health','phy_bw', 'phy_ex', 'meal',
              'height','weight','exercise','fruit','veg','cook','spend','income','gymtime',
              'disease','review','rate')

# Read and Merge two data file
data1 <- read.csv("DEF survey (Responses) - Form Responses 1.csv", 
                  col.names = col_names)
data2 <- read.csv("DEF2 (Responses) - Form Responses 1.csv",
                  col.names = col_names)
def <- rbind(data1[,], data2[,])
rm(data1, data2)

# See at a glance 
def %>% glimpse()

# Change variable type
def$ts <- as.POSIXct(strptime(def$ts, format = "%m/%d/%Y %H:%M:%S"))
def$age <- as.integer(def$age)
def$meal <- factor(def$meal, labels = c("1","3","4","5","5+","2"))

# exercise --> multilabel to list of labels
# disease --> check the spelling for 'NO', 'no', 'No'....
# review --> change to text data, where text mining is possible

write.csv(def, file = "def_data.csv")


# Scrutiny of data -----------------------------------------------------------------------

def <- read.csv("def_data.csv")
def <- def[,-1]

# Summary of the data
summary(def)

# Comments from summary of data
# Minimum age is 0.00, which is wrong data
# There are one transgender respondent
# Maximum height is 7777
# Maximum weight is 9.998001e+14
# Maximum spending is 687688678
# Minimum income is 0

# Let's look at data contain age is 0.00
def[def$age == 0,]

# Remove the absurd response from the data
def <- def[-which(def$age == 0),]

# Let's look at summary of the data
summary(def)

# Again we have maximum height is 182. Which is not in feet scale. So, need to change in 
# particular scale. 1 feet = 30.48 cm
def$height <- with(def, ifelse(height < 10, height*30.48, height))
# Maximum value is 500kg, which is again outlier
def <- def[-which(def$weight == 500),]
def <- def[-which(def$weight < 30),]

# Clean data
write.csv(def, 'clean_data.csv')

# Data Descriptions ----------------------------------------------------------------------

# Variable Name				Description				                    Data Type
# ________________________________________________________________________________________
# ts 	    - Time Stamp							                    DateTime
# age 		- Age of the respondent					                    Continuous	
# sex 		- Gender of the respondent					                Factor
# work		- Work preference of the respondent			                Factor
# 
# Ratings on Physical Condition: (in 10 scale)
# phy_ff 	- Rating of liking fast food 			                	Factor
# phy_health- Rating of healthiness					                    Factor
# phy_bw 	- Rating of one's preference to maintain body weight		Factor	
# phy_ex 	- Rating of importance of exercise			            	Factor
# 
# meal 		- Number of meals in a day				                	Discrete
# height	- Height of the respondent		    		            	Continuous
# weight 	- Weight of the respondent		    	                	Continuous
# exercise 	- Type of exercises 						                Factor
# 
# Diet Culture: (Number of times)
# fruit 	- Number of meals contain fruits				            Factor
# veg		- Number of meals contain vegetables			        	Factor
# cook		- Number of cooked plates				                	Factor
# 
# spend 	- Expenditure on fitness    				            	Continuous
# income 	- Monthly family income					                    Continuous
# gymtime 	- Time spent in gym. (In a day)				                Continuous
# disease 	- Whether suffers from any regular disease		        	Categorical
# 
# review	- Review of the survey			    		            	Text
# rate		- Rate the project topics and question		            	Factor


