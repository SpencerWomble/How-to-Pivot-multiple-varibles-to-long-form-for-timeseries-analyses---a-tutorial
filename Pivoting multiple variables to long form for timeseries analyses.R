############################################################################
############################################################################
############################################################################

# Pivoting multiple variables to long-form for timeseries analyses

############################################################################
############################################################################

# This tutorial provides the code and explanations for pivoting multiple variables
# of a wide-form timeseries data set into a long-form where each variable has its own column
# and values within each row correspond to a specific time point.

############################################################################


library(tidyverse)

# creating fake timeseries dataframe in wide-form
data<- data.frame(ID = c(1,2,3,4,5,6,7,8,9,10),
                  VAR1_timepoint_1 = rnorm(1:100, n = 10),
                  VAR1_timepoint_2 = rnorm(1:100, n = 10),
                  VAR1_timepoint_3 = rnorm(1:100, n = 10),
                  VAR2_timepoint_1 = rnorm(1:100, n = 10),
                  VAR2_timepoint_2 = rnorm(1:100, n = 10),
                  VAR2_timepoint_3 = rnorm(1:100, n = 10),
                  group = c("a","a","b","b","c","c","d","d","e","e"))

# rounding data to 2 decimal places
data<- data %>% mutate(across(VAR1_timepoint_1:VAR2_timepoint_3, round, 2))
print(data)


#############################################################################
#############################################################################

# short version

# Here is all the code you need to pivot multiple columns of wide-form timeseries data
# at once. For detailed explanations of how the code works, see the script below. I
# recommend you check out the explanations below so you can more readily adapt this
# code to suit your data. There's also two examples of how you can plot your
# newly-pivoted data

final_piovted_data<- data %>%
  pivot_longer(!c(ID,group),
               names_to = c("variable", "timepoint"), 
               names_sep = "_") %>%
  dplyr::select(!timepoint) %>% 
  mutate(timepoint = rep(1:3, times = 20)) %>%
pivot_wider(names_from = "variable", values_from = "value")

print(final_piovted_data)



#################################################
#################################################
#################################################

# Explanations of the code

#################################################

# pivoting to long form

# to pivot these variables to long form all together, there are 
# some coding work-arounds we'll need to use. First, we will
# pivot the data to long-form. This will stack all the variables together. 
# It looks messy, but we will clean it up

long_form_data<- data %>%
  pivot_longer(!c(ID,group), # ! is telling R not to include ID in the pivot (we only want to pivot the variable values)
               names_to = c("variable", "timepoint"), # we're naming these columns based on where R will separate them
               names_sep = "_") # this line tells R to separate the names into new columns when it encounters a "_" character
print(long_form_data)

# NOTE: You may want to rename your variables (at least temporarily)
#       to match the format presented in this example. Chiefly, so 
#       you can seperate on the "_" character.

###################################################


# Now, we see that all are variables are stacked into one column
# called "variable". Though not labeled here, each row corresponds to a
# time point for the variable in question. We will label these with
# numbers representing the time points next.

# This is good, but we still need to compress these data into a 
# long-form/wide-form hybrid that has each variable in its own column
# and create a new column that will be the time point. To do this,
# we are actually going to use "pivot_wide" again.



# Creating a new column for time point using matching character strings
# This column will have the numbers "1,2,3" repeating so that each variable
# in the "variable" column has a time point associated with its value

long_form_data2<- long_form_data %>%
  dplyr::select(!timepoint) %>% # remove timepoint column as we'll be replacing it with numbers (1,2,3)
  mutate(timepoint = rep(1:3, times = 20))

print(long_form_data2)

###################################################

# Now we will pivot wider again so that we end with one column for VAR1,
# one column for VAR2, and a time point column

long_form_data3<- long_form_data2 %>%
  pivot_wider(names_from = "variable", values_from = "value")

print(long_form_data3)

# now we have pivoted multiple variables into a long-form data set where the values
# for each variable correspond to a specific time point.

# pivoting like this is really handy for making timeseries plots if you 
# have timeseries data in a wide format (see examples below).



##########################################################
##########################################################

# example of a timeseries plot for VAR1 for our newly-pivoted long-form data

# making timepoint a factor rather than numeric
long_form_data3 %>%
  mutate(timepoint = as.factor(timepoint)) %>%
ggplot(aes(x = timepoint, y = VAR1, color = group)) +
  geom_point()+
    theme_classic()

########################################################

# you can also plot means on your newly-pivoted data to see changes in rates

long_form_data3 %>%
  group_by(timepoint, group) %>% 
  summarise(mean_VAR1 = mean(VAR1, na.rm = TRUE)) %>%
ggplot(aes(x = timepoint, y = mean_VAR1, color = group)) +
  geom_line(aes(linetype = group, color = group)) + 
  geom_point(data = long_form_data3, aes(x = timepoint, y = VAR1)) + # this adds the raw datapoints to the graph
  theme_classic()+
  scale_x_continuous(limits = c(1, 3), breaks = c(1,2,3))


