## ---- echo=FALSE, message=FALSE, warning=FALSE, comment = NA------------------------
################################################################
# download data and create new data frame
################################################################

# install tidyverse, caret, and data.table packages if needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

#call needed packages
library(tidyverse)
library(caret)
library(data.table)

# mushroom dataset link:
# https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data

# create object of data url - mushroom_url
mushroom_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"

# create temporary file for mushroom data
temp_filename <- tempfile()

# download csv mushroom data into temporary file
download.file(mushroom_url, temp_filename)

# create r df object containing all mushroom data
all_mushroom_dat <- as.data.frame(read.csv(temp_filename,
                                           header = FALSE,
                                           sep = ","))

# create vector of column names
column_names <- c("is_edible", "cap_shape", "cap_surface",
                  "cap_color","does_it_bruise", "odor",
                  "gill_attachment", "gill_spacing",
                  "gill_size", "gill_color", "stalk_shape",
                  "stalk_root",
                  "stalk_surface_above_ring",
                  "stalk_surface_below_ring",
                  "stalk_color_above_ring",
                  "stalk_color_below_ring",
                  "veil_type", "veil_color", "ring_number",
                  "ring_type","spore_print_color", "population",
                  "habitat")

# set column names
colnames(all_mushroom_dat) <- column_names

# make variables' classes equal to factor
make_factors <- c(1:23)
all_mushroom_dat[,make_factors] <- lapply(all_mushroom_dat[,make_factors], factor)

# remove unneeded objects
rm(mushroom_url, temp_filename, make_factors)

# look at structure of data frame
str(all_mushroom_dat)


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
################################################################
# create new data frame of mushroom data without veil_type and stalk_root columns
################################################################

mushroom_dat <- all_mushroom_dat %>%
  select(-veil_type, -stalk_root)


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
################################################################
# create mushroom_training_set and mushroom_test_set
################################################################

# make mushroom_test_set 20% of mushroom_dat
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = mushroom_dat$is_edible,
                                  times = 1, p = 0.2,
                                  list = FALSE)
mushroom_training_set <- mushroom_dat[-test_index,]
mushroom_test_set <- mushroom_dat[test_index,]


## ---- include=FALSE-----------------------------------------------------------------
#################################################################
# code for proportions of dependent variable in training and test sets
#################################################################

options(digits = 3)
# proportion of edible (e) vs non-edible (p) in training set
sum(mushroom_training_set$is_edible == "e") # [1] 3366
sum(mushroom_training_set$is_edible == "e")/
  nrow(mushroom_training_set) # [1] 0.518
sum(mushroom_training_set$is_edible == "p") # [1] 3132
sum(mushroom_training_set$is_edible == "p")/
  nrow(mushroom_training_set) # [1] 0.482

# proportion of edible (e) vs non-edible (p) in test set
sum(mushroom_test_set$is_edible == "e") # [1] 842
sum(mushroom_test_set$is_edible == "e")/
  nrow(mushroom_test_set) # [1] 0.518
sum(mushroom_test_set$is_edible == "p") # [1] 784
sum(mushroom_test_set$is_edible == "p")/
  nrow(mushroom_test_set) # [1] 0.482


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
#################################################################
# Figure 1 plot observations of dependent variable levels e and p for each independent variables by levels
#################################################################

# install cowplot and ggpubr packages if needed
if(!require(tidyverse)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

# call needed packages
library(cowplot)
library(ggpubr)

#################################################################
# create plot of individual independent variables
#################################################################
# plots will be in grid of 4 rows and 3 columns to a page  
# plot in first row and first column of page 1 need legend and y axis label
# plots in first column of pages 1 and 2 of figure need y axis label but no legend
# plots in second and third columns of pages 1 and 2 do not need legend or y axis label

################################################################
# create plot for cap_shape versus is_edible
# include legend and y axis label
cap_shape_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(cap_shape, fill = factor(is_edible))) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("bell",
                              "conical",
                              "flat",
                              "knobbed",
                              "sunken",
                              "convex")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Cap Shape",
       y = "Number of Observations",
       fill = "edible") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1),
        axis.title.y = element_text(size = 10)) +
  theme(legend.title = element_text(size = 9),
        legend.key.size = unit(0.2, "cm"))

###############################################################
# create plot for cap_surface versus is_edible
# don't include legend or y axis label
cap_surface_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(cap_surface, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("fibrous",
                              "grooves",
                              "smooth",
                              "scaly")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Cap Surface",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

#################################################################
# create plot for cap_color versus is_edible
# don't include legend or y axis label
cap_color_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(cap_color, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("buff",
                              "cinnamon",
                              "red",
                              "gray",
                              "brown",
                              "pink",
                              "green",
                              "purple",
                              "white",
                              "yellow")) +
 scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Cap Color",
       y = "Number of Observations",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

#################################################################
# create plot for does_it_bruise versus is_edible
# include y axis label but not legend
does_it_bruise_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(does_it_bruise, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("false",
                              "true")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Does It Bruise",
       y = "Number of Observations",
       fill = "edible") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1),
        axis.title.y = element_text(size = 10))

################################################################
# create plot for odor versus is_edible
# don't include legend or y axis label
odor_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(odor, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("almond",
                              "creosote",
                              "foul",
                              "anise",
                              "musty",
                              "none",
                              "pungent",
                              "spicy",
                              "fishy")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Odor",
       y = "Number of Observations",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

#################################################################
# create plot for gill_attachment versus is_edible
# don't include legend or y axis label
gill_attachment_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(gill_attachment, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("attached",
                              "free")) +
 scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Gill Attachment",
       y = "Number of Observations",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

#################################################################
# create plot for gill_spacing versus is_edible
# include y axis label but not legend
gill_spacing_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(gill_spacing, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("close",
                              "crowded")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Gill Spacing",
       y = "Number of Observations",
       fill = "edible") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1),
        axis.title.y = element_text(size = 10))

################################################################
# create plot for gill_size versus is_edible
# don't include legend or y axis label
gill_size_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(gill_size, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("broad",
                              "narrow")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Gill Size",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

#################################################################
# create plot for gill_color versus is_edible
# don't include legend or y axis label
gill_color_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(gill_color, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("buff",
                              "red",
                              "gray",
                              "chocolate",
                              "black",
                              "brown",
                              "orange",
                              "pink",
                              "green",
                              "purple",
                              "white",
                              "yellow")) +
 scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Gill Color",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

###############################################################
# create plot for stalk_shape versus is_edible
# include y axis label but not legend
stalk_shape_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(stalk_shape, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("enlarging",
                              "tapering")) +
scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Stalk Shape",
       y = "Number of Observations",
       fill = "edible") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1),
        axis.title.y = element_text(size = 10))

##############################################################
# create plot for stalk_surface_above_ring versus is_edible
# don't include legend or y axis label
stalk_surface_above_ring_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(stalk_surface_above_ring, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("fibrous",
                              "silky",
                              "smooth",
                              "scaly")) +
 scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Stalk Surface Above Ring",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

###############################################################
# create plot for stalk_surface_below_ring versus is_edible
# don't include legend or y axis label
stalk_surface_below_ring_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(stalk_surface_below_ring, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("fibrous",
                              "silky",
                              "smooth",
                              "scaly")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Stalk Surface Below Ring",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

#################################################################
# create plot for stalk_color_above_ring versus is_edible
# include y axis label but not legend
stalk_color_above_ring_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(stalk_color_above_ring, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("buff",
                              "cinnamon",
                              "red",
                              "gray",
                              "brown",
                              "orange",
                              "pink",
                              "white",
                              "yellow")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Stalk Color Above Ring",
       y = "Number of Observations",
       fill = "edible") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1),
        axis.title.y = element_text(size = 10))

##################################################################
# create plot for stalk_color_below_ring versus is_edible
# don't include legend or y axis label
stalk_color_below_ring_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(stalk_color_below_ring, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("buff",
                              "cinnamon",
                              "red",
                              "gray",
                              "brown",
                              "orange",
                              "pink",
                              "white",
                              "yellow")) +
scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Stalk Color Below Ring",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

#################################################################
# create plot for veil_color versus is_edible
# don't include legend or y axis label
veil_color_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(veil_color, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("brown",
                              "orange",
                              "white",
                              "yellow")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Veil Color",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

#################################################################
# create plot for ring_number versus is_edible
# include y axis label but not legend
ring_number_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(ring_number, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("none",
                              "one",
                              "two")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Ring Number",
       y = "Number of Observations",
       fill = "edible") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1),
        axis.title.y = element_text(size = 10))

################################################################
# create plot for ring_type versus is_edible
# don't include legend or y axis label
ring_type_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(ring_type, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("evanescent",
                              "flaring",
                              "large",
                              "none",
                              "pendant")) +
 scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Ring Type",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

#################################################################
# create plot for spore_print_color versus is_edible
# don't include legend or y axis label
spore_print_color_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(spore_print_color, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("buff",
                              "chocolate",
                              "black",
                              "brown",
                              "orange",
                              "green",
                              "purple",
                              "white",
                              "yellow")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Spore Print Color",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))

#################################################################
# create plot for population versus is_edible
# include y axis label but not legend
population_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(population, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("abundant",
                              "clustered",
                              "numerous",
                              "scattered",
                              "several",
                              "solitary")) +
  scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Population",
       y = "Number of Observations",
       fill = "edible") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1),
        axis.title.y = element_text(size = 10))

#################################################################
# create plot for habitat versus is_edible
# don't include legend or y axis label
habitat_plot <- ggplot(data = mushroom_training_set) +
  geom_bar(aes(habitat, fill = factor(is_edible)),
           show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 6500),
                     labels = scales::comma) +
  scale_x_discrete(labels = c("woods",
                              "grasses",
                              "leaves",
                              "meadows",
                              "paths",
                              "urban",
                              "waste")) +
 scale_fill_manual(values = c("green", "red"),
                    labels = c("yes", "no")) +
  labs(x = "Habitat",
       fill = "edible") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
################################################################
# create first page of Figure 1
# Figure 1: Observations of Dependent Variables by Levels of Independent Variables
################################################################

# need cap_shape_plot, cap_surface_plot, cap_color_plot, does_it_bruise_plot, odor_plot, gill_attachment_plot

group_1 <- ggarrange(cap_shape_plot,
          cap_surface_plot,
          cap_color_plot,
          does_it_bruise_plot,
          odor_plot,
          gill_attachment_plot,
          ncol = 3,
          nrow = 2,
          common.legend = TRUE,
          legend = "top")

# set title for page one Figure 1
annotate_figure(group_1,
                top = text_grob("Figure 1: Observations of Dependent Variable \nby Levels of Independent Variables",
                                color = "black",
                                face = "bold",
                                size = 12))


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
#################################################################
# fill in next six plots on page 1 of Figure 1
#################################################################

# need gill_spacing_plot, gill_size_plot, gill_color_plot, stalk_shape_plot, stalk_surface_above_ring_plot, stalk_surface_below_ring_plot

ggarrange(gill_spacing_plot,
          gill_size_plot,
          gill_color_plot,
          stalk_shape_plot,
          stalk_surface_above_ring_plot,
          stalk_surface_below_ring_plot,
          ncol = 3,
          nrow = 2)


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
#################################################################
# create second page of Figure 1
# Figure 1 Continued: Observations of Dependent Variables by Levels of Independent Variables
################################################################
# need stalk_color_above_ring_plot, stalk_color_below_ring_plot, veil_color_plot, ring_number_plot, ring_type_plot, spore_print_color_plot

group_2 <- ggarrange(stalk_color_above_ring_plot,
          stalk_color_below_ring_plot,
          veil_color_plot,
          ring_number_plot,
          ring_type_plot,
          spore_print_color_plot,
          ncol = 3,
          nrow = 2)

# set title for page two of Figure 1
annotate_figure(group_2,
                top = text_grob("Figure 1 Continued: Observations of Dependent Variable \nby Levels of Independent Variables",
                                color = "black",
                                face = "bold",
                                size = 12))


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
################################################################
# fill in final 2 plots on page 2 of Figure 1
################################################################

# need population_plot, habitat_plot

ggarrange(population_plot,
          habitat_plot,
          ncol = 3,
          nrow = 2)


## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.align='center'------------------
##################################################################
# create Figure 2
#   Figure 2: Association Between Independent Variables Cramér's V of 0.5 or Greater
##################################################################

# plot of Cramér's V >= 0.5 for all independent variables

# install lsr package if needed
if(!require(lsr)) install.packages("lsr", repos = "http://cran.us.r-project.org")
# install corrplot package if needed
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
# install DescTools package if needed
if(!require(DescTools)) install.packages("DescTools", repos = "http://cran.us.r-project.org")

# lsr package contains cramersV() function
# corrplot package contains corrplot() function
# DescTools package contains PairApply() function

#call needed packages
library(lsr)
library(corrplot)
library(DescTools)

# create matrix of Cramér's V for all independent variables in training set
cramV_training <- as.data.frame(PairApply(mushroom_training_set[,-1],
                                          cramersV))

# change all Cramér's V < 5 to 0 in cramV_training
cramV_training[cramV_training < 0.5] <- NA

# set color palette for plot
col <- colorRampPalette(c("orange", "brown"))

# plot Cramér's V >= 0.5 for independent variables with diagonal removed 
corrplot(as.matrix(cramV_training),
         method = "color",
         type = "lower",
         title = "Figure 2: Association Between Independent Variables \nCramér's V of 0.5 or greater",
         is.corr = FALSE,
         diag = FALSE,
         mar = c(0, 0, 2, 0),
         addgrid.col = "grey",
         tl.col = "black",
         tl.cex = 0.8,
         tl.srt = 45,
         col = col(100),
         na.label.col = "white",
         cl.cex = 0.7)


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
##################################################################
# train logistic regression model with mushroom_training_set
##################################################################

logistic_model <- glm(is_edible ~ .,
                   family = binomial(link = "logit"),
                   data = mushroom_training_set)

# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
#################################################################
# create training and test sets to train regularized logistic model
#################################################################

# install glmnet package if needed
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")

# install mltools package if needed
if(!require(mltools)) install.packages("mltools", repos = "http://cran.us.r-project.org")

#call glmnet and mtools packages
library(glmnet)
library(mltools)

#################################################################
# need mushroom_training_set in one-hot coded matrix for glmnet()
#################################################################

# first change mushroom_training_set data frame into data table
#   for use in one_hot() function  
dtemp_training <- as.data.table(mushroom_training_set)

# now run one-hot() to create oh_mushroom_training_set
oh_mushroom_training_set <- one_hot(dtemp_training)

# need mushroom_test_set in one-hot coded matrix for glmnet()
# first change mushroom_test_set data frame into data table
#   for use in one_hot() function  
dtemp_test <- as.data.table(mushroom_test_set)

# now run one-hot() to create oh_mushroom_test_set
oh_mushroom_test_set <- one_hot(dtemp_test)

# remove unneeded objects
rm(dtemp_training, dtemp_test)

##############################################################
# create x_train_glmnet matrix and y_train_glmnet matrices for
#   model training - glmnet requires for family = "binomial"
##############################################################

# create x_train_glmnet matrix
x_train_glmnet <- as.matrix(oh_mushroom_training_set)[,-1:-2]
# create data frame to get dimensions for use in inline code
df_x_train_glmnet <- data.frame(rows = nrow(x_train_glmnet),
                                columns = ncol(x_train_glmnet))

# create y_train_glmnet matrix
# make is_edible_e second as second column is target class
y_train_glmnet <- as.matrix(oh_mushroom_training_set)[, 2:1]
# create data frame to get dimensions for use in inline code
df_y_train_glmnet <- data.frame(rows = nrow(y_train_glmnet),
                                columns = ncol(y_train_glmnet))

###########################################################
# create x_test_glmnet and y_test_glmnet matrices for
#   model testing - glmnet requires for family = "binomial"
###########################################################

# create x_test_glmnet matrix
x_test_glmnet <- as.matrix(oh_mushroom_test_set)[,-1:-2]
# create data frame to get dimensions for use in inline code
df_x_test_glmnet <- data.frame(rows = nrow(x_test_glmnet),
                                columns = ncol(x_test_glmnet))

# create y_test_glmnet matrix
# make is_edible_e second as second column is target class
y_test_glmnet <- as.matrix(oh_mushroom_test_set)[, 2:1]
# create data frame to get dimensions for use in inline code
df_y_test_glmnet <- data.frame(rows = nrow(y_test_glmnet),
                                columns = ncol(y_test_glmnet))


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
###############################################################
# train regularized logistic model with cross validation to determine optimal lambda (lambda.min) to minimize out of sample error
###############################################################

# train model x_train_glmnet and y_train_glmnet with:
#   family = "binomial", and
#   alpha = 1 (LASSO regularization)
# set.seed() for reproducibility as results from cv.glmnet are random
set.seed(2, sample.kind="Rounding")
cv_LASSO_model <- cv.glmnet(x_train_glmnet,
                               y_train_glmnet,
                               family = "binomial",
                               alpha = 1)
#############################################################

# create data frame of coefficients of regularized model using result of lambda.min from the cross validation
cv_lasso_coef_min_lambda <- as.data.frame(as.matrix(coef(cv_LASSO_model,
                           s = min(cv_LASSO_model$lambda))))

# create data frame of non_zero coefficients by naming coefficients column, filtering for non-zero coefficients, rounding coefficients to 1 significant digit and selecting them
non_zero_cv_lasso_coefficients <-cv_lasso_coef_min_lambda %>% 
  mutate(CV_LASSO_Coefficients = s1) %>% 
  filter(CV_LASSO_Coefficients != 0) %>% 
  mutate(CV_LASSO_Coefficients = round(CV_LASSO_Coefficients, 1)) %>% 
  select(CV_LASSO_Coefficients)


## ---- echo=FALSE, message=FALSE, warning=FALSE, comment = NA------------------------
################################################################
# print non-zero coefficients
################################################################

non_zero_cv_lasso_coefficients


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
#################################################################
#train random forest model
#################################################################

# install randomForest package if needed
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

# call randomForest package
library(randomForest)

################################################################

# train model with:
#   mushroom_training_set,
#   importance = TRUE
#   cutoff = c(0.99, .01) - use strict rule converting votes to prediction

set.seed(3, sample.kind="Rounding")
random_forest_model_1 <- randomForest(factor(is_edible) ~ .,
                             data = mushroom_training_set,
                             importance = TRUE,
                             cutoff = c(0.99, 0.01))

# Call:
#   randomForest(formula = factor(is_edible) ~ ., data = mushroom_training_set,      importance = TRUE, cutoff = c(0.99, 0.01)) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 4
# 
# OOB estimate of  error rate: 2.69%
# Confusion matrix:
#   e    p  class.error
# e 3191  175 0.0519904932
# p    0 3132 0.0000000000
################################################################

#look at plot of random_forest_model_1 which shows shows error as function number of trees - choose 300 trees for minimum error after first rise in error

# tune mtry done manually with:
#   independent variables in mushroom_training_set[, -1],
#   dependent variable in mushroom_training_set[, 1],
#   ntreeTry = 300,
#   stepFactor = 0.5,
#   improve = 0.002,
#   trace = TRUE,
#   plot = TRUE,
#   cutoff = c(0.99, 0.01))

# produces mtry = 8
################################################################

# use mtry = 8 and train random_forest_model with:
#   dependent variable is_edible,
#   independent variables in mushroom_training_set,
#   ntree = 500,
#   mtry = 8,
#   importance = TRUE,
#   cutoff = c(0.99, 0.01)

set.seed(4, sample.kind="Rounding")
random_forest_model <-randomForest(factor(is_edible) ~ .,
                                   ntree = 500,
                                   mtry = 8,
                                   data = mushroom_training_set,
                                   importance = TRUE,
                                   cutoff = c(0.99, 0.01))

# Call:
#  randomForest(formula = factor(is_edible) ~ ., data = mushroom_training_set,      ntree = 500, mtry = 8, importance = TRUE, cutoff = c(0.99,          0.01)) 
#                Type of random forest: classification
#                      Number of trees: 500
# No. of variables tried at each split: 8
# 
#         OOB estimate of  error rate: 0.12%
# Confusion matrix:
#      e    p class.error
# e 3358    8  0.00237671
# p    0 3132  0.00000000



## ---- echo=FALSE, message=FALSE, warning=FALSE, comment = NA------------------------
#################################################################
#create data frame with two column of random forest variables and mean decrease accuracy (mda) arranged by decreasing mda
#################################################################

rf_ID_var_mda <- as.data.frame(random_forest_model$importance) %>% 
  mutate(MeanDecreaseAccuracy = round(MeanDecreaseAccuracy, 3)) %>%
  select(MeanDecreaseAccuracy) %>% 
  arrange(desc(MeanDecreaseAccuracy))

rf_ID_var_mda


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
################################################################
# test regularized logistic model on test set
# predict cv_lasso_model using x_test_glmnet
################################################################

# use strict test of greater than 0.99  for probabilities to be set to prediction of 1, if 0.99 or lower set to 0

options(digits = 3)
cv_LASSO_model_test_probabilities <- predict(cv_LASSO_model,
                                                x_test_glmnet,
                                                s = "lambda.min",
                                                type = "response")

cv_LASSO_model_test_predictions <- ifelse(cv_LASSO_model_test_probabilities > 0.99, 1, 0) %>%
  factor()

# confusion matrix calculations assuming edible is positive class
cv_LASSO_confusion_matrix <- table(cv_LASSO_model_test_predictions,
                          factor(y_test_glmnet[, 2]))

cv_LASSO_accuracy <- (cv_LASSO_confusion_matrix[4] + cv_LASSO_confusion_matrix[1])/sum(cv_LASSO_confusion_matrix[1:4])

cv_LASSO_sensitivity <- cv_LASSO_confusion_matrix[4]/(cv_LASSO_confusion_matrix[4] + cv_LASSO_confusion_matrix[2])

cv_LASSO_specificity <- cv_LASSO_confusion_matrix[1]/(cv_LASSO_confusion_matrix[1] + cv_LASSO_confusion_matrix[3])

cv_LASSO_false_positives <- cv_LASSO_confusion_matrix[3]

cv_LASSO_false_negatives <- cv_LASSO_confusion_matrix[2]


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------------
################################################################
# test random forest model on test set
# predict random forest model on mushroom_test_set
################################################################

options(digits = 6)
set.seed(4, sample.kind = "Rounding")
random_forest_model_test_predictions <- predict(random_forest_model, mushroom_test_set)

# confusion matrix calculations assuming edible is positive class

rf_confusion_matrix <- table(random_forest_model_test_predictions,
                          factor(mushroom_test_set$is_edible))

rf_accuracy <- (rf_confusion_matrix[1] + rf_confusion_matrix[4])/sum(rf_confusion_matrix[1:4])

rf_sensitivity <- rf_confusion_matrix[1]/(rf_confusion_matrix[1] + rf_confusion_matrix[3])

rf_specificity <- rf_confusion_matrix[4]/(rf_confusion_matrix[4] + rf_confusion_matrix[2])

rf_false_positives <- rf_confusion_matrix[2]

rf_false_negatives <- rf_confusion_matrix[3]

