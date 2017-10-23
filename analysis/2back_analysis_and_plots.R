###
##
##  Analysis of depressed and control 2-back models
##
##
###

#################
###   SETUP   ###
#################

rm(list=ls(all=T))

library(tidyverse)
library(scales)
library(forcats)
library(tikzDevice)
options("tikzDocumentDeclaration" = "\\documentclass[12pt]{article}\n") # Default is 10pt.

use_tikz = TRUE # set to TRUE to save .tex versions of the plots

data_path <- "/Users/maarten/Dropbox/Masterproject/emotional-n-back/data/"
fig_path <- "/Users/maarten/Dropbox/Masterproject/emotional-n-back/fig/"


#Create a custom colour scale
group_colours <- c("blue", "darkgreen", "red", "orange")
group_colours <- c("#5f91e2", "#5fc5e2", "#c4912d", "#e2ba5f")
names(group_colours) <- c("control", "control model", "depressed", "depressed model")
fillScale <- scale_fill_manual(name = "type",values = group_colours)



########################
### BEHAVIOURAL DATA ###
########################

### Read in Levens and Gotlib empirical data for comparison

lgdat <- read.csv(paste0(data_path, "lgdat_2back.csv"))


## Read in and reformat model data

file_dir <- paste0(data_path, "20171023")

beh_files <- tail(list.files(path = file_dir, pattern="beh.csv", full.names = TRUE),1)

behdatfull <- data.frame()
for (i in 1:length(beh_files)) {
  behdatfull <- rbind(behdatfull, read.csv(beh_files[i], header=TRUE,sep=","))
}

behdat <- behdatfull %>%
  filter(task_rep == 10) %>% # select only the last run
  select(-task_rep)


# Add outcome and condition of trial (as in Levens and Gotlib)
behdat$outcome = ""
behdat$condition = ""
for (i in 1:nrow(behdat)) {
  
  # The first two trials of each block are discarded, no 2-back exists in either
  if (behdat[i,]$trial %% 55 %in% 1:2) {
    behdat[i,]$outcome = "none"
    behdat[i,]$condition = "none"
  } else {
    expected_answer = ifelse(behdat[i,]$stimulus == behdat[i-2,]$stimulus, "same", "diff")
    behdat[i,]$outcome = ifelse(behdat[i,]$response == expected_answer, "correct", "wrong")
    
    if (expected_answer == "same") {
      # Match-set trial (same stimulus as two trials ago)
      behdat[i,]$condition = "match"
    } else {
      if (behdat[i-1,]$condition == "match") {
        if (behdat[i-1,]$stimulus == behdat[i,]$stimulus) {
          # Perseverance-set trial (diff. than 2-back, but same as match-set in previous trial)
          behdat[i,]$condition = "pers"
        } else {
          # Break-set trial (diff. than 2-back, and diff. than match-set in previous trial)
          behdat[i,]$condition = "break"
        }
      } else {
        behdat[i,]$condition = "noset"
      }
    }
  }
}

behdat$outcome <- as.factor(behdat$outcome)
behdat$condition <- as.factor(behdat$condition)


## Response time

rt_by_cond_model <- behdat %>%
  filter(condition != "none") %>%
  group_by(condition, stimulus) %>%
  summarise(rt.mean = mean(rt), rt.sd = sd(rt)) %>%
  mutate(type = as.factor("control model")) %>%
  rename(expression = stimulus)

rt_by_cond_lg <- lgdat %>%
  select(condition:rt.sd)

rt_by_cond <- rbind(data.frame(rt_by_cond_model), rt_by_cond_lg)

if (use_tikz) {
  tikz(file = paste0(fig_path, "2backResponseTime"), width = 6, height = 3)
}

plot <- ggplot(rt_by_cond, aes(x = expression, y = rt.mean, group = type, fill = type)) +
  facet_grid(~condition, labeller = labeller(condition = c("break" = "break set", match = "match set", noset = "no set", pers = "perseverance set"))) +
  geom_bar(stat = "identity", position=position_dodge(width=0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=rt.mean-rt.sd, ymax=rt.mean+rt.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Stimulus", y = "Response time (s)") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}


## Response rate

rr_by_condition <- behdat %>%
  filter(condition != "none") %>%
  group_by(condition, stimulus, response) %>%
  tally() ...
 # Need some non-response trials first


## Accuracy

acc_by_condition <- behdat %>%
  filter(condition != "none") %>%
  group_by(condition, stimulus, outcome) %>%
  tally() ...



