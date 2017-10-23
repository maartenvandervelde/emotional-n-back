###
##
## Analysis of depressed and control 0-back models
##
###


### Setup

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

lgdat <- read.csv(paste0(data_path, "lgdat_0back.csv"))
lgdat$type <- factor(lgdat$type, levels=rev(levels(lgdat$type)))
lgdat$response <- factor(lgdat$response, levels = rev(levels(lgdat$response)))





### Read in data for control and depressed models

file_dir <- paste0(data_path, "20171011") # location of two sets of model output files

beh_files <- tail(list.files(path = file_dir, pattern="beh.csv", full.names = TRUE),2)

behdatfull <- data.frame()
for (i in 1:length(beh_files)) {
  behdatfull <- rbind(behdatfull, read.csv(beh_files[i], header=TRUE,sep=","))
}

behdat <- behdatfull %>%
  filter(task_rep == 10) %>% # Select only the last run of the "wandering" model
  select(-task_rep)


## For now let's label one model as the control model and the other as the depressed model

behdat <- behdat %>%
  mutate(type = ifelse(model == levels(model)[1], "control model", "depressed model")) %>%
  select(-model)
  



### Plot the results next to those of Levens and Gotlib

## Accuracy

# Filter out non-responses
allbehdat <- behdat
behdat <- behdat %>% filter(outcome != "missed")

# Filter out trials with extreme RTs (outside 2.5 SDs of each participant's mean RT)
behdat <- behdat %>%
  group_by(participant) %>%
  mutate(rt.mean = mean(rt), rt.sd = sd(rt)) %>%
  filter(rt >= rt.mean - 2.5 * rt.sd, rt <= rt.mean + 2.5 * rt.sd)

### Accuracy
behdat <- behdat %>% 
  mutate(expected_response = as.factor(ifelse(stimulus == target, "same", "diff")), accuracy = (outcome == "correct"))


accdat <- behdat %>%
  group_by(participant, type, stimulus, expected_response) %>%
  summarise(acc = mean(accuracy)) %>%
  group_by(type, stimulus, expected_response) %>%
  summarise(acc.mean = mean(acc), acc.sd = sd(acc))
  

lg.acc.by.response <- lgdat %>%
  rename(stimulus = expression, expected_response = response) %>%
  mutate(acc.sd = NA, type = type) %>%
  select(stimulus, expected_response, acc.mean, acc.sd, type)

acc.by.response.all <- rbind(lg.acc.by.response, data.frame(accdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))


if (use_tikz) {
  tikz(file = paste0(fig_path, "0backAccuracyByResponse.tex"), width = 6, height = 3)
}

plot <- ggplot(acc.by.response.all, aes(x = expected_response, y = acc.mean, group = type, fill = type)) +
  facet_grid(~stimulus, labeller = labeller(stimulus = c(happy = "stimulus: happy", neutral = "stimulus: neutral", sad = "stimulus: sad"))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  # scale_y_continuous(labels = percent) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=acc.mean-acc.sd, ymax=acc.mean+acc.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Expected response", y = "Mean accuracy") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}



## Response rate

respdat <- allbehdat %>%
  mutate(expected_response = as.factor(ifelse(stimulus == target, "same", "diff")), ontime = (outcome %in% c("correct", "wrong"))) %>%
  group_by(participant, type, target, expected_response, ontime) %>%
  tally() %>%
  complete(ontime, fill = list(n = 0)) %>%
  group_by(participant, type, target, expected_response) %>%
  mutate(freq = n/sum(n)) %>% # Represent as fraction
  filter(ontime == TRUE) %>%
  select(-ontime, -n) %>%
  group_by(type, target, expected_response) %>%
  summarise(rr.mean = mean(freq), rr.sd = sd(freq)) %>%
  rename(expression = target)

lg.respdat <- lgdat %>%
  mutate(rr.sd = NA) %>%
  rename(expected_response = response) %>%
  select(expression, expected_response, rr.mean, rr.sd, type)


respdat.all <- rbind(lg.respdat, data.frame(respdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))

if (use_tikz) {
  tikz(file = paste0(fig_path, "0backResponseRate.tex"), width = 6, height = 3)
}
plot <- ggplot(respdat.all, aes(x = expected_response, y = rr.mean, group = type, fill = type)) +
  facet_grid(~expression, labeller = labeller(expression = c(happy = "target: happy", neutral = "target: neutral", sad = "target: sad"))) +
  geom_bar(stat = "identity", position=position_dodge(width=0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=rr.mean-rr.sd, ymax=rr.mean+rr.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Expected response", y = "Response rate") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}

## Response time

rtdat <- behdat %>%
  group_by(participant, type, stimulus, expected_response) %>%
  summarise(rt = mean(rt)) %>%
  group_by(type, stimulus, expected_response) %>%
  summarise(rt.mean = mean(rt), rt.sd = sd(rt)) %>%
  rename(expression = stimulus)

lg.rtdat <- lgdat %>%
  rename(expected_response = response) %>%
  select(expression, expected_response, rt.mean, rt.sd, type)

rtdat.all <- rbind(lg.rtdat, data.frame(rtdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))

if (use_tikz) {
  tikz(file = paste0(fig_path, "0backResponseTime.tex"), width = 6, height = 3)
}

plot <- ggplot(rtdat.all, aes(x = expected_response, y = rt.mean, group = type, fill= type)) +
  facet_grid(~expression, labeller = labeller(stimulus = c(happy = "stimulus: happy", neutral = "stimulus: neutral", sad = "stimulus: sad"))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=rt.mean-rt.sd, ymax=rt.mean+rt.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Expression", y = "Mean RT (s)") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}

## z-transformed RT

zrtdat <- behdat %>%
  group_by(participant, type) %>%
  mutate(overall.rt = mean(rt)) %>%
  group_by(participant, type, stimulus, expected_response, overall.rt) %>%
  summarise(condition.rt = mean(rt), condition.rt.sd = sd(rt)) %>%
  mutate(z.rt = (condition.rt - overall.rt) / condition.rt.sd) %>%
  group_by(type, stimulus, expected_response) %>%
  summarise(z.rt.mean = mean(z.rt), z.rt.sd = sd(z.rt)) %>%
  rename(expression = stimulus)

lg.zrtdat <- lgdat %>%
  rename(expected_response = response) %>%
  select(expression, expected_response, z.rt.mean, z.rt.sd, type)

zrtdat.all <- rbind(lg.zrtdat, data.frame(zrtdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))

if (use_tikz) {
  tikz(file = paste0(fig_path, "0backResponseTimeZ.tex"), width = 6, height = 3)
}

plot <- ggplot(zrtdat.all, aes(x = expected_response, y = z.rt.mean, group = type, fill= type)) +
  facet_grid(~expression, labeller = labeller(expression = c(happy = "stimulus: happy", neutral = "stimulus: neutral", sad = "stimulus: sad"))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=z.rt.mean-z.rt.sd, ymax=z.rt.mean+z.rt.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Expression", y = "z-transformed mean RT") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}



###############################
### MIND-WANDERING CONTENTS ###
###############################

mem_files <- tail(list.files(path = file_dir, pattern="mems.csv", full.names = TRUE),2)

memdatfull <- data.frame()
for (i in 1:length(mem_files)) {
  memdatfull <- rbind(memdatfull, read.csv(mem_files[i], header=TRUE,sep=","))
}

memdatall <- memdatfull %>%
  filter(task_rep == 10) %>% # Select only the last run of the "wandering" model
  select(-task_rep)

# For now we just select two models and pretend that one is depressed and the other healthy
memdatall <- memdatall %>%
  mutate(group = ifelse(model == levels(model)[1], "control model", "depressed model")) %>%
  select(-model)

memdat <- memdatall %>%
  filter(type == "memory") %>% # Select only memory retrievals
  select(-type) %>%
  mutate(mood = gsub("\\d+", "", retrieval_item)) # Extract mood from each memory

# Overall distribution of moods
mw_mood_freq <- memdat %>%
  group_by(participant, group, mood) %>%
  tally() %>%
  group_by(participant, group) %>%
  mutate(freq = n/sum(n)) %>%
  group_by(group, mood) %>%
  summarise(freq.mean = mean(freq), freq.sd = sd(freq))

if (use_tikz) {
  tikz(file = paste0(fig_path, "0backMWmoodDistribution.tex"), width = 6, height = 3)
}

plot <- ggplot(mw_mood_freq, aes(x = mood, y = freq.mean, group = group, fill= group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  # scale_y_continuous(labels=percent) +
  geom_errorbar(aes(ymin=freq.mean-freq.sd, ymax=freq.mean+freq.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Memory mood", y = "Frequency") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}

## Total number of retrievals
mw_retrieval_count <- memdat %>%
  group_by(participant, group) %>%
  tally() %>%
  group_by(group) %>%
  summarise(count.mean = mean(n), count.sd = sd(n))


if (use_tikz) {
  tikz(file = paste0(fig_path, "0backNumberOfRetrievals.tex"), width = 6, height = 3)
}

plot <- ggplot(mw_retrieval_count, aes(x = group, y = count.mean, fill= group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=count.mean-count.sd, ymax=count.mean+count.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Model", y = "Number of retrievals") +
  fillScale + 
  guides(fill=FALSE)

print(plot)
if (use_tikz) {
  dev.off()
}

## Retrieval latency by mood
mw_retrieval_latency <- memdat %>%
  group_by(participant, group, mood) %>%
  summarise(latency = mean(latency) * 1000) %>%
  group_by(group, mood) %>%
  summarise(latency.mean = mean(latency), latency.sd = sd(latency))


if (use_tikz) {
  tikz(file = paste0(fig_path, "0backRetrievalLatency.tex"), width = 6, height = 3)
}

plot <- ggplot(mw_retrieval_latency, aes(x = mood, y = latency.mean, group = group, fill= group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=latency.mean-latency.sd, ymax=latency.mean+latency.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Memory mood", y = "Retrieval latency (ms)") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}

## Retrievals per trial
mw_retrievals_per_trial <- memdat %>%
  group_by(participant, group, trial) %>%
  tally() %>%
  group_by(participant, group) %>%
  summarise(count = mean(n)) %>%
  group_by(group) %>%
  summarise(count.mean = mean(count), count.sd= sd(count))


if (use_tikz) {
  tikz(file = paste0(fig_path, "0backRetrievalCount.tex"), width = 6, height = 3)
}

plot <- ggplot(mw_retrievals_per_trial, aes(x = group, y = count.mean, fill= group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=count.mean-count.sd, ymax=count.mean+count.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Model", y = "Number of retrievals per trial") +
  fillScale +
  guides(fill=FALSE)

print(plot)
if (use_tikz) {
  dev.off()
}


## Proportion of retrievals that is off-task

prop_offtask <- memdatall %>%
  group_by(participant, group, type) %>%
  tally() %>%
  group_by(participant, group) %>%
  mutate(freq = n/sum(n)) %>%
  group_by(group, type) %>%
  summarise(freq.mean = mean(freq), freq.sd = sd(freq)) %>%
  filter(type == "memory")


if (use_tikz) {
  tikz(file = paste0(fig_path, "0backRetrievalsOffTask.tex"), width = 6, height = 3)
}


plot <- ggplot(prop_offtask, aes(x = group, y = freq.mean, fill= group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  # scale_y_continuous(labels=percent) +
  geom_errorbar(aes(ymin=freq.mean-freq.sd, ymax=freq.mean+freq.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Group", y = "Off-task retrievals") +
  fillScale +
  guides(fill = FALSE)

print(plot)
if (use_tikz) {
  dev.off()
}
  



## Duration of mind-wandering episodes

# It makes more sense to look at the operator data, since that includings timing of operators firing during MW



#################
### OPERATORS ###
#################

op_files <- tail(list.files(path = file_dir, pattern="ops.csv", full.names = TRUE),2)

opdatfull <- data.frame()
for (i in 1:length(op_files)) {
  opdatfull <- rbind(opdatfull, read.csv(op_files[i], header=TRUE,sep=","))
}

opdatall <- opdatfull %>%
  filter(task_rep == 10) %>% # Select only the last run of the "wandering" model
  select(-task_rep) %>%
  mutate(on_task = as.logical(on_task), success = as.logical(success))

# For now we just select two models and label one depressed and the other healthy
opdatall <- opdatall %>%
  mutate(group = ifelse(model == levels(model)[1], "control model", "depressed model")) %>%
  select(-model) %>%
  mutate(train = 0)


# Continuous thought trains are unbroken sequences of successful off-task operators

train <- 0
in_train <- FALSE

for (i in 1:(nrow(opdatall) - 1)) {
  
  # Go over items in sets of two
  this_op <- opdatall[i,]
  next_op <- opdatall[i+1,]
  if (this_op$participant == next_op$participant && this_op$on_task == FALSE && next_op$on_task == FALSE && this_op$success != FALSE && next_op$success != FALSE) {
    
    # The pair of operators constitutes a thought train
    
    if (!in_train) {
      in_train <- TRUE
      train <- train + 1
    }
    
    opdatall[i,]$train <- train
    opdatall[i+1,]$train <- train
    
  } else {
    in_train <- FALSE
  }
}


mw_train_length <- opdatall %>%
  filter(train != 0) %>%
  group_by(participant, group, train) %>%
  tally() %>%
  summarise(length = mean(n)) %>%
  group_by(group) %>%
  summarise(length.mean = mean(length), length.sd = sd(length))


if (use_tikz) {
  tikz(file = paste0(fig_path, "0backThoughtTrainLength.tex"), width = 6, height = 3)
}

plot <- ggplot(mw_train_length, aes(x = group, y = length.mean, fill= group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=length.mean-length.sd, ymax=length.mean+length.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Group", y = "Thought train length") +
  fillScale +
  guides(fill = FALSE)

print(plot)
if (use_tikz) {
  dev.off()
}

