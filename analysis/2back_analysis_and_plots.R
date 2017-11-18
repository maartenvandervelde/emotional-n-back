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

use_tikz = FALSE # set to TRUE to save .tex versions of the plots

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

file_dir_1 <- paste0(data_path, "20171118")
file_dir_2 <- paste0(data_path, "20171118b")

beh_files <- c()
beh_files[1] <- tail(list.files(path = file_dir_1, pattern="beh.csv", full.names = TRUE),1)
beh_files[2] <- tail(list.files(path = file_dir_2, pattern="beh.csv", full.names = TRUE),1)

behdatfull <- data.frame()
for (i in 1:length(beh_files)) {
  behdatfull <- rbind(behdatfull, read.csv(beh_files[i], header=TRUE,sep=","))
}

behdat <- behdatfull %>%
  # filter(task_rep == 10) %>% # select only the last run
  select(-task_rep)


## For now let's label one model as the control model and the other as the depressed model
## NOTE: they are actually the old and new model, respectively

behdat <- behdat %>%
  mutate(type = ifelse(model == levels(model)[1], "control model", "depressed model")) %>%
  select(-model)




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




### Plot the results next to those of Levens and Gotlib

## Accuracy

# Filter out non-responses
allbehdat <- behdat
behdat <- behdat %>%
  filter(response != "none") %>%
  filter(rt <= 2.0) %>%   # Responses after 2s are too late
  filter(rt > 0) %>%      # Response times of 0s indicate missed trials
  filter(outcome != "none") # Non-response trials at the start of each block

# Filter out trials with extreme RTs (outside 2.5 SDs of each participant's mean RT)
behdat <- behdat %>%
  group_by(type, participant) %>%
  mutate(rt.mean = mean(rt), rt.sd = sd(rt)) %>%
  filter(rt >= rt.mean - 2.5 * rt.sd, rt <= rt.mean + 2.5 * rt.sd)



accdat <- behdat %>%
  filter(condition != "none") %>%
  mutate(accuracy = outcome == "correct") %>%
  group_by(participant, type, condition, stimulus) %>%
  summarise(acc = mean(accuracy)) %>%
  group_by(type, condition, stimulus) %>%
  summarise(acc.mean = mean(acc), acc.sd = sd(acc))


lg.acc.by.condition <- lgdat %>%
  rename(stimulus = expression) %>%
  mutate(acc.sd = NA) %>%
  select(stimulus, condition, acc.mean, acc.sd, type)

acc.by.condition.all <- rbind(lg.acc.by.condition, data.frame(accdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))


if (use_tikz) {
  tikz(file = paste0(fig_path, "2backAccuracyByCondition.tex"), width = 6, height = 3)
}

plot <- ggplot(acc.by.condition.all, aes(x = stimulus, y = acc.mean, group = type, fill = type)) +
  facet_grid(~condition, labeller = labeller(condition = c("break" = "break-set", match = "match-set", noset = "no-set", pers = "perseverance-set"))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  # scale_y_continuous(labels = percent) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=acc.mean-acc.sd, ymax=acc.mean+acc.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Condition", y = "Mean accuracy") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}





## Response rate

respdat <- allbehdat %>%
  mutate(ontime = outcome %in% c("correct", "wrong") & rt <= 2.0 & response != "none") %>%
  group_by(participant, type, condition, stimulus, ontime) %>%
  tally() %>%
  complete(ontime, fill = list(n = 0)) %>%
  group_by(participant, type, condition, stimulus) %>%
  mutate(freq = n/sum(n)) %>% # Represent as fraction
  filter(ontime == TRUE) %>%
  select(-ontime, -n) %>%
  group_by(type, condition, stimulus) %>%
  summarise(rr.mean = mean(freq), rr.sd = sd(freq))

lg.respdat <- lgdat %>%
  mutate(rr.sd = NA) %>%
  rename(stimulus = expression) %>%
  select(condition, stimulus, rr.mean, rr.sd, type)


respdat.all <- rbind(lg.respdat, data.frame(respdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))

if (use_tikz) {
  tikz(file = paste0(fig_path, "2backResponseRate.tex"), width = 6, height = 3)
}
plot <- ggplot(respdat.all, aes(x = stimulus, y = rr.mean, group = type, fill = type)) +
  facet_grid(~condition, labeller = labeller(condition = c("break" = "break-set", match = "match-set", noset = "no-set", pers = "perseverance-set"))) +
  geom_bar(stat = "identity", position=position_dodge(width=0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=rr.mean-rr.sd, ymax=rr.mean+rr.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Condition", y = "Response rate") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}



## Response time

rtdat <- behdat %>%
  filter(condition != "none") %>%
  # group_by(participant, type, condition, stimulus) %>%  # Uncomment these lines for more than one participant
  # summarise(rt.mean = mean(rt)) %>% 
  group_by(type, condition, stimulus) %>%
  summarise(rt.mean = mean(rt), rt.sd = sd(rt))

lg.rtdat <- lgdat %>%
  rename(stimulus = expression) %>%
  select(condition, stimulus, type, rt.mean, rt.sd)

rtdat.all <- rbind(lg.rtdat, data.frame(rtdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))

if (use_tikz) {
  tikz(file = paste0(fig_path, "2backResponseTime.tex"), width = 6, height = 3)
}

plot <- ggplot(rtdat.all, aes(x = stimulus, y = rt.mean, group = type, fill= type)) +
  facet_grid(~condition, labeller = labeller(condition = c("break" = "break-set", match = "match-set", noset = "no-set", pers = "perseverance-set"))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=rt.mean-rt.sd, ymax=rt.mean+rt.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Condition", y = "Mean RT (s)") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}







## z-transformed RT

zrtdat <- behdat %>%
  group_by(participant, type) %>%
  mutate(overall.rt = mean(rt)) %>%
  group_by(participant, type, condition, stimulus, overall.rt) %>%
  summarise(condition.rt = mean(rt), condition.rt.sd = sd(rt)) %>%
  mutate(z.rt = (condition.rt - overall.rt) / condition.rt.sd) %>%
  group_by(type, condition, stimulus) %>%
  summarise(z.rt.mean = mean(z.rt), z.rt.sd = sd(z.rt))

lg.zrtdat <- lgdat %>%
  rename(stimulus = expression) %>%
  select(condition, stimulus, z.rt.mean, z.rt.sd, type)

zrtdat.all <- rbind(lg.zrtdat, data.frame(zrtdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))

if (use_tikz) {
  tikz(file = paste0(fig_path, "2backResponseTimeZ.tex"), width = 6, height = 3)
}

plot <- ggplot(zrtdat.all, aes(x = stimulus, y = z.rt.mean, group = type, fill= type)) +
  facet_grid(~condition, labeller = labeller(condition = c("break" = "break-set", match = "match-set", noset = "no-set", pers = "perseverance-set"))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=z.rt.mean-z.rt.sd, ymax=z.rt.mean+z.rt.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Condition", y = "z-transformed mean RT") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}






## How pervasive is mind-wandering?

op_files <- c()
op_files[1] <- tail(list.files(path = file_dir_1, pattern="ops.csv", full.names = TRUE),1)
op_files[2] <- tail(list.files(path = file_dir_2, pattern="ops.csv", full.names = TRUE),1)

opdatfull <- data.frame()
for (i in 1:length(op_files)) {
  opdatfull <- rbind(opdatfull, read.csv(op_files[i], header=TRUE,sep=","))
}

opdat <- opdatfull %>%
  # filter(task_rep == 10) %>% # select only the last run
  select(-task_rep)


## For now let's label one model as the control model and the other as the depressed model
## NOTE: they are actually the model without mind-wandering (control) and the first model with MW (depressed)

opdat <- opdat %>%
  mutate(type = ifelse(model == levels(model)[1], "control model", "depressed model")) %>%
  mutate(on_task = as.logical(on_task)) %>%
  select(-model)




# When do we consider a trial to be "on-task"?
# Let's call any trial in which more than a certain percentage of operators are MW operators a mind-wandering trial for now.

mwfreq <- opdat %>%
  group_by(type, participant, trial, on_task) %>%
  count(on_task) %>%
  ungroup() %>%
  complete(type, participant, trial, on_task, fill = list(n = 0)) %>%
  group_by(type, participant, trial) %>%
  mutate(freq = n/sum(n)) %>%
  ungroup() %>%
  filter(on_task == FALSE)
  

mwfreq %>%
  group_by(type) %>%
  summarise(mw.mean = mean(freq), mw.sd = sd(freq))


mwfreq %>%
  mutate(mwtrial = freq >= 0.5) %>%
  group_by(type) %>%
  count(mwtrial)




## Operator use

opdatall <- opdatfull %>%
  select(-task_rep) %>%
  mutate(on_task = as.logical(on_task), success = as.logical(success))

# For now we just select two models and label one depressed and the other healthy
opdatall <- opdatall %>%
  mutate(group = ifelse(model == levels(model)[1], "control model", "depressed model")) %>%
  select(-model) %>%
  mutate(train = 0, span = 0)


# How often was each operator used?
opfreq <- opdatall %>%
  filter(success == TRUE) %>%
  group_by(group, participant, operator, on_task) %>%
  count() %>%
  group_by(group, participant) %>%
  mutate(freq = n/sum(n)) %>%
  group_by(group, operator, on_task) %>%
  summarise(freq.mean = mean(freq), freq.sd = sd(freq))




ggplot(opfreq, aes(x = operator, y = freq.mean, group = group, fill= group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  coord_flip() +
  facet_grid(on_task ~ ., scales = "free", switch = "both", as.table = FALSE, labeller = labeller(on_task = c("TRUE" = "Task operators", "FALSE" = "Mind-wandering operators"))) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=freq.mean-freq.sd, ymax=freq.mean+freq.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Operator", y = "Frequency of use") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  fillScale


## How long are thought trains?
# Continuous thought trains are unbroken sequences of successful off-task operators
# In the same vein, we can define spans of attention as unbroken sequences of successful on-task operators 

train <- 0
in_train <- FALSE

span <- 0
on_task <- FALSE


for (i in 1:(nrow(opdatall) - 1)) {
  
  # Go over items in sets of two
  this_op <- opdatall[i,]
  next_op <- opdatall[i+1,]
  if (this_op$participant == next_op$participant && this_op$on_task == FALSE && next_op$on_task == FALSE && this_op$success == TRUE && next_op$success == TRUE) {
    
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
  
  if (this_op$participant == next_op$participant && this_op$on_task == TRUE && next_op$on_task == TRUE && this_op$success == TRUE && next_op$success == TRUE) {
      
    # The pair of operators constitutes an "attention span"
    
    if (!on_task) {
      on_task <- TRUE
      span <- span + 1
    }
    
    opdatall[i,]$span <- span
    opdatall[i+1,]$span <- span
    
  } else {
    on_task <- FALSE
  }
  
}


# Thought train length

mw_train_length <- opdatall %>%
  filter(train != 0) %>%
  group_by(participant, group, train) %>%
  tally() %>%
  summarise(length = mean(n)) %>%
  group_by(group) %>%
  summarise(length.mean = mean(length), length.sd = sd(length))


if (use_tikz) {
  tikz(file = paste0(fig_path, "2backThoughtTrainLength.tex"), width = 6, height = 3)
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


# Span of attention length

attention_span_length <- opdatall %>%
  filter(span != 0) %>%
  group_by(participant, group, span) %>%
  tally() %>%
  summarise(length = mean(n)) %>%
  group_by(group) %>%
  summarise(length.mean = mean(length), length.sd = sd(length))

if (use_tikz) {
  tikz(file = paste0(fig_path, "2backAttentionSpanLength.tex"), width = 6, height = 3)
}

plot <- ggplot(attention_span_length, aes(x = group, y = length.mean, fill= group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=length.mean-length.sd, ymax=length.mean+length.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Group", y = "Attention span length") +
  fillScale +
  guides(fill = FALSE)

print(plot)
if (use_tikz) {
  dev.off()
}



## Let's look specifically at how responses were made.
## What proportion of responses was made automatically?

opdatall %>%
  filter(operator %in% c("two-back-match-press-same", "two-back-no-match-press-diff", "process-memory-auto-respond-same", "process-memory-auto-respond-diff")) %>%
  filter(success == TRUE) %>%
  mutate(automatic = !on_task) %>%
  group_by(group, automatic) %>%
  count() %>%
  group_by(group) %>%
  mutate(freq = n/sum(n))



## Conditional probabilities of operators

# What is the chance of selecting any given operator at the start of a trial?

first_ops <- opdatall %>%
  group_by(group, participant, trial) %>%
  slice(1) %>%
  group_by(group, participant, on_task, operator) %>%
  tally() %>%
  mutate(p = n/sum(n)) %>%
  group_by(group, operator, on_task) %>%
  summarise(p.mean = mean(p), p.sd = sd(p))


ggplot(first_ops, aes(x = operator, y = p.mean, group = group, fill= group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  coord_flip() +
  facet_grid(on_task ~ ., scales = "free", switch = "both", as.table = FALSE, labeller = labeller(on_task = c("TRUE" = "Task operators", "FALSE" = "Mind-wandering operators"))) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=p.mean-p.sd, ymax=p.mean+p.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Operator", y = "Probability of use at trial start") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  fillScale



