###
##
##  Analysis of depressed and control 2-back models
##
##
###

#################
###   SETUP   ###
#################

library(tidyverse)
library(magrittr)
library(stringr)
library(scales)
library(forcats)
library(qgraph)
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

#file_dir_control <- paste0(data_path, "20171204b")
#file_dir_depressed <- paste0(data_path, "20171204d")
file_dir_control <- paste0(data_path, "2backnew/20171211")
file_dir_depressed <- paste0(data_path, "2backnew/20171219c")


beh_files <- c()
beh_files[1] <- tail(list.files(path = file_dir_control, pattern="beh.csv", full.names = TRUE),1)
beh_files[2] <- tail(list.files(path = file_dir_depressed, pattern="beh.csv", full.names = TRUE),1)

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
outcome <- rep("", nrow(behdat))
condition <- rep("", nrow(behdat))
valence <- behdat$stimulus

for (i in 1:nrow(behdat)) {
  
  # The first two trials of each block are discarded, no 2-back exists in either
  if (behdat[i,]$trial %% 55 %in% 1:2) {
    outcome[i] <- "none"
    condition[i] <- "none"
  } else {
    expected_answer <- ifelse(behdat[i,]$stimulus == behdat[i-2,]$stimulus, "same", "diff")
    outcome[i] <- ifelse(behdat[i,]$response == expected_answer, "correct", "wrong")
    
    if (expected_answer == "same") {
      # Match-set trial (same stimulus as two trials ago)
      condition[i] <- "match"
    } else {
      if (condition[i-1] == "match") {
        if (behdat[i-1,]$stimulus == behdat[i,]$stimulus) {
          # Perseverance-set trial (diff. than 2-back, but same as match-set in previous trial)
          condition[i] <- "pers"
        } else {
          # Break-set trial (diff. than 2-back, and diff. than match-set in previous trial)
          condition[i] <- "break"
          
          # In this case, the relevant valence is that of the broken set, not the current stimulus
          valence[i] <- valence[i-1] 
        }
      } else {
        condition[i] <- "noset"
      }
    }
  }
}

behdat$outcome <- as.factor(outcome)
behdat$condition <- as.factor(condition)
behdat$valence <- as.factor(valence)


### Plot the results next to those of Levens and Gotlib

## Accuracy

allbehdat <- behdat
behdat <- behdat %>%
  filter(response != "none") %>% # Filter out non-responses
  filter(rt <= 2.0) %>%   # Responses after 2s are too late
  filter(rt > 0) %>%      # Response times of 0s indicate missed trials
  filter(outcome != "none") # Non-response trials at the start of each block

# Filter out trials with extreme RTs (outside 2.5 SDs of each participant's mean RT)
behdat <- behdat %>%
  group_by(type, participant) %>%
  mutate(rt.mean = mean(rt), rt.sd = sd(rt)) %>%
  filter(rt >= rt.mean - 2.5 * rt.sd, rt <= rt.mean + 2.5 * rt.sd)



accdat <- behdat %>%
  mutate(accuracy = outcome == "correct") %>%
  group_by(participant, type, condition, valence) %>%
  summarise(acc = mean(accuracy)) %>%
  group_by(type, condition, valence) %>%
  summarise(acc.mean = mean(acc), acc.sd = sd(acc))


lg.acc.by.condition <- lgdat %>%
  rename(valence = expression) %>%
  mutate(acc.sd = NA) %>%
  select(valence, condition, acc.mean, acc.sd, type)

acc.by.condition.all <- rbind(lg.acc.by.condition, data.frame(accdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))


if (use_tikz) {
  tikz(file = paste0(fig_path, "2backAccuracyByCondition.tex"), width = 6, height = 3)
}

plot <- ggplot(acc.by.condition.all, aes(x = valence, y = acc.mean, group = type, fill = type)) +
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
  group_by(participant, type, condition, valence, ontime) %>%
  tally() %>%
  complete(ontime, fill = list(n = 0)) %>%
  group_by(participant, type, condition, valence) %>%
  mutate(freq = n/sum(n)) %>% # Represent as fraction
  filter(ontime == TRUE) %>%
  select(-ontime, -n) %>%
  group_by(type, condition, valence) %>%
  summarise(rr.mean = mean(freq), rr.sd = sd(freq))

lg.respdat <- lgdat %>%
  mutate(rr.sd = NA) %>%
  rename(valence = expression) %>%
  select(condition, valence, rr.mean, rr.sd, type)


respdat.all <- rbind(lg.respdat, data.frame(respdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))

if (use_tikz) {
  tikz(file = paste0(fig_path, "2backResponseRate.tex"), width = 6, height = 3)
}
plot <- ggplot(respdat.all, aes(x = valence, y = rr.mean, group = type, fill = type)) +
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
  filter(outcome == "correct") %>%
  group_by(participant, type, condition, valence) %>%
  summarise(rt = mean(rt)) %>% 
  group_by(type, condition, valence) %>%
  summarise(rt.mean = mean(rt), rt.sd = sd(rt))

lg.rtdat <- lgdat %>%
  rename(valence = expression) %>%
  select(condition, valence, type, rt.mean, rt.sd)

rtdat.all <- rbind(lg.rtdat, data.frame(rtdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))

if (use_tikz) {
  tikz(file = paste0(fig_path, "2backResponseTime.tex"), width = 6, height = 3)
}

plot <- ggplot(rtdat.all, aes(x = valence, y = rt.mean, group = type, fill= type)) +
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
  filter(outcome == "correct") %>%
  group_by(participant, type) %>%
  mutate(overall.rt = mean(rt)) %>%
  group_by(participant, type, condition, valence, overall.rt) %>%
  summarise(condition.rt = mean(rt), condition.rt.sd = sd(rt)) %>%
  mutate(z.rt = (condition.rt - overall.rt) / condition.rt.sd) %>%
  group_by(type, condition, valence) %>%
  summarise(z.rt.mean = mean(z.rt), z.rt.sd = sd(z.rt))

lg.zrtdat <- lgdat %>%
  rename(valence = expression) %>%
  select(condition, valence, z.rt.mean, z.rt.sd, type)

zrtdat.all <- rbind(lg.zrtdat, data.frame(zrtdat)) %>%
  mutate(type = fct_relevel(type, "control", "control model", "depressed", "depressed model"))

if (use_tikz) {
  tikz(file = paste0(fig_path, "2backResponseTimeZ.tex"), width = 6, height = 3)
}

plot <- ggplot(zrtdat.all, aes(x = valence, y = z.rt.mean, group = type, fill= type)) +
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


## Plot just the conditions with differences between groups
zrtdat.all.selection <- zrtdat.all %>%
  #filter(condition == "break" & valence == "happy" | condition == "break" & valence == "sad" | condition == "noset" & valence == "sad")
  filter(condition %in% c("break")) %>%
  mutate(valence = factor(valence, levels = c("sad", "neutral", "happy")))

if (use_tikz) {
  tikz(file = paste0(fig_path, "2backResponseTimeZselection.tex"), width = 6, height = 3)
}

plot <- ggplot(zrtdat.all.selection, aes(x = valence, y = z.rt.mean, group = type, fill= type)) +
  #facet_grid(~condition, scales = "free", space = "free_x", labeller = labeller(condition = c("break" = "break-set", match = "match-set", noset = "no-set", pers = "perseverance-set"))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=z.rt.mean-z.rt.sd, ymax=z.rt.mean+z.rt.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "", y = "z-transformed mean RT") +
  fillScale +
  guides(fill = FALSE)

print(plot)
if (use_tikz) {
  dev.off()
}


########################
###  OPERATOR DATA   ###
########################


## How pervasive is mind-wandering?

op_files <- c()
op_files[1] <- tail(list.files(path = file_dir_control, pattern="ops.csv", full.names = TRUE),1)
op_files[2] <- tail(list.files(path = file_dir_depressed, pattern="ops.csv", full.names = TRUE),1)

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
  

# Percentage of operators from mind-wander goal
mw_share_of_ops <- mwfreq %>%
  group_by(type) %>%
  summarise(mw.mean = mean(freq), mw.sd = sd(freq))


# Percentage of trials in which mind-wandering operators dominate (at least 50%)
mw_dominant <- mwfreq %>%
  mutate(mwtrial = freq >= 0.5) %>%
  group_by(type) %>%
  count(mwtrial) %>%
  mutate(freq = nn/sum(nn))




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

span_count <- 0
span <- rep(0, nrow(opdatall))
on_task <- FALSE

train_count <- 0
train <- rep(0, nrow(opdatall))
in_train <- FALSE

d <- select(opdatall, participant, on_task, success)

for (i in 1:(nrow(opdatall) - 1)) {
  
  # Go over items in sets of two
  this_op <- d[i,]
  next_op <- d[i+1,]
  
  if (this_op$participant != next_op$participant || this_op$on_task != next_op$on_task || this_op$success == FALSE || next_op$success == FALSE) {
    in_train <- FALSE
    on_task <- FALSE
    next
  }
  
  
  if (this_op$on_task) {
    # The current and the next operator are part of an attention span
    
    if (!on_task) {
      on_task <- TRUE
      span_count <- span_count + 1
      span[i] <- span_count
    }
    
    span[i+1] <- span_count
    
    
  } else {
    # The current and the next operator are part of a thought train
    
    if(!in_train) {
      in_train <- TRUE
      train_count <- train_count + 1
      train[i] <- train_count
    }
    
    train[i+1] <- train_count
  }
}

opdatall$span <- span
opdatall$train <- train



# Thought train length (number of operators)

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


# Thought train duration (seconds)
mw_train_duration <- opdatall %>%
  filter(train != 0) %>%
  group_by(train) %>%
  mutate(time = cumsum(exec_time)) %>%
  slice(n()) %>% # select only the last element of each train, which has the total time
  group_by(group, participant) %>%
  summarise(time = mean(time)) %>%
  group_by(group) %>%
  summarise(time.mean = mean(time), time.sd = sd(time))




# Span of attention length (number of operators)

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

auto_responses <- opdatall %>%
  filter(operator %in% c("two-back-match-press-same", "two-back-no-match-press-diff", "process-memory-auto-respond-same", "process-memory-auto-respond-diff")) %>%
  filter(success == TRUE) %>%
  mutate(automatic = !on_task) %>%
  group_by(group, automatic) %>%
  count() %>%
  group_by(group) %>%
  mutate(freq = n/sum(n))




## Prior probabilities of operators

# How likely is each operator to be used regardless of context?

opfreq <-opdatall %>%
  group_by(group, participant, on_task, operator) %>%
  tally() %>%
  group_by(group, participant) %>%
  mutate(freq = n/sum(n)) %>%
  group_by(group, on_task, operator) %>%
  summarise(freq.mean = mean(freq), freq.sd = sd(freq))

ggplot(opfreq, aes(x = operator, y = freq.mean, group = group, fill= group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  coord_flip() +
  facet_grid(on_task ~ ., scales = "free", switch = "both", as.table = FALSE, labeller = labeller(on_task = c("TRUE" = "Task operators", "FALSE" = "Mind-wandering operators"))) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=freq.mean-freq.sd, ymax=freq.mean+freq.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Operator", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  fillScale



## Conditional probabilities of operators

# What is the chance of selecting any given operator at the start of a trial?

first_ops <- opdatall %>%
  group_by(group, participant, trial) %>%
  slice(1) %>%
  group_by(group, participant, on_task, operator) %>%
  tally() %>%
  group_by(group, participant) %>%
  mutate(p = n/sum(n)) %>%
  group_by(group, on_task, operator) %>%
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



## What is the chance of selecting an on-task/off-task operator given the current operator?

## Transition probabilities
calc.trans.probs <- function(dat, variable) {
  
  dat <- droplevels(dat)
  
  vals <- sort(unique(dat[[variable]]))
  
  p <- matrix(nrow = length(vals), ncol = length(vals), 0)
  row.names(p) <- vals
  colnames(p) <- vals
  
  for (i in 1:(nrow(dat) - 1)) {
    # Go over items in sets of two
    # If they're from the same participant, count the transition
    if (dat$participant[i] == dat$participant[i+1] & dat$group[i] == dat$group[i+1]) {
      p[dat[[variable]][i], dat[[variable]][i+1]] <- p[dat[[variable]][i], dat[[variable]][i+1]] + 1
    }
  }
  
  # Convert counts to probability
  p <- p / rowSums(p)

  return(p)
}

plot.trans.probs.ops <- function(p, style = "circular") {
  # Plot transition probabilities between operators
  
  on_task = which(!str_detect(labels(p)[[1]], "wander|memory"))
  wandering = which(str_detect(labels(p)[[1]], "wander|memory"))
  groups = list("on task" = on_task, "wandering" = wandering)
  
  p[round(p,2) == 0] = NA # Remove zero-probabilities from plot
  
  qgraph(
    p, # transition probabilities, rounded to two decimal places
    groups = groups,
    layout = style,
    layout.control = 0.9,
    vsize = 5,
    esize = 5,
    #mar = c(8,7,8,7), # Margin size (bottom, left, top, right)
    labels = labels(p)[[1]],
    label.scale = FALSE,
    curveAll = TRUE,
    curveDefault = 1.05,
    edge.labels = TRUE, # Show numbers on arrows
    edge.label.cex = 1, # Size of edge labels
    border.width = 2, # Width of the borders of the nodes
    colFactor = 0.15, # controls degree of transparancy
    asize = 2, # Size of arrow heads
    maximum = 0.2 # Maximum transition probability, used for scaling
    #filetype = "pdf", # set to "pdf" or "tex" for standalone files
    #filename = paste0(gsub(" ", "_", graph_title))
  )
}



opdatcontrol <- opdatall %>%
  mutate(on_task = if_else(on_task, "on task", "wandering")) %>%
  filter(group == "control model", success == TRUE)

opdatdepr <- opdatall %>%
  mutate(on_task = if_else(on_task, "on task", "wandering")) %>%
  filter(group =="depressed model", success == TRUE) 

calc.trans.probs(opdatcontrol, "on_task") %>%
  plot.trans.probs.ops()

calc.trans.probs(opdatcontrol, "operator") %>%
  plot.trans.probs.ops(style = "spring")


calc.trans.probs(opdatdepr, "on_task") %>%
  plot.trans.probs.ops()

calc.trans.probs(opdatdepr, "operator") %>%
  plot.trans.probs.ops()




########################
###  RETRIEVAL DATA  ###
########################



mem_files <- c()
mem_files[1] <- tail(list.files(path = file_dir_control, pattern="mems.csv", full.names = TRUE),1)
mem_files[2] <- tail(list.files(path = file_dir_depressed, pattern="mems.csv", full.names = TRUE),1)

memdatfull <- data.frame()
for (i in 1:length(mem_files)) {
  memdatfull <- rbind(memdatfull, read.csv(mem_files[i], header=TRUE,sep=","))
}

memdat <- memdatfull %>%
  select(-task_rep)


## For now let's label one model as the control model and the other as the depressed model
## NOTE: they are actually two different iterations of the same model 

memdat <- memdat %>%
  mutate(group = ifelse(model == levels(model)[1], "control model", "depressed model")) %>%
  select(-model)



mems <- memdat %>% 
  filter(type == "memory") %>%
  droplevels()

get.valence <- function(memory) {
  return(gsub("\\d", "", memory))
}

mems$valence <- sapply(mems$retrieval_item, get.valence)



memfreq <- mems %>%
  group_by(group, participant, valence) %>% 
  tally() %>%
  mutate(freq = n/sum(n)) %>%
  group_by(group, valence) %>%
  summarise(freq.mean = mean(freq), freq.sd = sd(freq))


if (use_tikz) {
  tikz(file = paste0(fig_path, "2backMemFreq.tex"), width = 6, height = 3)
}

plot <- ggplot(memfreq, aes(x = valence, y = freq.mean, fill= group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=freq.mean-freq.sd, ymax=freq.mean+freq.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Valence", y = "Frequency") +
  fillScale

print(plot)
if (use_tikz) {
  dev.off()
}


plot.trans.probs.mems <- function(p, style = "circular") {
  # Plot transition probabilities between memories
  
  p[round(p,2) == 0] = NA # Remove zero-probabilities from plot
  
  qgraph(
    p, # transition probabilities, rounded to two decimal places
    layout = style,
    layout.control = 0.9,
    vsize = 10,
    esize = 10,
    mar = c(8,7,8,7), # Margin size (bottom, left, top, right)
    labels = labels(p)[[1]],
    label.scale = FALSE,
    curveAll = TRUE,
    curveDefault = 1.05,
    edge.labels = TRUE, # Show numbers on arrows
    edge.label.cex = 1, # Size of edge labels
    border.width = 2, # Width of the borders of the nodes
    colFactor = 0.15, # controls degree of transparancy
    asize = 2, # Size of arrow heads
    maximum = 0.2 # Maximum transition probability, used for scaling
    #filetype = "pdf", # set to "pdf" or "tex" for standalone files
    #filename = paste0(gsub(" ", "_", graph_title))
  )
}


mems %>% filter(group == "control model") %>%
  calc.trans.probs("valence") %>%
  plot.trans.probs.mems()

mems %>% filter(group == "depressed model") %>%
  calc.trans.probs("valence") %>%
  plot.trans.probs.mems()















########################
###  SUMMARY REPORT  ###
########################


### Summarise the important metrics of comparison
accdat %>% group_by(type) %>% summarise(mean(acc.mean), sd(acc.mean))
respdat %>% group_by(type) %>% summarise(mean(rr.mean), sd(rr.mean))
rtdat %>% group_by(type) %>% summarise(mean(rt.mean), sd(rt.mean))

auto_responses

mw_share_of_ops
mw_dominant
mw_train_length
mw_train_duration
attention_span_length






### Summary plots

## response rate

respdat.summarised <- respdat.all %>%
  group_by(type) %>%
  summarise(rr = mean(rr.mean), rr.sd = sd(rr.mean)) %>%
  mutate(group = c("control", "control", "depressed", "depressed"))

if (use_tikz) {
  tikz(file = paste0(fig_path, "2backRRsummary.tex"), width = 6, height = 3)
}

plot <- ggplot(respdat.summarised, aes(x = group, y = rr, group_by(group), fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=rr-rr.sd, ymax=rr+rr.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "", y = "Response rate") +
  fillScale + 
  guides(fill=FALSE)

print(plot)
if (use_tikz) {
  dev.off()
}

## accuracy

accdat.summarised <- acc.by.condition.all %>%
  group_by(type) %>%
  summarise(acc = mean(acc.mean), acc.sd = sd(acc.sd)) %>%
  mutate(group = c("control", "control", "depressed", "depressed"))


if (use_tikz) {
  tikz(file = paste0(fig_path, "2backAccsummary.tex"), width = 6, height = 3)
}

ggplot(accdat.summarised, aes(x = group, y = acc, group_by(group), fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous() +
  geom_errorbar(aes(ymin=acc-acc.sd, ymax=acc+acc.sd), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "", y = "Accuracy") +
  fillScale + 
  guides(fill=FALSE)

print(plot)
if (use_tikz) {
  dev.off()
}

