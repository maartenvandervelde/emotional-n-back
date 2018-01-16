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

use_tikz = FALSE # set to TRUE to save .tex versions of the plots

data_path <- "/Users/maarten/Dropbox (Work)/Masterproject/emotional-n-back/data/0back/"
fig_path <- "/Users/maarten/Dropbox (Work)/Masterproject/emotional-n-back/fig/"


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

# file_dir <- paste0(data_path, "20171011") # location of two sets of model output files
file_dir_1 <- paste0(data_path, "020180116q")
file_dir_2 <- paste0(data_path, "020180116s")


# beh_files <- tail(list.files(path = file_dir, pattern="beh.csv", full.names = TRUE),2)
beh_files <- c()
beh_files[1] <- tail(list.files(path = file_dir_1, pattern="beh.csv", full.names = TRUE),1)
beh_files[2] <- tail(list.files(path = file_dir_2, pattern="beh.csv", full.names = TRUE),1)

behdatfull <- data.frame()
for (i in 1:length(beh_files)) {
  behdatfull <- rbind(behdatfull, read.csv(beh_files[i], header=TRUE,sep=","))
}

behdat <- behdatfull %>%
  select(-task_rep)


## For now let's label one model as the control model and the other as the depressed model

behdat <- behdat %>%
  mutate(type = ifelse(model == levels(model)[1], "control model", "depressed model")) %>%
  select(-model)
  



### Plot the results next to those of Levens and Gotlib

## Accuracy


allbehdat <- behdat
behdat <- behdat %>%
  filter(response != "none") %>% # Filter out non-responses
  filter(rt <= 2.0) %>%   # Responses after 2s are too late
  filter(rt > 0)      # Response times of 0s indicate missed trials



# Filter out trials with extreme RTs (outside 2.5 SDs of each participant's mean RT)
behdat <- behdat %>%
  group_by(type, participant) %>%
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



# ANOVA
# Three-way repeated measures ANOVA.
# Factors: group (depressed/control), emotion (happy, neutral, sad), response (same, different).

summary(aov(accuracy ~ (type * stimulus * response), data = behdat))
# summary(aov(accuracy ~ (type * stimulus * response) + Error(participant/(stimulus*response)), data = behdat)) # RM ANOVA




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


# ANOVA
# Three-way repeated measures ANOVA.
# Factors: group (depressed/control), emotion (happy, neutral, sad), response (same, different).

resp.all <- allbehdat %>%
  mutate(responded = (outcome %in% c("correct", "wrong")))

summary(aov(responded ~ (type * stimulus * response), data = resp.all))
# summary(aov(responded ~ (type * stimulus * response) + Error(participant/(stimulus*response)), data = resp.all)) # RM ANOVA




## Response time

rtdat <- behdat %>%
  filter(outcome == "correct") %>%
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
  select(-rt.mean, -rt.sd) %>%
  filter(outcome == "correct") %>%
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


# ANOVA
# Three-way repeated measures ANOVA.
# Factors: group (depressed/control), emotion (happy, neutral, sad), response (same, different).

zrt.all <- behdat %>%
  select(-rt.mean, -rt.sd) %>%
  filter(outcome == "correct") %>%
  group_by(participant, type) %>%
  mutate(overall.rt = mean(rt)) %>%
  group_by(participant, type, stimulus, expected_response, overall.rt) %>%
  mutate(condition.rt = mean(rt), condition.rt.sd = sd(rt)) %>%
  mutate(z.rt = (condition.rt - overall.rt) / condition.rt.sd)

summary(aov(z.rt ~ (type * stimulus * response), data = zrt.all))
# summary(aov(z.rt ~ (type * stimulus * response) + Error(participant/(stimulus*response)), data = zrt.all)) # RM ANOVA







########################
###  OPERATOR DATA   ###
########################


## How pervasive is mind-wandering?

op_files <- c()
op_files[1] <- tail(list.files(path = file_dir_1, pattern="ops.csv", full.names = TRUE),1)
op_files[2] <- tail(list.files(path = file_dir_2, pattern="ops.csv", full.names = TRUE),1)

opdatfull <- data.frame()
for (i in 1:length(op_files)) {
  opdatfull <- rbind(opdatfull, read.csv(op_files[i], header=TRUE,sep=","))
}

opdat <- opdatfull %>%
  select(-task_rep)


## For now let's label one model as the control model and the other as the depressed model

opdat <- opdat %>%
  mutate(type = ifelse(model == levels(model)[1], "control model", "depressed model")) %>%
  mutate(on_task = as.logical(on_task)) %>%
  select(-model)




# When do we consider a trial to be "on-task"?
# Let's call any trial in which more than a certain percentage of operators are MW operators a mind-wandering trial for now.

mwfreq <- opdat %>%
  group_by(type, participant, block, trial, on_task) %>%
  count(on_task) %>%
  ungroup() %>%
  complete(type, participant, block, trial, on_task, fill = list(n = 0)) %>%
  group_by(type, participant, block, trial) %>%
  mutate(freq = n/sum(n)) %>%
  ungroup() %>%
  filter(on_task == FALSE) %>%
  mutate(freq = if_else(is.na(freq), 0, freq))


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
  tikz(file = paste0(fig_path, "0backAttentionSpanLength.tex"), width = 6, height = 3)
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
  filter(operator %in% c("face-expression-matches-target", "face-expression-does-not-match-target", "process-memory-auto-respond-same", "process-memory-auto-respond-diff")) %>%
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
mem_files[1] <- tail(list.files(path = file_dir_1, pattern="mems.csv", full.names = TRUE),1)
mem_files[2] <- tail(list.files(path = file_dir_2, pattern="mems.csv", full.names = TRUE),1)

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
  tikz(file = paste0(fig_path, "0backMemFreq.tex"), width = 6, height = 3)
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





#### 
#### Old analyses below this point
####
####
####




# ###############################
# ### MIND-WANDERING CONTENTS ###
# ###############################
# 
# mem_files <- tail(list.files(path = file_dir, pattern="mems.csv", full.names = TRUE),2)
# 
# memdatfull <- data.frame()
# for (i in 1:length(mem_files)) {
#   memdatfull <- rbind(memdatfull, read.csv(mem_files[i], header=TRUE,sep=","))
# }
# 
# memdatall <- memdatfull %>%
#   filter(task_rep == 10) %>% # Select only the last run of the "wandering" model
#   select(-task_rep)
# 
# # For now we just select two models and pretend that one is depressed and the other healthy
# memdatall <- memdatall %>%
#   mutate(group = ifelse(model == levels(model)[1], "control model", "depressed model")) %>%
#   select(-model)
# 
# memdat <- memdatall %>%
#   filter(type == "memory") %>% # Select only memory retrievals
#   select(-type) %>%
#   mutate(mood = gsub("\\d+", "", retrieval_item)) # Extract mood from each memory
# 
# # Overall distribution of moods
# mw_mood_freq <- memdat %>%
#   group_by(participant, group, mood) %>%
#   tally() %>%
#   group_by(participant, group) %>%
#   mutate(freq = n/sum(n)) %>%
#   group_by(group, mood) %>%
#   summarise(freq.mean = mean(freq), freq.sd = sd(freq))
# 
# if (use_tikz) {
#   tikz(file = paste0(fig_path, "0backMWmoodDistribution.tex"), width = 6, height = 3)
# }
# 
# plot <- ggplot(mw_mood_freq, aes(x = mood, y = freq.mean, group = group, fill= group)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#   # scale_y_continuous(labels=percent) +
#   geom_errorbar(aes(ymin=freq.mean-freq.sd, ymax=freq.mean+freq.sd), width=0.2, position = position_dodge(width = 0.9)) +
#   labs(x = "Memory mood", y = "Frequency") +
#   fillScale
# 
# print(plot)
# if (use_tikz) {
#   dev.off()
# }
# 
# ## Total number of retrievals
# mw_retrieval_count <- memdat %>%
#   group_by(participant, group) %>%
#   tally() %>%
#   group_by(group) %>%
#   summarise(count.mean = mean(n), count.sd = sd(n))
# 
# 
# if (use_tikz) {
#   tikz(file = paste0(fig_path, "0backNumberOfRetrievals.tex"), width = 6, height = 3)
# }
# 
# plot <- ggplot(mw_retrieval_count, aes(x = group, y = count.mean, fill= group)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#   scale_y_continuous() +
#   geom_errorbar(aes(ymin=count.mean-count.sd, ymax=count.mean+count.sd), width=0.2, position = position_dodge(width = 0.9)) +
#   labs(x = "Model", y = "Number of retrievals") +
#   fillScale + 
#   guides(fill=FALSE)
# 
# print(plot)
# if (use_tikz) {
#   dev.off()
# }
# 
# ## Retrieval latency by mood
# mw_retrieval_latency <- memdat %>%
#   group_by(participant, group, mood) %>%
#   summarise(latency = mean(latency) * 1000) %>%
#   group_by(group, mood) %>%
#   summarise(latency.mean = mean(latency), latency.sd = sd(latency))
# 
# 
# if (use_tikz) {
#   tikz(file = paste0(fig_path, "0backRetrievalLatency.tex"), width = 6, height = 3)
# }
# 
# plot <- ggplot(mw_retrieval_latency, aes(x = mood, y = latency.mean, group = group, fill= group)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#   scale_y_continuous() +
#   geom_errorbar(aes(ymin=latency.mean-latency.sd, ymax=latency.mean+latency.sd), width=0.2, position = position_dodge(width = 0.9)) +
#   labs(x = "Memory mood", y = "Retrieval latency (ms)") +
#   fillScale
# 
# print(plot)
# if (use_tikz) {
#   dev.off()
# }
# 
# ## Retrievals per trial
# mw_retrievals_per_trial <- memdat %>%
#   group_by(participant, group, trial) %>%
#   tally() %>%
#   group_by(participant, group) %>%
#   summarise(count = mean(n)) %>%
#   group_by(group) %>%
#   summarise(count.mean = mean(count), count.sd= sd(count))
# 
# 
# if (use_tikz) {
#   tikz(file = paste0(fig_path, "0backRetrievalCount.tex"), width = 6, height = 3)
# }
# 
# plot <- ggplot(mw_retrievals_per_trial, aes(x = group, y = count.mean, fill= group)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#   scale_y_continuous() +
#   geom_errorbar(aes(ymin=count.mean-count.sd, ymax=count.mean+count.sd), width=0.2, position = position_dodge(width = 0.9)) +
#   labs(x = "Model", y = "Number of retrievals per trial") +
#   fillScale +
#   guides(fill=FALSE)
# 
# print(plot)
# if (use_tikz) {
#   dev.off()
# }
# 
# 
# ## Proportion of retrievals that is off-task
# 
# prop_offtask <- memdatall %>%
#   group_by(participant, group, type) %>%
#   tally() %>%
#   group_by(participant, group) %>%
#   mutate(freq = n/sum(n)) %>%
#   group_by(group, type) %>%
#   summarise(freq.mean = mean(freq), freq.sd = sd(freq)) %>%
#   filter(type == "memory")
# 
# 
# if (use_tikz) {
#   tikz(file = paste0(fig_path, "0backRetrievalsOffTask.tex"), width = 6, height = 3)
# }
# 
# 
# plot <- ggplot(prop_offtask, aes(x = group, y = freq.mean, fill= group)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#   # scale_y_continuous(labels=percent) +
#   geom_errorbar(aes(ymin=freq.mean-freq.sd, ymax=freq.mean+freq.sd), width=0.2, position = position_dodge(width = 0.9)) +
#   labs(x = "Group", y = "Off-task retrievals") +
#   fillScale +
#   guides(fill = FALSE)
# 
# print(plot)
# if (use_tikz) {
#   dev.off()
# }
#   
# 
# 
# 
# ## Duration of mind-wandering episodes
# 
# # It makes more sense to look at the operator data, since that includings timing of operators firing during MW
# 
# 
# 
# #################
# ### OPERATORS ###
# #################
# 
# op_files <- tail(list.files(path = file_dir, pattern="ops.csv", full.names = TRUE),2)
# 
# opdatfull <- data.frame()
# for (i in 1:length(op_files)) {
#   opdatfull <- rbind(opdatfull, read.csv(op_files[i], header=TRUE,sep=","))
# }
# 
# opdatall <- opdatfull %>%
#   filter(task_rep == 10) %>% # Select only the last run of the "wandering" model
#   select(-task_rep) %>%
#   mutate(on_task = as.logical(on_task), success = as.logical(success))
# 
# # For now we just select two models and label one depressed and the other healthy
# opdatall <- opdatall %>%
#   mutate(group = ifelse(model == levels(model)[1], "control model", "depressed model")) %>%
#   select(-model) %>%
#   mutate(train = 0)
# 
# 
# # Continuous thought trains are unbroken sequences of successful off-task operators
# 
# train <- 0
# in_train <- FALSE
# 
# for (i in 1:(nrow(opdatall) - 1)) {
#   
#   # Go over items in sets of two
#   this_op <- opdatall[i,]
#   next_op <- opdatall[i+1,]
#   if (this_op$participant == next_op$participant && this_op$on_task == FALSE && next_op$on_task == FALSE && this_op$success != FALSE && next_op$success != FALSE) {
#     
#     # The pair of operators constitutes a thought train
#     
#     if (!in_train) {
#       in_train <- TRUE
#       train <- train + 1
#     }
#     
#     opdatall[i,]$train <- train
#     opdatall[i+1,]$train <- train
#     
#   } else {
#     in_train <- FALSE
#   }
# }
# 
# 
# mw_train_length <- opdatall %>%
#   filter(train != 0) %>%
#   group_by(participant, group, train) %>%
#   tally() %>%
#   summarise(length = mean(n)) %>%
#   group_by(group) %>%
#   summarise(length.mean = mean(length), length.sd = sd(length))
# 
# 
# if (use_tikz) {
#   tikz(file = paste0(fig_path, "0backThoughtTrainLength.tex"), width = 6, height = 3)
# }
# 
# plot <- ggplot(mw_train_length, aes(x = group, y = length.mean, fill= group)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#   scale_y_continuous() +
#   geom_errorbar(aes(ymin=length.mean-length.sd, ymax=length.mean+length.sd), width=0.2, position = position_dodge(width = 0.9)) +
#   labs(x = "Group", y = "Thought train length") +
#   fillScale +
#   guides(fill = FALSE)
# 
# print(plot)
# if (use_tikz) {
#   dev.off()
# }
# 
