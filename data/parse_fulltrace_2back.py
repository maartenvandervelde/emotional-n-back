#!/usr/bin/env python

import sys
import random
import argparse
import glob
import csv
import re


task_operators = [
    "process-face",
    "recognise-face",
    "no-one-back",
    "retrieve-one-back",
    "failed-to-retrieve-one-back",
    "retrieve-two-back",
    "no-two-back",
    "failed-to-retrieve-two-back",
    "two-back-match-press-same",
    "two-back-no-match-press-diff",
	"update-one-back-cue",
	"rehearse-one-back",
	"rehearse-two-back",
	"focus-on-fixation",
	"focus-on-task"
]

def read_files_from_directory(path):
	files = []
	files.extend(glob.glob1(path, '*.fulltrace'))
	return files


def write_to_csv(data, path):
	with open(path, "wb") as f:
		writer = csv.writer(f, delimiter = ",")
		writer.writerows(data)

		
def main():
	
	# Generate a list of fulltrace files in the specified directory
	files = read_files_from_directory(filedir)
	
	if (len(files) > 0):
		print("Converting {} fulltrace file(s) in directory '{}'...".format(len(files), filedir))
	else:
		print("There are no fulltrace files in directory '{}', or the directory does not exist.".format(filedir))
		return
		
	# Parse one file at a time
	
	for file in files:
	
		model = file.replace(".fulltrace", "")
		
		
		## Behavioural
		participant_num = 0
		task_name = ""
		task_rep_num = 0
		block_num = 0
		trial_num = 0
		trial_stimulus = ""
		trial_response = ""
		trial_start_time = 0.0
		trial_rt = 0.0
		
		behavioural_data = [["model", "participant", "task", "task_rep", "block", "trial", "stimulus", "response", "start_time", "rt"]]
		
		## Operators
		operator = ""
		executing_operator = False
		operator_start_time = 0.0
		operator_exec_time = 0.0
		operator_on_task = True
		operator_success = True
		
		operator_data = [["model", "participant", "task", "task_rep", "block", "trial", "stimulus", "operator", "start_time", "exec_time", "on_task", "success"]]		
		
		
		## Retrievals
		retrieval_item = ""
		retrieval_type = ""
		retrieval_start = 0.0
		retrieval_latency = 0.0
		
		retrieval_data = [["model", "participant", "task", "task_rep", "block", "trial", "stimulus", "retrieval_item", "type", "retrieval_start", "latency"]]
		
				
		path = filedir + '/' + file
		
		with open(path, 'r') as f:
			for line in f:

				split = line.split()
				
				# Skip empty lines
				if not split:
					continue
				
				
				### Behavioural data
				
				# New participant run
				if line.startswith("Run #"):
					participant_num = int(re.search("[0-9]", line).group(0))
					
				# Start of a new task (model) 
				elif line.startswith("Running task"):
					task_name = split[2]
					task_rep_num = 0
				
				
				# Mew repetition of the task
				elif line.startswith("Setting up"):
					task_rep_num = task_rep_num + 1
					trial_num = 0
				
				# New block
				elif line.startswith("Starting block"):
					block_num = split[2]
					
				# New trial
				elif line.startswith("Trial"):
					trial_num = trial_num + 1
					trial_stimulus = split[3]
					trial_start_time = float(split[5])
					
					
				# Key press (same or diff)
				elif "Pressing" in line and "start" not in line:
					trial_response = split[2]
					times = [float(i) for i in re.findall("\d+\.\d+", line)] # [start time of action, latency]
					trial_rt = sum(times) - trial_start_time
					behavioural_data.append([model, participant_num, task_name, task_rep_num, block_num, trial_num, trial_stimulus, trial_response, trial_start_time, trial_rt])
					
				# No response at all
				elif line.startswith("Action: none"):
					trial_response = "none"
					trial_rt = 0.0
					behavioural_data.append([model, participant_num, task_name, task_rep_num, block_num, trial_num, trial_stimulus, trial_response, trial_start_time, trial_rt])
					
					
				
				### Operator firing
				
				# Operator selected for execution
				elif "Retrieved operator" in line:
					if executing_operator:
						operator_data.append([model, participant_num, task_name, task_rep_num, block_num, trial_num, trial_stimulus, operator, operator_start_time, operator_exec_time, operator_on_task, operator_success])
						executing_operator = False
						operator_start_time = 0.0
						operator_exec_time = 0.0
						operator_on_task = True
						operator_success = True
					operator = split[4]
					executing_operator = True
					operator_start_time = float(split[0])
					operator_on_task = operator in task_operators
					
				# Executing (part of) an operator (always takes 50 ms)
				elif "Firing" in line:
					operator_exec_time = operator_exec_time + 0.050
									
				# Compiling or reinforcing (part of) an operator (always takes 300 ms, but happens at same time as firing)
				elif "Compiling" in line or "Reinforcing" in line:
					operator_exec_time = operator_exec_time + 0.250
				
				# Current operator has failed
				elif "failed" in line:
					operator_success = False
					operator_data.append([model, participant_num, task_name, task_rep_num, block_num, trial_num, trial_stimulus, operator, operator_start_time, operator_exec_time, operator_on_task, operator_success])
					executing_operator = False
					operator_start_time = 0.0
					operator_exec_time = 0.0
					operator_on_task = True
					operator_success = True
				
				# Operator has finished
				elif executing_operator:
					operator_data.append([model, participant_num, task_name, task_rep_num, block_num, trial_num, trial_stimulus, operator, operator_start_time, operator_exec_time, operator_on_task, operator_success])
					executing_operator = False
					operator_start_time = 0.0
					operator_exec_time = 0.0
					operator_on_task = True
					operator_success = True

				
				
				### Retrievals
				
				# Retrieved facial expression
				if re.search("\sface\d+", line):
					retrieval_start = split[0]
					retrieval_item = split[1]
					retrieval_latency = [float(i) for i in re.findall("\d+\.\d+", line)][1]
					retrieval_type = "face"
					retrieval_data.append([model, participant_num, task_name, task_rep_num, block_num, trial_num, trial_stimulus, retrieval_item, retrieval_type, retrieval_start, retrieval_latency])

				
				# Retrieved n-back item
				if "imaginalN" in line and not "New" in line:
					retrieval_start = split[0]
					retrieval_item = split[1]
					retrieval_latency = [float(i) for i in re.findall("\d+\.\d+", line)][1]
					retrieval_type = "nback"
					retrieval_data.append([model, participant_num, task_name, task_rep_num, block_num, trial_num, trial_stimulus, retrieval_item, retrieval_type, retrieval_start, retrieval_latency])


				# Retrieved random memory
				if re.search("\smemory", line):
					retrieval_start = split[0]
					retrieval_item = split[1].replace("memory", "")
					retrieval_latency = [float(i) for i in re.findall("\d+\.\d+", line)][1]
					retrieval_type = "memory"
					retrieval_data.append([model, participant_num, task_name, task_rep_num, block_num, trial_num, trial_stimulus, retrieval_item, retrieval_type, retrieval_start, retrieval_latency])
					


					
		write_to_csv(behavioural_data, path.replace(".fulltrace", "_beh.csv"))			
		write_to_csv(operator_data, path.replace(".fulltrace", "_ops.csv"))			
		write_to_csv(retrieval_data, path.replace(".fulltrace", "_mems.csv"))			
		


		
		
if __name__ == '__main__':
	# Argument parsing (also enables "python parse_fulltrace.py -h" to explain usage)
    parser = argparse.ArgumentParser(description="Tool that transforms .fulltrace files produced by batch runs of the emotional 2-back PRIMs model into usable data files.")
    parser.add_argument("directory", metavar = "dir", nargs = 1, help="directory containing the fulltrace files")
    args = parser.parse_args()

    filedir = args.directory[0]    # Directory containing fulltrace files
    
    main()
