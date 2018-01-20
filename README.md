# **Emotional n-back**

This repository contains the code for the models and analyses in my master's thesis: **A Computational Cognitive Model of Rumination**.


## Table of contents
- [Getting started](#getting-started)
    - [PRIMs](#prims)
    - [Useful links](#useful-links)
- [Models](#models)
    - [Running a single model](#running-a-single-model)
    - [Doing multiple model runs at once](#doing-multiple-model-runs-at-once)
    - [Saving and loading model states](#saving-and-loading-model-states)
- [Data](#data)
    - [Converting `.fulltrace` to `.csv`](#converting-fulltrace-to-csv)
- [Analysis](#analysis)
    - [Notebooks](#notebooks)
- [Read more](#read-more)


## Getting started

#### PRIMs
The models require a customised version of Niels Taatgen's PRIMs cognitive architecture.
This application only works with macOS.
Download the file [`PRIMs.app.zip`](PRIMs.app.zip), extract it, and run the application.

#### Useful links
- [PRIMs tutorial](https://github.com/ntaatgen/PRIMs-Tutorial)
- [Source code for the original PRIMs](https://github.com/ntaatgen/ACTransfer)
- [Source code for my modified version of PRIMs](https://github.com/maartenvandervelde/ACTransfer/tree/Maarten/)


## Models
Files related to running models are provided in the [`models`](models) folder.

#### Running a single model
Files with a `.prims` extension contain the actual models.
You can load a model by selecting `Load Model` from the PRIMs menu bar.
Use the `Step` button on the menu bar to step through the model.


#### Doing multiple model runs at once
Files with a `.bprims` extension can be used to perform a batch run (simulating more than one participant).
Some examples are provided.

To start a batch run, select `Run`, then `Run batch` from the PRIMs menu, and select the desired `.bprims` file.

The `Run` menu also contains a `Run batch with trace` option, which works the same, but will also save the entire output trace to a `.fulltrace` file.
Use this option when you want to analyse the model's output with the provided script.
(Note that saving the full output trace can take quite some time!)


#### Saving and loading model states
To save time, you can train a model once and store its state in a `.brain` file.
Use the menu option `Save image` to save the current state of a model and `Load image` to reinstate a previous model state.


## Data
The output of the models that are discussed in the thesis is provided in the [`data`](data) folder.

Each `.fulltrace` file contains the full output trace of a single batch run, as produced by PRIMs.

#### Converting `.fulltrace` to `.csv`
Before the output can be analysed, it has to be converted into a sensible format using the python parse script.
To convert a `.fulltrace` file in the `0back/depressed` folder, run the following command in the terminal:
```
python parse_fulltrace_0back.py 0back/depressed
```

Similarly, a `.fulltrace` file from a 2-back run in the `2back/control` folder is parsed by running:
```
python parse_fulltrace_2back.py 2back/control
```

In both cases the script will produce a set of `.csv` files in the specified folder:
- `*_beh.csv` : behavioural results
- `*_mems.csv` : list of retrievals from declarative memory
- `*_ops.csv` : list of operator executions


## Analysis

The [`analyis`](analysis) directory contains separate `R` scripts for analysing 0-back and 2-back model runs.
These scripts reproduce the accuracy, response rate, and (z-transformed) response time plots shown in the thesis, along with the presented statistical tests, and additional analyses of operator selection.

#### Notebooks
- [0-back analysis notebook](http://htmlpreview.github.io/?https://github.com/maartenvandervelde/emotional-n-back/blob/master/analysis/0back_model_analysis.nb.html)
- [2-back analysis notebook](http://htmlpreview.github.io/?https://github.com/maartenvandervelde/emotional-n-back/blob/master/analysis/2back_model_analysis.nb.html)




## Read more

- [PRIMs homepage](http://www.ai.rug.nl/~niels/actransfer.html)
