# Review helpfulness classification

This repository contains the source code of my Master's Dissertation in Business Economics at Ghent University in the year 2016-17. It tackled the automatic classification/ranking of Amazon.com reviews. I received a 16 out of 20 on my thesis project. 
My work started from the original source code and data of Michel Ballings, Ricky Yue and Matthijs Meire. All code however was entirely restructured and rewritten, with an exception for the python code.

I am always available for questions or remarks with regards to this project.

The structure of this repository is as follows:
* All `.R` files in the top level are essential the the execution of the full pipeline
 * `installing_packages.R` contains an overview of all packages that need to be installed + how
 * `parse_nrc_lexicon.R` takes care of parsing the original NRC emotion lexicon in a more usable format. This will be needed in `data_prep.R`
 * `data_prep.R` takes care of transforming the original input data files into a set of features. Most of the logic is put in functions in `data_prep_functions.R`.
 * `reporting_performance.R` takes care of using the features to train different classifiers, and evaluating their performance. All plotting is also done here. The help functions used in this file are defined in `model_functions.R`
* In the folder `data`  all relevant input and intermediate files are stored. The only exception are the principal components of the document-term matrices, as these would be too large to allow easy handling of this repository. These can be created however by executing `data_prep.R` first.
* Also `list` is used as input for creating the features. The files in here however are more auxiliary information such as word lists.
* In `python` the input files, output files + code for using TextBlob (sentiment analysis) are saved. 
* `results` contain some numerical results for the classsification. These are presented in the tables of the thesis book
* `old` contains all code that was used for experimentation, tweaking, etc. None of the files in this folder were in the end used to obtain the results. It for example contains the scraping scripts who unfortunately do not work any more.

All further information can be found in the code comments and thesis book (contact me in private for receiving a copy).
