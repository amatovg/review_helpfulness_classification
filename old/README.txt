This s a README file for the model.R script. 
Also check data_prep.R to see how data are prepared.
Check scraping.R and reviewer.R to see how some data were scraped from Amazon website and cleaned.  


1. Dependent Variables

Three dependent variables are used, including:

1.1 binary variable: helpful vote or not;
1.2 helpfulness ratio: (# of helpful votes)/(# of votes);
1.3 transformation of helpfulness ratio: log((# of helpful votes)^2/(# of votes)+1).

2. Categories of Predictors

In the script, all predictors used are described and categorized as 

2.1 reviewer info; 
2.1.1 reviewer_rank: rank of reviewer among all Amazon reviewers
2.1.2 helpful_vote: total helpful votes for the reviewer on Amazon
2.1.3 vote: total votes for all the reviews the reviewer has written on Amazon
2.1.4 top_reviewer: whether the reviewer is any of TOP1, TOP10,TOP50,TOP500,TOP1000 ranked reviewers or 
		    Vine Voice(check https://www.amazon.com/gp/vine/help for more detailes) 

2.2 review subjectivity; 

Predictors are calcualted based the Python package TextBlob.
(See http://textblob.readthedocs.org/en/dev/quickstart.html for detailes)

2.2.1 subjectivity: subjectivitiy of the review within the range[0,1], where 0 is very objective and 1 is very subjective
2.2.2 sentiment: sentiment of the review within the range of [-1,1], where -1 is very negative and 1 is very positive


2.3 bag of words; 
2.3.1 svd: The first 25 vectors from the unitary matrix U after applying Singular Value Decomposition(SVD) on the Document Term Matrix

Using binary variable(see 1.1) as dependent variable and vectors from matrix U as predictors,
with all 3953 vectors for U, AUC(Area Under Curve) = 0.721;
with the first 1000 vectors, AUC = 0.671;
with the first 500 vectors, AUC = 0.693;
with the first 250 vectors, AUC = 0.712;
with the first 100 vectors, AUC = 0.722;
with the first 50 vectors, AUC = 0.724;
with the first 25 vectors, AUC = 0.732;
with the first 10 vectors, AUC = 0.714.

Based on comparison of AUC above, the first 25 vectors from SVD are used as bag of words predictors. 


2.4 review framing;
2.4.1 num_of_words: lenght of a review by word.

 
2.5 review readability;

To develop metrics that measure how literally readably a review is, references below are used:
1) W.H. DuBay, The Principles of Readability, Impact Information, http://www.nald.ca/library/research/readab/readab.pdf, 2004
2) A. Ghose and P. G. Ipeirotis, "Estimating the Helpfulness and Economic Impact of Product Reviews: Mining Text and Reviewer Characteristics," 
   in IEEE Transactions on Knowledge and Data Engineering, vol. 23, no. 10, pp. 1498-1512, Oct. 2011 

2.5.1 FOG: Gunnin fog index
2.5.2 FK: Flesch-Kincard Grade Level
2.5.3 ARI: Automated Readability Index
2.5.4 CLI : Coleman-Liau Index


2.6 review recency; 
2.6.1 period: period since a review posted until the latest review from the dataset posted


2.7 reviewer engagement; 

The two predictors below are based on number of helpful votes and number of votes the reviewer received in the data set(all
about Logitech Mobility products). The helpful_vote(2.1.2) and vote(2.1.3) as mentioned above are from reviewer's profile 
webpage where a reviewer get votes based on all his/her reviews written on Amazon.com, not only from Logitech products. 

2.7.1 prev_helpful:  (number of previous helpful votes for the reviewer)^2/(number of previous votes for the reviewer)
2.7.2 num_prev_vote: number of previous votes for the reviewer


2.8 review extremity. 

2.8.1 avg_star: average star rating of the product
2.8.2 star: star rating by the reviewer  
2.8.3 star_extremity: (star - avg_star)/avg_star
2.8.4 extreme_review: whether star rating belongs to c(1,5) or not


3. Algorithms
Algorithms including Random Forest, SVM, xgboost, Naive Bayes and Generalized Linear Model(GLM)
are used. All predictos listed above used for prediction. Models are evaluated by AUC and RMSE. 
Those results are shown as comments in the R script.


4. Data Source

There are a total 3952 records for modeling. Some variables are from the original Logitech dataset,
some are through web scripting and some are metrics calculated based on existed variables. 

reviewer.csv             
top_reviewer.csv         
subj_semt.csv            
svd_top25.csv
readblty.csv
data_pm.csv
reviewer_product.csv
avg_star.csv


5. Libraries Required

5.1 For modeling
library(randomForest)           
library(e1071)                  ## has Naive Nayes and SVM
library(xgboost)                

5.2 For model evaluation
library(AUC)
library(Metrics)



6. Summary of Predictive Results

With all predictors used and data split by 50/50 for training/test, xgboost 
gives the best AUC of 0.83 on the test data for the binary prediction, and 
Random Forest gives the minimum RMSE of 0.31 for the helpful ratio
prediction on the test data.



