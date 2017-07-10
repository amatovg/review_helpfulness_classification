import glob
import nltk
import textblob
from textblob import TextBlob

mypath="/home/yyf/review/*.txt"
s=[]

for i in glob.glob(mypath):
    fil=open(i).readlines()[1]
    fil=unicode(fil, errors='ignore')
    try:
    	testimonial=TextBlob(fil)
   	print testimonial
    subj=testimonial.sentiment.subjectivity
	semt=testimonial.sentiment.polarity
        id=int(str(i)[17:-4])
    	s.append([id,subj,semt])
   	print subj
    except:
	print id

import csv
csvfile="/home/yyf/review/review.csv"

with open(csvfile,'w') as output:
    writer=csv.writer(output,lineterminator='\n')
    for val in s:
    	writer.writerow([val])
         	
