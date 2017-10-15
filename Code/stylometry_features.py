import json
import csv


def savefile(sourcefile, metadata = False):
	# given source file with reddit comment data, return path to CSV file with stylometry features
	return savefile + "_metadata.csv" if metadata else savefile + "_features.csv"


def process(sourcefile, term=None):

	#make metadata save file CSVwriter
	metafile = open(savefile(sourcefile, metadata = True), 'w')
	metacolumns = ['id', 'subreddit_id', 'author', 'created_utc', 'retrieved_on', 'parent_id', 'score', 'ups', 
	'downs', 'controversiality', 'gilded', 'edited'] 
	mwriter = csv.DictWriter(metafile, fieldnames = metacolumns)
	mwriter.writeheader()

	#word frequencies
	featfile= open(savefile(sourcefile, metadata = False), 'w')
	fwriter = csv.writer(featfile)
	featcolumns = 'length_char,length_words' + ',yules_k,lego_1,lego_2,lego_3,lego_4,lego_5,lego_6,lego_7,lego_8,lego_9,lego_10' + 
	',all_upper,all_lower,first_upper,camel,other_case' + ',word_1,word_2,word_3,word_4,word_5,word_6,word_7,word_8,word_9,word_10' +
	',word_11,word_12,word_13,word_14,word_15,word_16,word_17,word_18,word_19,word_20' + 
	',a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z' +
	',0,1,2,3,4,5,6,7,8,9' + ',.,?,!,\,,;,:,(,),",-,\'' +


	fwriter.writeheader()

	#read sourcefile in line by line, find tweet location, clean, and save in appropriate file
	with open(sourcefile, 'r') as f:
		include_all_tweets = False if term is not None else True
		for line in f:
			comment = line.split('\n')[0] 
			comment = json.loads(comment)
			mwriter.writerow({key: value for key, value in comment.items() if value is in metacolumns})
		
			#update count of words
			#It takes way too much processing power to continually update wordcnts, 
			#so instead save the text to a temporary file and update after 1000 tweets
			i += 1
			words = twok.tokenize(text)
			words =  ' '.join(set(words)) #so each instance of a word corresponds to one tweet
			temp.write('{} '.format(words)) 
			if i % chunk is 0:
				temp.seek(0)
				words = temp.read()
				words = twok.tokenize(words)
				wordcnt.update(words)
				temp.seek(0)
				temp.truncate()

	temp.seek(0) #get last few words
	words = temp.read()
	words = twok.tokenize(words)
	wordcnt.update(words)
	temp.close()
	
	counts = pd.DataFrame(list(wordcnt.items()), columns=["word", "count"])
	counts = counts.sort_values("count", ascending=False)
	counts.to_csv(savefile(sourcefile, tweets=False), index=False)
