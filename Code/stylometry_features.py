import json
import csv
from nltk.tokenize import RegexpTokenizer, word_tokenize
from string import ascii_lowercase, digits, punctuation
from string_utils import is_camel_case
from numpy import arange
import bz2
from collections import Counter
from re import escape
import sys

#create path to CSV files, with different names depending on if it is for metadata or not
def savefile(sourcefile, metadata = False):
#	sourcefile = sourcefile.replace("../Data/", "../../../../work/akz5056/502_Project/Data")
	# given source file with reddit comment data, return path to CSV file with stylometry features
	return sourcefile.replace(".bz2", "_metadata.csv") if metadata else sourcefile.replace(".bz2", "_features.csv")


#pull out useful information that is not in the text and save to a separate CSV file
def extract_metadata(sourcefile, subreddits = None):
	#make metadata save file CSVwriter
	metafile = open(savefile(sourcefile, metadata = True), 'w')
	metacolumns = ['id', 'subreddit_id', 'subreddit', 'author', 'created_utc', 'score', 'gilded', 'edit_time'] 
	mwriter = csv.DictWriter(metafile, fieldnames = metacolumns)
	mwriter.writeheader()
	with bz2.open(sourcefile, 'rt') as f:
		for line in f:
			comment = line.split('\n')[0] 
			comment = json.loads(comment)
			if subreddits is not None:
				if comment['subreddit'].lower() not in subreddits:
					continue
			if comment['author'] in ["[deleted]", "AutoModerator"] or comment['body'] == "[deleted]":
				continue
			row = {key: value for key, value in comment.items() if key in metacolumns}
			row['edit_time'] = comment['edited'] - comment['created_utc'] if comment['edited'] else ''
			if not row:
				continue
			mwriter.writerow(row)

#pull out stylometric text features and save to a separate CSV file
def extract_text_features(sourcefile, subreddits = None):
	#create column names for CSV file
	function_words = set(open("../Data/function_words.txt", 'r').read().split('\n'))
	fw_colnames = list(map(lambda x: "fw_{}".format(x), function_words))
	chars = list(ascii_lowercase)
	digs = list(digits)
	punct = list(punctuation)
	othercols = ['id', 'author', 'subreddit', 'length_char', 'length_words', 'yules_k', 
		'lego_1', 'lego_2', 'lego_3', 'lego_4', 'lego_5', 'lego_6', 'lego_7', 'lego_8', 'lego_9', 'lego_10p', 
		'all_upper', 'all_lower', 'first_upper', 'camel', 'other_case', 'word_1', 'word_2', 'word_3', 'word_4', 'word_5', 'word_6', 'word_7', 'word_8', 'word_9', 'word_10',
		'word_11', 'word_12', 'word_13', 'word_14', 'word_15', 'word_16', 'word_17', 'word_18', 'word_19', 'word_20p'] 

	#prepare CSV file to save features to
	featfile= open(savefile(sourcefile, metadata = False), 'w')
	fwriter = csv.DictWriter(featfile, fieldnames = othercols + fw_colnames + chars + digs + punct)
	fwriter.writeheader()

	#use tokenizer to get # of characters in each comment
	#does not include other languages or characters with hats on them, etc.
	pregex = '[a-z0-9{}]'.format(escape(punctuation))
	tokenizer = RegexpTokenizer(pregex)

	with bz2.open(sourcefile, 'rt') as f: #line-by-line for RAM purposes
		for line in f:
			#massage comment into useful forms
			comment = json.loads(line.split('\n')[0])  
			if subreddits is not None:
				if comment['subreddit'].lower() not in subreddits:
					continue
			if comment['author'] == "[deleted]":
				continue
			words = word_tokenize(comment['body'])
			words = [word for word in words if word.isalnum()] #only include words, not punctuation
			lower_words = [word.lower() for word in words]

			#count character-level tokens and function words
			cntr = Counter() 
			cntr.update(tokenizer.tokenize(comment['body'].lower()))
			fwords = ["fw_{}".format(word) for word in lower_words if word in function_words]
			cntr.update(fwords)
			rowdict = {key: value for key, value in cntr.items()}

			#count how many words of each length (legomena)
			lencnt = Counter() 
			lencnt.update([len(word) for word in words])
			lencnt = {"word_{}".format(key): value for key, value in lencnt.items() if key < 20}
			lencnt['word_20p'] = len(words) - sum([value for key, value in lencnt.items() if int(key.split("_")[1]) < 20])
			rowdict.update(lencnt)

						#other features that are easily wrapped in single-line operations
			otherfeat = { 
			"length_char": len(comment['body']),
			"length_words": len(words),
			"all_lower": sum(map(lambda x: x.islower(), words)),
			"all_upper": sum(map(lambda x: x.isupper(), words)),
			"first_upper": sum(map(lambda x: x.istitle(), words)),
			"camel": sum(map(lambda x: is_camel_case(x), words)),
			'id': comment['id'],
			'author': comment['author'],
			'subreddit': comment['subreddit']
			}
			otherfeat["other_case"] = otherfeat["length_words"] - otherfeat["all_upper"] - otherfeat["all_lower"] - otherfeat["camel"]

						#vocabulary richness features
			timescnt = Counter() 
			timescnt.update(lower_words)
			legocnt = Counter() 
			legocnt.update(timescnt.values())
			legocnt = {'lego_{}'.format(key): value for key, value in legocnt.items()}
			legocnt = {key: value for key, value in legocnt.items() if int(key.split('_')[1]) < 10}
			legocnt['lego_10p'] = len(timescnt.keys()) - sum([value for key, value in legocnt.items() if int(key.split('_')[1]) < 10])
			rowdict.update(legocnt)

			otherfeat["yules_k"] = 10**4 * (-1/len(words) + 
				sum(
					map(
						lambda m: legocnt['lego_{}'.format(m)] * (m/len(words))**2 if 'lego_{}'.format(m) in legocnt else 0, 
						arange(max(timescnt.values())) + 1))) if len(words) > 0 else 0
			rowdict.update(otherfeat)
			#write row to CSV file
			# fwriter.writerow({**cntr, **lencnt, **otherfeat, **legocnt}) #for python 3.5 and above
			fwriter.writerow(rowdict)


if __name__ == '__main__':
	subreddits = sys.argv[2:] if len(sys.argv) > 2 else None
	extract_metadata(sys.argv[1], subreddits)
	extract_text_features(sys.argv[1], subreddits)