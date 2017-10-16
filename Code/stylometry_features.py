import json
import csv
from nltk.tokenize import RegexpTokenizer, word_tokenize
from string import ascii_lowercase, digits, punctuation
from string_utils import is_camel_case
from numpy import arange
import bz2
from collections import Counter
from re import escape

def savefile(sourcefile, metadata = False):
	# given source file with reddit comment data, return path to CSV file with stylometry features
	return sourcefile.replace(".bz2", "_metadata.csv") if metadata else sourcefile.replace(".bz2", "_features.csv")


def extract_metadata(sourcefile):
	#make metadata save file CSVwriter
	metafile = open(savefile(sourcefile, metadata = True), 'w')
	metacolumns = ['id', 'subreddit_id', 'subreddit', 'author', 'created_utc', 'retrieved_on', 'parent_id', 'score', 'ups', 'downs', 'controversiality', 'gilded', 'edited'] 
	mwriter = csv.DictWriter(metafile, fieldnames = metacolumns)
	mwriter.writeheader()
	with bz2.open(sourcefile, 'rt') as f:
		for line in f:
			comment = line.split('\n')[0] 
			comment = json.loads(comment)
			mwriter.writerow({key: value for key, value in comment.items() if key in metacolumns})


def extract_text_features(sourcefile):
	#pull out features as used by Narayanan et al
	function_words = open("../Data/function_words.txt", 'r').read().split('\n')
	chars = list(ascii_lowercase)
	digs = list(digits)
	punct = list(punctuation)
	othercols = ['id', 'subreddit_id', 'author', 'length_char', 'length_words', 'yules_k', 
		'lego_1', 'lego_2', 'lego_3', 'lego_4', 'lego_5', 'lego_6', 'lego_7', 'lego_8', 'lego_9', 'lego_10', 
		'all_upper', 'all_lower', 'first_upper', 'camel', 'other_case', 'word_1', 'word_2', 'word_3', 'word_4', 'word_5', 'word_6', 'word_7', 'word_8', 'word_9', 'word_10',
		'word_11', 'word_12', 'word_13', 'word_14', 'word_15', 'word_16', 'word_17', 'word_18', 'word_19', 'word_20'] 

	#prepare CSV file to save features to
	featfile= open(savefile(sourcefile, metadata = False), 'w')
	fwriter = csv.DictWriter(featfile, fieldnames = othercols + function_words + chars + digs + punct)
	fwriter.writeheader()

	#use tokenizer to get # of characters in each comment
	pregex = '[a-z0-9{}]'.format(escape(punctuation))
	tokenizer = RegexpTokenizer(pregex)

	with bz2.open(sourcefile, 'rt') as f: #line-by-line for RAM purposes
		for line in f:
			comment = line.split('\n')[0]  #massage comment into useful forms
			comment = json.loads(comment)
			words = word_tokenize(comment['body'])
			words = [word for word in words if word.isalnum()]
			lower_words = [word.lower() for word in words]

			cntr = Counter() #count character-level tokens and function words
			cntr.update(tokenizer.tokenize(comment['body'].lower()))
			fwords = [word for word in lower_words if word in function_words]
			cntr.update(fwords)

			lencnt = Counter() #count how many words of each length
			lencnt.update([len(word) for word in words])
			lencnt = {"word_{}".format(key): value for key, value in lencnt.items() if key < 21}

			otherfeat = { #other features that are easily wrapped in single-line operations
			"length_char": len(comment['body']),
			"length_words": len(words),
			"all_lower": sum(map(lambda x: x.islower(), words)),
			"all_upper": sum(map(lambda x: x.isupper(), words)),
			"first_upper": sum(map(lambda x: x.istitle(), words)),
			"camel": sum(map(lambda x: is_camel_case(x), words)),
			'id': comment['id'],
			'author': comment['author'],
			'subreddit_id': comment['subreddit_id']
			}
			otherfeat["other_case"] = otherfeat["length_words"] - otherfeat["all_upper"] - otherfeat["all_lower"] - otherfeat["camel"]

			timescnt = Counter() #vocabulary richness features
			timescnt.update(lower_words)
			legocnt = Counter() 
			legocnt.update(timescnt.values())
			legocnt = {'lego_{}'.format(key): value for key, value in legocnt.items()}
			otherfeat["yules_k"] = 10**4 * (-1/len(words) + 
				sum(
					map(
						lambda m: legocnt['lego_{}'.format(m)] * (m/len(words))**2 if 'lego_{}'.format(m) in legocnt else 0, 
						arange(max(timescnt.values())) + 1))) if len(words) > 0 else 0
			legocnt = {key: value for key, value in legocnt.items() if int(key.split('_')[1]) < 11}

			fwriter.writerow({**cntr, **lencnt, **otherfeat, **legocnt})