# -*- coding: utf-8 -*-
'''This script loads pre-trained word embeddings (GloVe embeddings)
into a frozen Keras Embedding layer, and uses it to
train a Author Attribution model on Reddit data.

GloVe embedding data can be found at:
http://nlp.stanford.edu/data/glove.6B.zip
(source page: http://nlp.stanford.edu/projects/glove/)

Reddit data can be found at:
https://www.reddit.com/r/datasets/comments/65o7py/updated_reddit_comment_dataset_as_torrents/

Borrowed code from https://blog.keras.io/using-pre-trained-word-embeddings-in-a-keras-model.html
'''

from __future__ import print_function

import os
#import sys
import numpy as np
import pandas as pd
os.environ['KERAS_BACKEND']='tensorflow'
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
#from keras.utils import to_categorical
from keras.layers import Dense, Input, Flatten, Dropout
from keras.layers import Conv1D, MaxPooling1D, Embedding, Merge
from keras.models import Model
from keras import optimizers
from keras.callbacks import EarlyStopping, ModelCheckpoint
import matplotlib.pyplot as plt
from keras.metrics import sparse_top_k_categorical_accuracy
pd.set_option('display.max_rows', 25)
from keras.utils import plot_model
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import f1_score


np.random.seed(1117)
subset = '50a-250p'
graph = 'CNN_' + subset + '_acc.png'
rank_output = subset + '_preds.csv'
GLOVE_data = 'glove.6B/glove.6B.300d.txt'
TEXT_DATA = 'data_502/RC_' + subset + '_subset.csv'
MAX_SEQUENCE_LENGTH = 500
MAX_NB_WORDS = 40000
EMBEDDING_DIM = 300
VALIDATION_SPLIT = 0.2

def top_5_categorical_accuracy(y_true, y_pred):
    return sparse_top_k_categorical_accuracy(y_true, y_pred, k=5) 


# first, build index mapping words in the embeddings set
# to their embedding vector

print('Indexing word vectors.')

embeddings_index = {}
f = open(os.path.join(GLOVE_data),encoding="utf-8")
for line in f:
    values = line.split()
    word = values[0]
    coefs = np.asarray(values[1:], dtype='float32')
    embeddings_index[word] = coefs
f.close()

print('Found %s word vectors.' % len(embeddings_index))

# second, prepare text samples and their labels
print('Processing text dataset')


#read in  Data and list features
df = pd.read_csv(TEXT_DATA, encoding="latin1")
print(df.n_posts.describe())
print(df.length.describe())
#df = df[df.subreddit == "worldnews"]

print('Found %s unique authors.' % len(df.author.unique()))

texts = df['body']  # list of text samples
labels_index = {}  # dictionary mapping label name to numeric id
labels = df['author']  # list of label ids
for string in labels:
    labels_index.setdefault(string, len(labels_index))
labels =  np.asarray([labels_index[author] for author in labels])


print('Found %s texts.' % len(texts))

texts = texts.astype(str)

# finally, vectorize the text samples into a 2D integer tensor
tokenizer = Tokenizer(num_words=MAX_NB_WORDS)
tokenizer.fit_on_texts(texts)
sequences = tokenizer.texts_to_sequences(texts)

word_index = tokenizer.word_index
print('Found %s unique tokens.' % len(word_index))

data = pad_sequences(sequences, maxlen=MAX_SEQUENCE_LENGTH)

#labels = to_categorical(np.asarray(labels))
print('Shape of data tensor:', data.shape)
print('Shape of label tensor:', labels.shape)

# split the data into a training set and a validation set
indices = np.arange(data.shape[0])
np.random.shuffle(indices)
data = data[indices]
labels = labels[indices]
num_validation_samples = int(VALIDATION_SPLIT * data.shape[0])

x_train = data[:-num_validation_samples]
y_train = labels[:-num_validation_samples]
x_val = data[-num_validation_samples:]
y_val = labels[-num_validation_samples:]

#y_train = y_train.reshape((-1, 1))

print('Preparing embedding matrix.')

# prepare embedding matrix
num_words = min(MAX_NB_WORDS, len(word_index))
embedding_matrix = np.zeros((num_words + 1, EMBEDDING_DIM))
for word, i in word_index.items():
    if i >= MAX_NB_WORDS:
        continue
    embedding_vector = embeddings_index.get(word)
    if embedding_vector is not None:
        # words not found in embedding index will be all-zeros.
        embedding_matrix[i] = embedding_vector

# load pre-trained word embeddings into an Embedding layer
# note that we set trainable = False so as to keep the embeddings fixed
embedding_layer = Embedding(num_words+1,
                            EMBEDDING_DIM,
                            weights=[embedding_matrix],
                            input_length=MAX_SEQUENCE_LENGTH,
                            trainable=False)

print('Training model.')

# train a 1D convnet with global maxpooling
sequence_input = Input(shape=(MAX_SEQUENCE_LENGTH,), dtype='int32')
embedded_sequences = embedding_layer(sequence_input)
# set up layers
filter_sizes = [2,3,4,5]
convs = []

for fsz in filter_sizes:
    l_conv = Conv1D(activation="relu", filters=128, kernel_size=fsz)(embedded_sequences)
    l_pool = MaxPooling1D(5)(l_conv)
    convs.append(l_pool)
    
x = Merge(mode='concat', concat_axis=1)(convs)
l_cov1= Conv1D(128, 5, activation='relu')(x)
l_pool1 = MaxPooling1D(5)(l_cov1)
drop = Dropout(0.1)(l_pool1)
l_cov2 = Conv1D(128, 5, activation='relu')(drop)
l_pool2 = MaxPooling1D(5)(l_cov2)
l_flat = Flatten()(l_pool2)
l_dense = Dense(128, activation='relu')(l_flat)
preds = Dense(len(labels_index), activation='softmax')(l_dense)

model = Model(sequence_input, preds)
model.compile(loss='sparse_categorical_crossentropy',
              optimizer='rmsprop',
              metrics=['acc'])
model.summary()
Wsave = model.get_weights()

#plot_model(model, to_file='CNN_noshape_architecture.png')

optimizers.RMSprop(lr=0.001, rho=0.9, epsilon=1e-08, decay=0.1)

callbacks = [EarlyStopping(monitor='val_acc', patience=5),
             ModelCheckpoint(filepath='weights.hdf5', monitor='val_acc', save_best_only=True)]

#history = model.fit(x_train, y_train, validation_data=(x_val, y_val),
#         epochs=80, verbose=1, batch_size=128, callbacks=callbacks)

#==============================================================================
# define 10-fold cross validation test harness
kfold = StratifiedKFold(n_splits=5, shuffle=False, random_state=19)
cvscores = []
for train, test in kfold.split(data, labels):
    # Fit the model
    model.set_weights(Wsave)
    model.fit(data[train], labels[train], epochs=25, batch_size=10, verbose=0)
    #evaluate the model
    scores = model.evaluate(data[test], labels[test], verbose=0)
    print("%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
    cvscores.append(scores[1] * 100)
print("%.2f%% (+/- %.2f%%)" % (np.mean(cvscores), np.std(cvscores)))
#==============================================================================


#==============================================================================
# 

# 
#predict
model.load_weights('weights.hdf5')
x = model.predict(x=x_val)
pred_rank = np.zeros((1,len(y_val)))
y_pred = np.zeros(len(y_val))
#index predictions
new_row = np.array(range(len(labels_index)))+1
new_row = np.asmatrix(new_row)
x = np.concatenate((new_row,x), axis=0)
for i in range(len(y_val)):
    rank = np.concatenate([x[0], x[i]]) #take only index and current row
    likelihood = rank[:, [y_val[i-1]]] #keep only correct column
    y_pred[i-1] = likelihood[0,0]
    likelihood  = likelihood[1,0]# find likelihood
    rank_n = x[[i]] 
    rank_n = rank_n[np.where(rank_n >= likelihood)]# drop those with lower likelihood
    pred_rank[0,i-1] = rank_n.shape[1]# append length value

#save predictions
np.savetxt(rank_output, pred_rank, delimiter=",")

#print f1 score
f1 = f1_score(y_val, y_pred, average='micro')
print("This is f1 score:", f1)

# summarize history for accuracy
#plt.plot(history.history['acc'])
#plt.plot(history.history['val_acc'])
#plt.title('Model Accuracy')
#plt.ylabel('Accuracy')
#plt.xlabel('Epoch')
#plt.legend(['Train', 'Test'], loc='upper left')

#plt.savefig(graph, format='png', dpi=500)
#plt.show()
# 
#==============================================================================
