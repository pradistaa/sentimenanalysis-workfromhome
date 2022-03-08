# -*- coding: utf-8 -*-
"""
Created on Mon Jun  1 19:43:45 2020

@author: Admin
"""
import numpy as np
import pandas as pd
import re
import string
from sklearn.svm import SVC
from sklearn.linear_model import LogisticRegression
from Sastrawi.Stemmer.StemmerFactory import StemmerFactory
from Sastrawi.StopWordRemover.StopWordRemoverFactory import StopWordRemoverFactory,StopWordRemover, ArrayDictionary

def text_prep(text):
    factory = StemmerFactory()
    stemmer = factory.create_stemmer()
    text   = stemmer.stem(text)
    
    text = text.lower()
    text = re.sub(r"\d+", "", text)
    text = text.translate(str.maketrans("","",string.punctuation))
    text = text.strip()
    
    stop_factory = StopWordRemoverFactory().get_stop_words() #load default stopword
    more_stopword = ['wkwkwk', 'wkwk','iy','gk','bgt','banget','skrg']
    
    data = stop_factory + more_stopword
    dictionary = ArrayDictionary(data)
    stopr = StopWordRemover(dictionary)
    
    text = stopr.remove(text)
    
    return text
    
    

dataset = pd.read_csv("D:\Data\Tugas Kuliah\Tugas\Big Data\Source code Tubes\Dataset_Klasifikasi\dataset_klasifikasi_wfh_preproses.csv")
dataset_test = pd.read_csv("D:\Data\Tugas Kuliah\Tugas\Big Data\Source code Tubes\Dataset_Klasifikasi\dataset_test.csv")
#X = dataset['text']
#y = dataset['klasifikasi']
X_train = dataset['text']
y_train = dataset['klasifikasi']
X_test = dataset_test['text']
y_test = dataset_test['klasifikasi']

X_test = X_test.apply(lambda x : text_prep(x))

#from sklearn.model_selection import train_test_split
#X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25 , random_state=0)

from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.feature_extraction.text import TfidfVectorizer

vectorizer = TfidfVectorizer(max_features=1500,ngram_range=(1,1))
train_vectors = vectorizer.fit_transform(X_train)
test_vectors = vectorizer.transform(X_test)
print(train_vectors.shape, test_vectors.shape)

print("========= SVM =========")
clf = SVC(kernel='linear').fit(train_vectors, y_train)
from  sklearn.metrics  import accuracy_score
predicted1 = clf.predict(test_vectors)
print(accuracy_score(y_test,predicted1))

print("========= Logistic Regression =========")
clf = LogisticRegression(max_iter=10000).fit(train_vectors, y_train)
predicted2 = clf.predict(test_vectors)
print(accuracy_score(y_test,predicted2))
