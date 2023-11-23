## PreMSc Thesis: Investigating the Effect of Earnings Call Sentiment on Stock Volatility
## George Dreemer
## PreMSc Marketing Analyrics & Data Science

# NOTE: For outputs access this code on Google Collab: https://colab.research.google.com/drive/1cHAWOr2hcSNUctMLjWbyRG5einhLjRqw?usp=sharing


# LIBRARIES #

pip install tensorflow
pip install librosa
pip install numpy==1.21
#pip3 install torch==1.11.0+cpu torchvision==0.12.0+cpu torchaudio==0.11.0+cpu -f https://download.pytorch.org/whl/cpu/torch_stable.html
pip install transformers
pip install yfinance
pip install datetime
pip install scikit-learn
pip install nltk
#nltk.download(["names","stopwords","state_union","twitter_samples","movie_reviews","averaged_perceptron_tagger","vader_lexicon","punkt",])

import numpy as np
from numpy import mean
import pandas as pd
import gzip
import nltk
from nltk.corpus import stopwords
nltk.download(["stopwords","vader_lexicon","punkt"])
from nltk.tokenize import word_tokenize
from nltk.sentiment import SentimentIntensityAnalyzer
from pprint import pprint
from random import shuffle

from bs4 import BeautifulSoup
import requests
import gzip
import urllib.request
import json
import shutil
import moviepy.editor as mp
import tensorflow as tf
import keras
import librosa
import collections
import io
import glob
import os
from transformers import pipeline
from tqdm.auto import tqdm

import yfinance as yf
import datetime as datetime
from datetime import datetime
from datetime import timedelta, date
import os
import re
import matplotlib
import matplotlib.pyplot as plt

from sklearn import linear_model
import statsmodels.api as sm
from sklearn.naive_bayes import (
    BernoulliNB,
    ComplementNB,
    MultinomialNB,
)
from sklearn.neighbors import KNeighborsClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression
from sklearn.neural_network import MLPClassifier
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis
from sklearn.model_selection import train_test_split

import textwrap

from array import *

import seaborn as sns

from scipy.interpolate import make_interp_spline, BSpline

from pathlib import Path

from scipy.stats import norm

# [1] CLEANING #
def calls(path):

    line_by_line = []
    with open(path,'r') as file:
        lines = file.readlines()
        for line in lines:
            line_by_line.append(line)

    #Define PreQA and QA boundaries

    for i,line in enumerate(line_by_line):
        line = line.lower()
        if line.startswith('presentation'):
            pqa = i

        if line == "definitions\n" or "disclaimer\n":
            d = i

        if line == "questions and answers\n":
            qa = i

    PreQA = line_by_line[pqa:qa]
    QA = line_by_line[qa:d]



    #CLEANING PreQA

    for line in PreQA:
        if line.startswith('-'):
            PreQA.remove(line)
    for line in PreQA:
        if line.startswith('='):
            PreQA.remove(line)
    for line in PreQA:
        if line.endswith(']\n'):
            PreQA.remove(line)
    for line in PreQA:
        if line.startswith('Presentation\n'):
            PreQA.remove(line)
    for line in PreQA:
        if line.startswith('\n'):
            PreQA.remove(line)
    for line in PreQA:
        if line is '\n':
            PreQA.remove(line)
    for line in PreQA:
        if line.endswith(')\n'):
            PreQA.remove(line)

    clean_PreQAlines = []
    for line in PreQA:
        clean_PreQAlines.append(line.strip().replace('-','').replace('=','').replace('   ','').replace('  ',' '))

    PreQA_final = ' '.join(clean_PreQAlines)


    #CLEANING QA

    for line in QA:
        if line.startswith('-'):
            QA.remove(line)
    for line in QA:
        if line.startswith('='):
            QA.remove(line)
    for line in QA:
        if line.endswith(']\n'):
            QA.remove(line)
    for line in QA:
        if line.startswith('Questions '):
            QA.remove(line)
    for line in QA:
        if line.endswith(')\n'):
            QA.remove(line)
    for line in QA:
        if line.startswith('\n'):
            QA.remove(line)
    for line in QA:
        if line is '\n':
            QA.remove(line)

    clean_QAlines = []
    for line in QA:
        clean_QAlines.append(line.strip().replace('-','').replace('=','').replace('   ','').replace('  ',' '))

    QA_final = ' '.join(clean_QAlines)


    return (PreQA_final, QA_final)

# [2] Sentiment Analysis (NLTK) #
#Sentiment - "Divide and Conquer"

def sentiment_dac(PreQA_final, QA_final, chunk_size):

    stopwords = nltk.corpus.stopwords.words("english")

    #tokenize PreQA_final and QA_final, as well as exclude punctuation & stopwords
    PreQA_tkn = [w for w in word_tokenize(PreQA_final) if w.isalpha() if w.lower() not in stopwords]
    QA_tkn = [w for w in word_tokenize(QA_final) if w.isalpha() if w.lower() not in stopwords]

    all_tokens = PreQA_tkn + QA_tkn
    bla = ' '.join(all_tokens)
    #chunkify into chunks of x
    PreQA_chunks = textwrap.wrap(' '.join(PreQA_tkn), chunk_size)
    QA_chunks = textwrap.wrap(' '.join(QA_tkn), chunk_size)

    #get sentiment polarity scores (compound, neg, neu, pos)
    sia = SentimentIntensityAnalyzer()
    PreQA_ps_perchunk = []
    for chunk in PreQA_chunks:
        PreQA_ps_perchunk.append(sia.polarity_scores(chunk))

    QA_ps_perchunk = []
    for chunk in QA_chunks:
        QA_ps_perchunk.append(sia.polarity_scores(chunk))

    #mean compound score of all chunks
    PreQA_cs = mean([chunk_ps['compound'] for chunk_ps in PreQA_ps_perchunk]) / chunk_size *100
    QA_cs = mean([chunk_ps['compound'] for chunk_ps in QA_ps_perchunk]) / chunk_size *100



    #define if positive (1) or negative (0) - for fun - arbitrary

    if PreQA_cs > 0:
        PreQA_np = 1
    else:
        PreQA_np = 0

    if QA_cs > 0:
        QA_np = 1
    else:
        QA_np = 0

    return (PreQA_cs, QA_cs, PreQA_np, QA_np, bla, all_tokens)

#Determine optimal chunk size

#list of chunk sizes ranging from 50 - 800 with interval of 50
#lower bound - 75 characters is the average length of a sentence
#upper bound - 512 characters is the upper limit of sentiment algorithms, however we choose 600 to showcase how the c-score barely changes afterwards
chunk_sizes = list(range(75,800,50))
chunk_sizes

#calls function necessary to run sentiment_dac
path = '/content/drive/My Drive/PMT/Datasets/Transcripts/INTC/2016-Jan-14-INTC.txt'
preqa,qa = calls(path)

PreQA_compound = []
QA_compound = []

for cs in chunk_sizes:
    cs_test = sentiment_dac(preqa,qa,cs)
    PreQA_compound.append(sorted( {cs_test[0],cs} ))
    QA_compound.append(cs_test[1])

compound_score, chunk_size = zip(*PreQA_compound) # unpack a list of pairs into two tuples

#plt.plot(chunk_size,compound_score)
plt.figure(figsize=(12,8))
plt.bar(chunk_size,compound_score)
plt.plot(chunk_size,compound_score)
plt.show()

# 10 represents number of points to make between T.min and T.max
xnew = np.linspace(np.array(chunk_size).min(), np.array(chunk_size).max(), 10)

spl = make_interp_spline(chunk_size, compound_score, k=3)  # type: BSpline
compoundscore_smooth = spl(xnew)

plt.figure(figsize=(12,8))
plt.plot(xnew, compoundscore_smooth, label = "Compound Score")
plt.title("Determining the Optimal Chunk Size:\n When does the compound score level out?")
plt.xlabel("Chunk Size\n(# of characters)")
plt.ylabel("Compound Score")
plt.axvline(x=470,color='red').set_label("470 characters")
plt.legend(fancybox=True, framealpha=1, shadow=True, borderpad=1)
plt.savefig("/content/drive/My Drive/PMT/Datasets/Output/optimal_chunksize-plot")

plt.show()

# [3] delta-Volatility #
#Volatility Function

def volatility(filename,delta):

    #IV1 and IV2 -------------------------------------------------------------------------

    #ticker from filename
    ticker = filename.split('-')[-1].split('.')[0]

    #calldate from filename
    calldate = datetime.strptime(filename[:11], '%Y-%b-%d')

    beforedate = calldate - timedelta(days=10)
    afterdate = calldate + timedelta(days=10)

    df = yf.download(ticker, start=calldate-timedelta(days=delta), end=calldate+timedelta(days=delta))
    df['Volatility'] = df['Close'].pct_change().rolling(10).std().values

    #compute Δvolatility
    avgvol_before = mean(df['Volatility'].loc[beforedate:calldate].values.tolist())
    avgvol_after = mean(df['Volatility'].loc[calldate:afterdate].values.tolist())

    vol_delta = avgvol_after - avgvol_before

    #binary volatility increase variable: 0=no increase & 1=increase
    if avgvol_after > avgvol_before:
        vol_increase = 1
    else:
        vol_increase = 0

    #plot of volatility + save plot to folder Volatility Graphs
    savepath = '/content/drive/My Drive/PMT/Datasets/Output/Volatility Graphs/' + filename.split('.')[0] + '-plot'
    f = plt.figure()
    df["Volatility"].loc[beforedate:afterdate].plot(title="10 days close price historical volatility before & after the Earning's Call",figsize=(12,8))
    plt.axvline(x=calldate,color='red').set_label("Earning's Call Date")
    plt.xlabel('Date')
    plt.ylabel('Volatility')
    plt.legend(fancybox=True, framealpha=1, shadow=True, borderpad=1)
    plt.savefig(savepath);

    # NASDAQ Control Variable -------------------------------------------------------------

    df_nasdaq = yf.download("NDAQ", start=calldate-timedelta(days=delta), end=calldate+timedelta(days=delta))
    df_nasdaq['Volatility'] = df_nasdaq['Close'].pct_change().rolling(10).std().values

    #compute Δvolatility
    avgvol_before_nasdaq = mean(df_nasdaq['Volatility'].loc[beforedate:calldate].values.tolist())
    avgvol_after_nasdaq = mean(df_nasdaq['Volatility'].loc[calldate:afterdate].values.tolist())

    nasdaq_vol_delta = avgvol_after_nasdaq - avgvol_before_nasdaq

    #binary volatility increase variable: 0=no increase & 1=increase
    if avgvol_after_nasdaq > avgvol_before_nasdaq:
        nasdaq_vol_increase = 1
    else:
        nasdaq_vol_increase = 0



    return vol_delta, vol_increase, nasdaq_vol_delta, nasdaq_vol_increase
#    return df.loc[beforedate:afterdate]

# CREATE DATAFRAME WITH IVs and DV #
#IV1 and IV2 (compound and binary)
PreQA_Compound = []
QA_Compound = []
PreQA_NP = []
QA_NP = []

#DV (delta and binary)
Volatility_delta = []
Volatility_increase = []

#CV (delta and binary)
NASDAQ_Volatility_delta = []
NASDAQ_Volatility_increase = []


#Additional data for df
call_names = []


for folder in tqdm(os.listdir('/content/drive/My Drive/PMT/Datasets/Transcripts')):
    folder_path = '/content/drive/My Drive/PMT/Datasets/Transcripts/' + folder + '/*.txt'
    for f in glob.glob(folder_path):

        call_name = f.split('/')[-1]
        preqa,qa = calls(f)

        iv = sentiment_dac(preqa,qa,470)
        dv = volatility(call_name,30)

        call_names.append(call_name)
        PreQA_Compound.append(iv[0])
        QA_Compound.append(iv[1])
        PreQA_NP.append(iv[2])
        QA_NP.append(iv[3])

        Volatility_delta.append(dv[0])
        Volatility_increase.append(dv[1])

        NASDAQ_Volatility_delta.append(dv[2])
        NASDAQ_Volatility_increase.append(dv[3])

#Create DF from Dictionary
dict = {'Call Name': call_names, 'PreQA C-Score': PreQA_Compound, 'PreQA B-Sentiment': PreQA_NP, 'QA C-Score': QA_Compound, 'QA B-Sentiment': QA_NP, 'Volatility Delta': Volatility_delta, 'Volatility Increase': Volatility_increase, 'NASDAQ Vol. Delta': NASDAQ_Volatility_delta, 'NASDAQ Vol. Increase': NASDAQ_Volatility_increase}
pmt_df = pd.DataFrame(dict)

#Save as .csv and .xlsx (it saves to the Output folder)
spath_csv = Path('/content/drive/My Drive/PMT/Datasets/Output/pmt_data.csv')
spath_xlsx = Path('/content/drive/My Drive/PMT/Datasets/Output/pmt_data.xlsx')

pmt_df.to_csv(spath_csv, index=False)
pmt_df.to_excel(spath_xlsx, index=False)

# [4] MLR Model
#Load Dataset & Preview
pmt_dataset = pd.read_csv('/content/drive/My Drive/PMT/Datasets/Output/pmt_data.csv')

pmt_dataset.head(300)

#Descriptive Statistics
pmt_desc = pmt_dataset.describe()
pmt_desc.to_excel('pmt_desc.xlsx', index=False)

pmt_desc

#IVs: PreQA C-Score & QA C-Score
X = pmt_dataset[['PreQA C-Score', 'QA C-Score', 'NASDAQ Vol. Delta']]

#DV: Volatility Delta
Y = pmt_dataset['Volatility Delta']

#Split data into 80% training set and 20% test set
#X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=0)

#Linearity
fig, axs = plt.subplots(ncols=1)
sns.regplot(x='PreQA C-Score', y='Volatility Delta', data=pmt_dataset, ax=axs, label='PreQA C-Score (IV1)')
sns.regplot(x='QA C-Score', y='Volatility Delta', data=pmt_dataset, ax=axs, label='QA C-Score (IV2)')
sns.set(rc={'figure.figsize':(16,8)})
plt.legend(fontsize = 'large')
plt.xlabel('Compound Score')

#Fit regression
pmt_linreg = LinearRegression()
pmt_linreg.fit(X, Y)

predictions = pmt_linreg.predict(X)

residuals = Y - predictions

sns.distplot(residuals, bins=18)
plt.xlabel('Residuals')

def calculate_residuals(model, features, label):
    """
    Creates predictions on the features with the model and calculates residuals
    """
    predictions = model.predict(features)
    df_results = pd.DataFrame({'Actual': label, 'Predicted': predictions})
    df_results['Residuals'] = abs(df_results['Actual']) - abs(df_results['Predicted'])

    return df_results

# Checking linear assumpton: Normality, Homoscedasticity ..

def normal_errors_assumption(model, features, label, p_value_thresh=0.05):
    """
    Normality: Assumes that the error terms are normally distributed. If they are not,
    nonlinear transformations of variables may solve this.

    This assumption being violated primarily causes issues with the confidence intervals
    """
    from statsmodels.stats.diagnostic import normal_ad
    print('Assumption 2: The error terms are normally distributed', '\n')

    # Calculating residuals for the Anderson-Darling test
    df_results = calculate_residuals(model, features, label)

    print('Using the Anderson-Darling test for normal distribution')

    # Performing the test on the residuals
    p_value = normal_ad(df_results['Residuals'])[1]
    print('p-value from the test - below 0.05 generally means non-normal:', p_value)

    # Reporting the normality of the residuals
    if p_value < p_value_thresh:
        print('Residuals are not normally distributed')
    else:
        print('Residuals are normally distributed')

    # Plotting the residuals distribution
    plt.subplots(figsize=(12, 6))
    plt.title('Distribution of Residuals')
    sns.distplot(df_results['Residuals'])
    plt.show()

    print()
    if p_value > p_value_thresh:
        print('Assumption satisfied')
    else:
        print('Assumption not satisfied')
        print()
        print('Confidence intervals will likely be affected')
        print('Try performing nonlinear transformations on variables')

normal_errors_assumption(pmt_linreg, X, Y)

# Homoscedasticity
def homoscedasticity_assumption(model, features, label):
    """
    Homoscedasticity: Assumes that the errors exhibit constant variance
    """
    print('Assumption 5: Homoscedasticity of Error Terms', '\n')

    print('Residuals should have relative constant variance')

    # Calculating residuals for the plot
    df_results = calculate_residuals(model, features, label)

    # Plotting the residuals
    plt.subplots(figsize=(12, 6))
    ax = plt.subplot(111)  # To remove spines
    plt.scatter(x=df_results.index, y=df_results.Residuals, alpha=0.5)
    plt.plot(np.repeat(0, df_results.index.max()), color='darkorange', linestyle='--')
    ax.spines['right'].set_visible(False)  # Removing the right spine
    ax.spines['top'].set_visible(False)  # Removing the top spine
    plt.title('Residuals')
    plt.show()

homoscedasticity_assumption(pmt_linreg, X, Y)

#Coefficients
coeff_df = pd.DataFrame(pmt_linreg.coef_, X.columns, columns=['Coefficient'])
coeff_df