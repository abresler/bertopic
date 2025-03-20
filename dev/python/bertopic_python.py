from bertopic import BERTopic
from sklearn.datasets import fetch_20newsgroups
from sklearn.feature_extraction.text import CountVectorizer

docs = fetch_20newsgroups(subset='all',  remove=('headers', 'footers', 'quotes'))['data']

#' Quick Start
#' add stopwords to bertopic
topic_model = BERTopic(verbose = True)
topics, probs = topic_model.fit_transform(docs)

# Get Topics

topic_model.get_topic_info()

topic_model.get_topic(0)

#' https://maartengr.github.io/BERTopic/getting_started/quickstart/quickstart.html

vectorizer_model = CountVectorizer(ngram_range=(1, 2), stop_words="english")
