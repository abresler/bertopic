import re
import pandas as pd
from bertopic import BERTopic

trump = pd.read_csv('https://drive.google.com/uc?export=download&id=1xRKHaP-QwACMydlDnyFPEaFdtskJuBa6')
trump.text = trump.apply(lambda row: re.sub(r"http\S+", "", row.text).lower(), 1)
trump.text = trump.apply(lambda row: " ".join(filter(lambda x:x[0]!="@", row.text.split())), 1)
trump.text = trump.apply(lambda row: " ".join(re.sub("[^a-zA-Z]+", " ", row.text).split()), 1)
trump = trump.loc[(trump.isRetweet == "f") & (trump.text != ""), :]
timestamps = trump.date.to_list()
tweets = trump.text.to_list()

model = BERTopic(verbose=True)
topics, probs = model.fit_transform(tweets)
topics_over_time = model.topics_over_time(tweets, timestamps)
viz = model.visualize_topics_over_time(topics_over_time, topics=[9, 10, 72, 83, 87, 91])

