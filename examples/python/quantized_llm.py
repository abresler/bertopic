# https://colab.research.google.com/drive/1DdSHvVPJA3rmNfBWjCo2P1E9686xfxFx?usp=sharing#scrollTo=rbwHbJc_YXi8

from datasets import load_dataset
import os
# Use llama.cpp to load in a 4-bit quantized version of Zephyr 7B Alpha
# and truncate each document to 50 words
os.chdir('/Users/alexbresler/')

# ArXiv ML Documents
docs = load_dataset("CShorten/ML-ArXiv-Papers")["train"]["abstract"]
sample_docs = docs[:5000]
from llama_cpp import Llama

# Use llama.cpp to load in a Quantized LLM
llm = Llama(model_path="openhermes-2.5-mistral-7b.Q4_K_M.gguf", n_gpu_layers=-1, n_ctx=4096, stop=["Q:", "\n"])

from bertopic.representation import KeyBERTInspired, LlamaCPP

prompt = """ Q:
I have a topic that contains the following documents:
[DOCUMENTS]

The topic is described by the following keywords: '[KEYWORDS]'.

Based on the above information, can you give a short label of the topic of at most 5 words?
A:
"""

representation_model = {
    "KeyBERT": KeyBERTInspired(),
    "LLM": LlamaCPP(llm, prompt=prompt),
}


