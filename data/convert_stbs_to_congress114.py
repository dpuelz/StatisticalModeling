#!/usr/bin/env python3
"""Convert STBS 114th Congress data to congress109-style format.
Requires: numpy, scipy, pandas
Run from StatisticalModeling/data directory.
"""
import numpy as np
import pandas as pd
from scipy import sparse
from pathlib import Path

TEMP_DIR = Path("temp_stbs")
OUT_DIR = Path(".")

# Load data
counts = sparse.load_npz(TEMP_DIR / "counts114.npz")
author_indices = np.load(TEMP_DIR / "author_indices114.npy")
with open(TEMP_DIR / "vocabulary114.txt") as f:
    vocab = [line.strip() for line in f]
author_info = pd.read_csv(TEMP_DIR / "author_info114.csv", index_col=0)

# Convert vocabulary to dot format (congress109 uses "word.word" not "word word")
vocab_dots = [v.replace(" ", ".") for v in vocab]

# Aggregate counts by author: sum rows for each author
n_authors = author_info.shape[0]
n_terms = counts.shape[1]
author_counts = np.zeros((n_authors, n_terms))
for i in range(n_authors):
    mask = author_indices == i
    author_counts[i, :] = np.asarray(counts[mask, :].sum(axis=0)).flatten()

# Build member names (First Last format from author_info)
names = author_info["name"].str.title().values

# congress114.csv: rows=members, cols=phrases
count_df = pd.DataFrame(author_counts, index=names, columns=vocab_dots)
count_df.index.name = "name"
count_df.to_csv(OUT_DIR / "congress114.csv")

# congress114members.csv: name, party, state, chamber
# STBS is Senate only; format names to match congress109 (Title Case)
members = author_info.copy()
members["name"] = members["name"].str.title()
members["chamber"] = "S"  # Senate only
# congress109 has repshare, cs1, cs2 - we don't have these, use NA
members["repshare"] = np.nan
members["cs1"] = np.nan
members["cs2"] = np.nan
members = members[["name", "party", "state", "chamber", "repshare", "cs1", "cs2"]]
members.to_csv(OUT_DIR / "congress114members.csv", index=False)

print(f"Created congress114.csv: {count_df.shape[0]} members x {count_df.shape[1]} phrases")
print(f"Created congress114members.csv: {members.shape[0]} members")
