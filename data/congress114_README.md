# 114th Congress Speech Data for congress114.R

The script `congress114.R` is structurally identical to `congress109.R` but uses **114th Congress** data (2015–2017), the most recent congress available in the Stanford Congressional Record dataset.

## Data Files (included)

- **congress114.csv**: 99 senators × 5,031 phrase bigrams (Senate only; from STBS/Stanford hein-daily)
- **congress114members.csv**: Columns = `name`, `party`, `state`, `chamber`, etc.

## Data Source

The underlying data comes from:

**Congressional Record for the 43rd–114th Congresses: Parsed Speeches and Phrase Counts**  
Gentzkow, Matthew; Shapiro, Jesse M.; Taddy, Matt. Stanford Libraries, 2018.  
https://data.stanford.edu/congress_text

- **Coverage**: 43rd–114th Congress (1873–2017)
- **114th Congress**: 2015–2017 (most recent in this dataset)
- **License**: Open Data Commons Attribution (ODC-BY 1.0)

## How to Obtain the Data

1. **Register** at [data.stanford.edu/congress_text](https://data.stanford.edu/congress_text) (Stanford Libraries SSDS)
2. **Download** `hein-daily.zip` – contains:
   - `speeches_114.txt`, `descr_114.txt`, `114_SpeakerMap.txt` – speeches and speaker metadata
   - `byspeaker_2gram_114.txt` – phrase counts by speaker (if available in your download)
3. **Process** the raw files into congress109-style format.

### Option A: Use STBS preprocessing pipeline

The [STBS repository](https://github.com/vavrajan/STBS) provides a Python script that processes Stanford hein-daily data:

1. Download hein-daily from Stanford and extract to `data/hein-daily/orig/`
2. Run `preprocess_speeches.py` from the STBS repo (requires `pandas`, `scipy`, `scikit-learn`)
3. This produces `counts114.npz`, `author_indices114.npy`, `vocabulary114.txt`, `author_info114.csv` in a `clean/` folder
4. Write a conversion script to aggregate counts by author and output congress114.csv + congress114members.csv

### Option B: Process raw Stanford files directly

If `byspeaker_2gram_114.txt` exists in hein-daily, it may already contain speaker-level phrase counts. Check the Stanford codebook for the exact format. Otherwise, merge `speeches_114.txt` with `114_SpeakerMap.txt`, extract bigrams (e.g. via `CountVectorizer` or similar), aggregate by speaker, and match to party/state/chamber from the speaker map.

### Output format

- **congress114.csv**: First column = member name (row names), remaining columns = phrase bigrams, values = counts. Phrase names in congress109 use dots (e.g. `health.care.reform`); Stanford/CountVectorizer may use spaces—normalize as needed.
- **congress114members.csv**: Columns `name`, `party`, `state`, `chamber` (and optionally `repshare`, `cs1`, `cs2` if available). Row names = member names, matching congress114.csv.

## Alternative: ICPSR Data (104th–110th Only)

ICPSR study [33501](https://www.icpsr.umich.edu/web/ICPSR/studies/33501) provides phrase counts for 104th–110th Congresses. It does **not** include the 114th Congress. For the most recent data, use the Stanford source above.

## Regenerating the Data

To regenerate from the STBS source:

1. Create `temp_stbs/` and download from [STBS GitHub](https://github.com/vavrajan/STBS/tree/master/data/hein-daily/clean): `counts114.npz`, `author_indices114.npy`, `vocabulary114.txt`, `author_info114.csv`
2. Run `python convert_stbs_to_congress114.py` (requires numpy, scipy, pandas; use a venv)

## Note on Current Congress (118th)

There is no publicly available phrase-count dataset in the congress109 format for the 118th Congress (2023–present) or later. The Stanford dataset ends at the 114th Congress (2017).
