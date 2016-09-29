# Data notes

## Data files: decisions.csv and decisions_2.csv

- decisons.csv is the original file
- decisions_2.csv has been lightly cleaned in LibreOffice Calc

Variable definitions for decisions.csv and decisions_2.csv:

Variable | Definition
-------- | ----------
sort: | sequential list of numbers to sort by, if needed
ms_id: | manuscript identification number
author_count: | number of authors per manuscript
author_sex_ratio: | ratio of females to males authors per manuscript
corr_auth_sex: | sex of corresponding author: 0 male ; 1 female
first_auth_sex: | sex of first author: 0 male ; 1 female
senior_auth_sex: | sex of senior author: 0 male ; 1 female
submit_auth_sex: | sex of submitting author: 0 male ; 1 female
corr_auth_first_auth: | corresponding author is first author: 0 false ; 1 true
corr_auth_submit_auth: | corresponding author is submitting author: 0 false ; 1 true
submit_auth_first_auth: | submitting author is first author: 0 false ; 1 true
submit_auth_senior_auth: | submitting author is senior author: 0 false ; 1 true
corr_auth_senior_auth: | corresponding author is senior author: 0 false ; 1 true
no_auth_sex_ident: | number of authors where sex has been identified
first_auth_geog: | geographical region of first author
corr_auth_geog: | geographical region of corresponding author
submit_auth_geog: | geographical region of submitting author
senior_auth_geog: | geographical region of senior author
handling_editor: | handling editor ID 
handling_editor_sex: | sex of handling editor
handling_editor_geog: | geographical region of handling editor
editor_seniority: | seniority in years since PhD of editor
editor_years: | years as handling editor
sex_ratio_reviewers: | ratio of females to males of reviers per manuscript 
mean_review_score: | mean review score (lower better) 
mean_reviewer_days_respond: | mean number of days for reviewers to respond for request to review
mean_reviewer_days_review: | mean number of days for reviewers to review manuscript
prop_reviewers_responding: | proportion of reviewers responding to request for review
prop_reviewers_agreeing: | proportion of reviewers agreeing to review
sent_for_review: | sent for review: 0 No ; 1 Yes 
max_review_score: | maximum review score per manuscipt
paper_rejected: | paper rejected: 0 No ; 1 Yes
no_reviews_obtained: | number of reviews received from reviewers
no_reviews_responded: | number of reviewers responding to request for review
title_word_count: | word count of manuscript titles
abstract_word_count: | word count of manuscript abstract
time_to_decision: | time in days to decision on manuscript

## Data file: author_decisions.csv

Whereas the data in decisions*.csv groups variables by manuscript, author_decisions.csv unpacks manuscript data and contains extra observations about authors per manuscript.

Variable definitions for decisions.csv and decisions_2.csv:

Variable | Definition
-------- | ----------
manuscript_id: | identification number for the manuscript
sort: | sequential list of numbers to sort by, if needed
submit_year: | year manuscript was submitted
submit_month: | month manuscript was submitted
author_person_id: | identification number for author (sequential)
author_order: | author's order in byline
corresponding_author: | author is corresponding author: 0 No ; 1 Yes ; 2 None
submitting_author: | author is submitting author: 0 No ; 1 Yes
senior_author: | author is senior author (last author): 0 No ; 1 Yes
missing_authors: | any missing authors: 0 No ; 1 Yes 
author_country: | country of author based on byline
HDI: | human development index score (see notes below)
language: | language of author country (see notes below)
geographic region: | geographic region of author
author_sex: | sex of author (male/female)
prob_sex: | probability that author_sex classification is true
author_institution: | author's institution based on byline 
manuscript_status: | acceptance, revision, rejection status of mansuscript 
final_decision: | final decision on manuscript
submit_date: | submission date in %m/%d/%y format

## Notes on variable *language* in author_decisions.csv

**Human Development index scores**

- Caymen Islands is not listed, since it is a British Territory, we use the United Kingdom index.
- Martinique is a territory of France, so we use the France index.
- Monaco is an independent country, but does not have a HDI, so we use France, its closest neighbor.
- New Caledonia is a part of France, so we use the France Index.
- Puerto Rico is part of the US, so we use the US index.
- Svalbard and Jan Mayan is part of Norway, so we use the Norway index.
- Taiwan is counted as China.
- French Guiana uses the France index.

Source: http://hdr.undp.org/en/countries, accessed on May 27, 2016

**Languages, CIA World Handbook:**

- In the CIA World Handbook, languages are listed in rank order by country.
- If English is listed as an official language, despite the rank order, then English is selected. This is done to reduce bias in the analysis.
- If the country is not listed, then an alternate site is used.

Notes and alternate sites for the following countries are listed below:

- Cameroon people speak over two dozen African langauges (no dominant language), but English is listed as one of the official languages, so English is listed as the language for Cameroon.
- French Guiana: French from Wikipedia (5/31/2016)
- Ghana lists English as an official language. English is used.
- India: English
- Martinique: French from Wikipedia (5/31/2016)
- Namibia: English
- Norway lists two versions of Norwegian: Bokmal Norwegian (official) and Nynorsk Norwegian (official). I reduced to Norwegian.
- Rwanda: English.
- Sri Lanka: English has special status in the constitution. English is used.

Source: 
https://www.cia.gov/library/publications/the-world-factbook/fields/2098.html, 
accessed on May 31, 2016