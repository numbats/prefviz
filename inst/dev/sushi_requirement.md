### Analysis requirements for `sushi_ranking` (BayesMallows package)

**General setup**

- Use **R** and write the analysis in a **Quarto** `.qmd` file.  
- Everything should be in folder `inst/dev/`.
- The `.qmd` should:
  - Use the `sushi` dataset by running `prefio::read_preflib("00014 - sushi/00014-00000001.soc", from_preflib = TRUE)`.
  - Perform all analyses described in Parts 1–3.  
  - Be rendered to a **self-contained HTML report** at the end.
  - Output should be named: sushi_analysis_prefviz.html
- The **HTML report** must:
  - **Front‑load the conclusions**: start with a short “Summary of findings” section at the top, clearly stating the main answers from Parts 1–3 (most popular options, presence of real agreement, and clustering results).  
  - Then present the detailed methods, code, tables, and plots supporting those conclusions.  
- Work with the complete ranking data as provided (rows = respondents, columns/items = sushi options, values = ranks where 1 is most preferred).
- For all questions, use `prefviz` functions as much as possible: i.e., IRV result, first preference, Condorcet winner, etc.

***

## Part 1 – Most popular options (IRV, Condorcet, first preferences)

Goal: Identify “most popular” sushi options using **three voting-style rules** on the ranking data.

1. **First-preference counts**
   - For each sushi option, count how many respondents ranked it **1st**.  
   - Report in the HTML:
     - A table with sushi option, count of 1st-place votes, and proportion of respondents.  
     - The option with the **largest number of first-preference votes**.

2. **Instant-Runoff Voting (IRV) winner**
   - Implement an IRV procedure using the rankings:
     - Start by counting only **first-place** votes.  
     - If one option has a majority (> 50%), it is the winner.  
     - If not, eliminate the option with the **fewest** first-place votes.  
     - Transfer ballots for the eliminated option to the next highest-ranked option on each ballot that is still in the race.  
     - Repeat until one option has a majority or only one option remains.
   - Report in the HTML:
     - The **sequence of eliminations**.  
     - The **IRV winner**.

3. **Condorcet analysis**
   - For each pair of sushi options (A, B), compute:
     - The number of respondents who rank A above B.  
     - The number who rank B above A.  
   - Determine whether a **Condorcet winner** exists:
     - An option that wins a strict majority head-to-head against every other option.
   - Report in the HTML:
     - If a Condorcet winner exists, identify it.  
     - If not, state that there is no Condorcet winner (due to preference cycles), and provide an example cycle if found.

4. **Summary inside report**
   - In the early “Summary of findings” section, clearly state:
     - First-preference leader.  
     - IRV winner.  
     - Condorcet winner (if any).  
     - Whether these three agree or differ.

***

## Part 2 – Clustering with probabilistic class membership (not using BayesMallows clustering)

Goal: Cluster respondents into latent preference classes and obtain, **for each observation**, the **probability of membership** in each class. Do **not** use the clustering / mixture functionality provided by BayesMallows.

1. **Choice of clustering model**
   - Use a **model-based clustering method for ranking data** implemented in R that is **independent of BayesMallows**.  
   - Acceptable approaches include:
     - A **mixture of Mallows models** or similar ranking models from another package (e.g., `MSmix`, `Rankcluster`, or other R packages that handle rank data), or  
     - A general latent class / mixture modeling approach that supports ranking items.  
   - In code comments and in the report’s methods section, state:
     - Which package and model you chose.  
     - A brief justification (1–2 sentences) and reference to package documentation.

2. **Data preparation**
   - Use the same ranking matrix as in Part 2.  
   - If the chosen package requires a specific format (e.g., order/position vectors, long format, or permutations), reshape the data accordingly.

3. **Number of clusters**
   - Fit models with several candidate cluster numbers, e.g. \(K = 1, 2, 3, 4\).  
   - For each \(K\), compute and record:
     - A model selection criterion (e.g., BIC or AIC).  
   - Choose a **preferred number of clusters** based on these criteria (typically the lowest BIC).  
   - In the report, explain briefly how the number of clusters was chosen.

4. **Fit final clustering model**
   - Fit the final model using the selected number of clusters.  
   - Extract and report:
     - **Posterior class membership probabilities** for each respondent: matrix with rows = respondents, columns = clusters, entries = \(P(\text{class } k \mid \text{their ranking})\).  
     - Cluster-specific preference summaries, such as:
       - Central rankings or the top 3–5 sushi items in each cluster.

5. **Outputs and interpretation in the HTML report**
   - Include:
     - A table or heatmap of **average ranks or top items per cluster**.  
     - A table of **cluster sizes** (counts and proportions).  
     - A table or downloadable object (if appropriate) with **posterior membership probabilities** by respondent.
     - A 2D ternary plot using the **posterior probabilities** of each respondent in each cluster.
   - In the “Summary of findings” at the top:
     - State the chosen number of clusters.  
     - Briefly describe each cluster in plain language (e.g., “Cluster 1 prefers X and Y; Cluster 2 prefers A and B”).  
     - Mention that respondents are assigned **probabilistically** to clusters.

***

At the very top of the HTML report, include a short **“Summary of findings”** section (1–3 paragraphs) that concisely answers:

- Which sushi options are most popular (by first preference, IRV, and Condorcet).  
- Whether there is meaningful agreement in rankings (Kendall’s \(W\) result).  
- How many preference clusters exist, what they look like, and that each respondent has a probability of belonging to each cluster.