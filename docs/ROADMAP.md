# Shiny Project: Roadmap

> The plan we follow from start to finish. Tick boxes as work gets done, and log decisions at the bottom.
> Official brief: the project description given by the teacher (S. Affeldt).

## Deadline

- **Wednesday 31 January 2027, 20:00**: the deployed app URL goes into the groups file.
- The app must be **deployed and working**, probably on shinyapps.io (`rsconnect` was already used in `cours1_2`).
- Group of 3.

## What the app does

The user loads a dataset, prepares it, explores it, then trains and compares classifiers.
The app also contains a full case study on one dataset we choose.

```
[1. Load data] → [2. Preprocessing] → [3. Exploration] → [4. Modelling] → [5. Case study]
```

Each step is one tab in the app (`navbarPage` or `tabsetPanel`).
All steps share one `reactiveValues` object that holds the raw data and the prepared data.

---

## Phase 0: Setup

- [x] Agree on the app layout (one `app.R` or `ui.R` + `server.R` + `R/` helper files)
- [x] Pick the R packages (see the list below) and check they install
- [x] Do a test deployment of a "hello world" app on shinyapps.io now, so deployment problems show up early → https://ethica79.shinyapps.io/projet_mlds/
- [x] Set up Git or another shared folder for the group → https://github.com/monci2004/Programmation-web-Project (skeleton not pushed yet)

## Phase 1: Loading data (brief point 1)

> **Clarification from the teacher:** the app accepts **only one file format** → **we chose CSV**,
> but it must **analyse what's inside the file** and spot the problems on its own.

- [ ] Upload a **CSV file only** (`fileInput` with `accept = ".csv"`), with options for separator (`,` `;` tab), decimal mark (`.` `,`) and header
- [ ] Preview the data (table, dimensions)
- [ ] **Detect each variable's type automatically** (quantitative or qualitative), and let the user change it by hand
  - numbers with few distinct values (e.g. 0/1, 1–5) → probably qualitative
  - text that is really numbers (e.g. `"3,5"` with a comma decimal) → convert to quantitative
- [ ] **Content checks** (shown as warnings, with an option to drop the column or row):
  - [ ] **Constant** column (a single value) → useless, suggest dropping it
  - [ ] **Almost constant** column (e.g. more than 95% the same value)
  - [ ] **Identifier** column (every value different, e.g. `id`, `patient_no`) → suggest dropping it
  - [ ] **Missing values written as codes**: `"?"`, `""`, `"NA"`, `"-"`, `-999`… → turn them into real `NA` (UCI datasets use `"?"`)
  - [ ] **Duplicate rows** and **duplicate columns**
  - [ ] **Mixed column** (mostly numbers with a few text values)
  - [ ] Qualitative column with **too many categories**
  - [ ] Column that is **entirely empty**, or with too many `NA` (e.g. > 50%)
  - [ ] **Row with many `NA`** (e.g. missing in more than 50% of its columns, adjustable threshold) → filling it in would mostly create made-up values, so suggest dropping the row. Show how many rows are affected, plus a histogram of "number of `NA` per row"
- [ ] Summary table per variable: detected type, number of distinct values / categories, % missing, number of outliers, warnings
- [ ] Let the user choose the **target variable** (the class to predict)

## Phase 2: Preprocessing (brief point 1)

Each option can be switched on or off in the interface:

- [ ] **Missing values**: first drop the rows/columns that are too empty (thresholds from Phase 1), then for the rest drop rows or fill in with the mean/median (quantitative) or the most frequent value (qualitative)
- [ ] **Outliers**: detect them with the IQR rule (boxplot rule), then keep, remove or cap them
- [ ] **Qualitative variables with many categories**: merge rare categories into "Other"
- [ ] **Normalization**: none, z-score, or min-max
- [ ] **Dummification**: one-hot encoding of qualitative variables
- [ ] **Class imbalance**: show the class distribution, then offer oversampling, undersampling or class weights
- [ ] **Manual drop**: the user can delete any columns they judge useless (`selectInput(multiple = TRUE)`), and rows (select them in the `DT` table, or by condition / by index)
- [ ] **Merge columns** when needed:
  - quantitative: combine with an operation (sum, mean, difference, ratio) into a new column
  - qualitative: paste the values together (e.g. `A` + `B` → `A_B`)
  - the user names the new column and chooses whether to keep or drop the originals
- [ ] Show the prepared dataset afterwards, with a summary of what was changed

> **Note: history of processing steps (undo)**
> Every operation (drop, merge, imputation, outliers, normalization, dummification, rebalancing…) is recorded in a **history list**:
> - each entry stores the **step name**, its **parameters**, and the **dataset before the step** (or enough to replay it)
> - the history is shown in the interface as an ordered list (step 1, step 2…)
> - an **"Undo"** button goes back one step, and the user can **return to any earlier step** (everything after it is removed)
> - a **"Reset"** button goes back to the raw data
> - in Shiny: a list inside `reactiveValues`, e.g. `rv$history <- list(list(step = "drop_cols", params = ..., data = ...))`
> - the same history can be reused to **describe the pipeline** in the case study

## Phase 3: Exploratory analysis (brief point 2)

Reuse what we saw in class (`cours1_unidimensionelle`, `Cours2_bidimensionel`).

The course scripts hard-code column names (EF/CA, Sex/Level). We make them work for **any variable the user picks**, using `selectInput`.

**One variable at a time** (course 1)
- [ ] Statistics table: counts, cumulative counts, frequencies, cumulative frequencies
- [ ] Quantitative: histogram (bin rule Sturges/Scott/FD, counts or density, density curve, quantile lines), boxplot + `rug`, empirical cumulative distribution (ECDF), centre and spread (min, max, mean, median, Q1, Q3, variance, standard deviation)
- [ ] Qualitative: bar chart, pie chart, frequency table

**Pairs of variables** (course 2)
- [ ] Quant × Quant: scatter plot, back-to-back histograms (`Hmisc::histbackback`), covariance, **Pearson correlation r**, regression line `lm`
- [ ] Quant × Qual: boxplots side by side (base R + ggplot with jitter), **correlation ratio η** (between-group vs within-group variance)
- [ ] Qual × Qual: contingency table, bar charts (stacked, side by side, profile plot with `position="fill"`), **χ², Φ², Cramér's V**
- [ ] Optional: correlation matrix (heatmap) for all quantitative variables

## Phase 4: Modelling (brief point 2)

- [ ] Choose the models (at least 2), for example **logistic regression** + **Random Forest** (a decision tree, SVM or k-NN could be a third)
- [ ] Train/test split (adjustable ratio), and cross-validation if possible
- [ ] Metrics for each model: **Precision, Recall, F-score**, confusion matrix
- [ ] **ROC curve + AUC**, with the models plotted on the same graph
- [ ] Table comparing the models side by side
- [ ] **Feature importance** (Random Forest importance, logistic coefficients)
- [ ] Handle multi-class targets (macro-averaged metrics, one-vs-rest ROC), or limit the app to binary targets and say so

> **Note: history of model results**
> Every training run is saved in a **results history** so we can compare runs, not just the models of the current run:
> - each entry stores: **run number / time**, **model** and its **parameters** (e.g. `ntree`), **train/test split**, the **preprocessing steps applied** (taken from the processing history of Phase 2), the **metrics** (Precision, Recall, F-score, AUC), and what's needed to **redraw the ROC curve** and feature importance
> - shown as a **table of all runs** (sortable with `DT`), with the best value of each metric highlighted
> - the user can **select several runs** to overlay their ROC curves and compare them
> - buttons to **delete a run** or **clear the history**, and to **export it as CSV** (useful for the case study report)
> - in Shiny: a list inside `reactiveValues`, e.g. `rv$model_history`, with one entry added each time "Train" is clicked
> - this makes it easy to answer questions like *"did normalization improve the Random Forest?"*

## Phase 5: Case study (brief point 3)

- [ ] Pick the dataset from the list in the brief
- [ ] Do the full study **with our app**: data → preprocessing → exploration → models → conclusions
- [ ] Present it in a dedicated tab (text + charts), or explain it in a report

## Phase 6: Finishing up

- [ ] Test with several datasets (small/large, binary/multi-class, with or without missing values)
- [ ] Error handling (`req()`, `validate()`, clear messages instead of crashes)
- [ ] Clean up the interface (labels, help texts)
- [ ] Deploy the final version and check the URL works
- [ ] Put the URL in the groups file **before 31/01/2027, 20:00**

---

## R packages (installed 2026-10-27)

| Purpose | Package |
|---|---|
| App | `shiny` (+ optional `bslib` for styling) |
| Tables | `DT` |
| Plots | base R + `ggplot2`, `reshape2`, `Hmisc` (all used in class) |
| Models | `randomForest`, `glm` (base R) |
| Metrics / ROC | `pROC` (+ optional `caret`) |
| Deployment | `rsconnect` |

## Decisions to make

- [x] Which file format: **CSV**
- [ ] Which models? (suggestion: logistic regression + Random Forest)
- [ ] Binary only, or multi-class too?
- [ ] Which case-study dataset?
- [ ] How is the work split between the 3 members?

## Decision log

| Date | Decision |
|---|---|
| 2026-10-27 | Roadmap created. Phase 0 next. |
| 2026-10-27 | Layout: `app/app.R` (navbarPage, 5 tabs) + `app/R/` helpers. Deployed as `projet_mlds`. |
| 2026-10-27 | Phase 3 lined up with the course scripts (cours 1 & 2). Deploy to shinyapps.io account `ethica79`. |
| 2026-10-27 | Teacher: only one file format accepted, but its content must be checked automatically (constants, IDs, coded NAs…). Added to Phase 1. |
| 2026-10-27 | File format: **CSV only** (`read.csv`, as in class, no extra package). Packages installed: DT, randomForest, pROC, Hmisc. |
| 2026-10-27 | Group repo = monci2004/Programmation-web-Project. Added `.gitignore`. Our `app/` skeleton + roadmap stay local for now, pushed later. |
| 2026-10-27 | GitHub SSH set up, `.gitignore` pushed. Teammate Achraf pushed an example single-file `app.R` (446 lines, 3 tabs: Données, Exploration, Modèles; uses DT, ggplot2, randomForest, pROC). To review before building Phase 1. |
| 2026-10-27 | Phase 2: added manual drop of rows/columns, column merging, and a processing history with undo / go back / reset. |
| 2026-10-27 | Phase 4: added a history of model results (all runs with params, preprocessing, metrics, ROC; compare / delete / export). |
