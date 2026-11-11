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
- [x] Set up Git or another shared folder for the group → https://github.com/MLSD-UParisCite-projects/Programmation-web-Project

## Review of the demo `app.R` (Achraf, 2026-10-27)

The demo runs fine with its built-in dataset. It's a good base, but it's a demo: we keep the ideas, and rebuild the structure.

**To keep**
- Calculation functions separated from the UI (`traiter_na`, `supprimer_outliers`, `normaliser`, `dummifier`, `sur_echantillonner`, `evaluer`) → move them into `R/`
- The chain of reactives (raw → no NA → no outliers → normalized → dummified) → base for the processing history
- Oversampling applied **on the training set only, after the split** (correct)
- Modelling tab: logistic regression + Random Forest, Precision / Recall / F-score / AUC, both ROC curves on one plot (`ggroc`), confusion matrices, importance (Gini + |z|)
- Bivariate analysis that adapts to the variable types (Pearson/Spearman, η², χ² + Cramér's V)

**To fix**
- [x] **`"?"` breaks loading**: tested with a UCI-style CSV, a single `"?"` turns a numeric column into a factor → `read.csv(..., na.strings = c("?", "", "NA"))` + separator / decimal options (see Phase 1)
- [x] **Data leakage**: normalization and imputation are computed on the whole dataset before the train/test split → compute the parameters (mean, sd, median, mode) **on train only**, then apply them to test
- [x] **Positive class chosen alphabetically** (`lev[2]`) → let the user choose the positive class (Precision / Recall depend on it)
- [x] **Split not stratified** → with an imbalanced target, the test set may have too few (or zero) minority rows, and `roc()` fails → stratified split by class
- [x] **Type detection too simple** (numeric = quantitative) → `0/1` or `1–5` codes should be suggested as qualitative (see Phase 1)
- [x] Rows with `NA` are silently dropped before training (`complete.cases`) → tell the user how many

**Missing compared to this roadmap**
- Content checks (constants, IDs, rows with many `NA`, duplicates…)
- Manual drop / merge of columns
- Processing history (undo) and model results history
- Course plots: ECDF, histogram bin rules (Sturges/Scott/FD), back-to-back histograms, pie chart, `rug`, statistics table
- `global.R` + `R/` + `modules/` split, "Case study" tab

**Decision proposed**: keep the demo's functions and reactive logic, rebuild the structure (`global.R`, `R/`, one module per tab) following this roadmap.

---

## Phase 1: Loading data (brief point 1)

> **Clarification from the teacher:** the app accepts **only one file format** → **we chose CSV**,
> but it must **analyse what's inside the file** and spot the problems on its own.

- [x] Upload a **CSV file only** (`fileInput` with `accept = ".csv"`), with options for separator (`,` `;` tab), decimal mark (`.` `,`) and header
- [x] Preview the data (table, dimensions)
- [x] **Detect each variable's type automatically** (quantitative or qualitative), and let the user change it by hand
  - numbers with few distinct values (e.g. 0/1, 1–5) → probably qualitative
  - text that is really numbers (e.g. `"3,5"` with a comma decimal) → convert to quantitative
- [x] **Content checks** (shown as warnings, with an option to drop the column or row):
  - [x] **Constant** column (a single value) → useless, suggest dropping it
  - [x] **Almost constant** column (e.g. more than 95% the same value)
  - [x] **Identifier** column (every value different, e.g. `id`, `patient_no`) → suggest dropping it
  - [x] **Missing values written as codes**: `"?"`, `""`, `"NA"`, `"-"`, `-999`… → turn them into real `NA` (UCI datasets use `"?"`)
  - [x] **Duplicate rows** and **duplicate columns**
  - [x] **Mixed column** (mostly numbers with a few text values)
  - [x] Qualitative column with **too many categories**
  - [x] Column that is **entirely empty**, or with too many `NA` (e.g. > 50%)
  - [x] **Row with many `NA`** (e.g. missing in more than 50% of its columns, adjustable threshold) → filling it in would mostly create made-up values, so suggest dropping the row. Show how many rows are affected, plus a histogram of "number of `NA` per row"
- [x] Summary table per variable: detected type, number of distinct values / categories, % missing, number of outliers, warnings
- [x] Let the user choose the **target variable** (the class to predict)
  - **by default, the target is the last column** of the file (common convention in UCI datasets)
  - the user can **change it** at any time (`selectInput`); the change updates the class distribution, the checks and the models
  - warn if the chosen target looks wrong: quantitative with many distinct values, only one class, or an identifier column
- [x] **"Validate and go to preparation" button**: refused if the target has an error; only validated data reach tab 2; status shows validated / not validated / changes not validated; the app then switches to tab 2

## Phase 2: Preprocessing (brief point 1)

> **Design: two kinds of operations** (to avoid data leakage)
> - **A. Cleaning** (safe on the whole dataset): drop columns / rows, duplicates, rows with many `NA`, merge columns, group rare categories, outliers → applied **in tab 2**, through the history
> - **B. Learned transformations** (they compute something from the data): imputation, normalization, dummification, rebalancing → **chosen** in tab 2, but **applied in Modelling**, fitted on train only, then applied to test
>
> **When to drop a column**: only when it brings nothing by nature (ID, constant, almost empty, duplicate) or by a **justified study choice** (noted in the history). Example: `Height` explains half of the variance of `Sex` in `students.csv` (η² = 0.51), so it must stay if the target is `Sex`. Feature importance (Phase 4) tells which variables really matter.

Each option can be switched on or off in the interface:

- [x] **Missing values**: first drop the rows/columns that are too empty (thresholds from Phase 1), then for the rest drop rows or fill in with the mean/median (quantitative) or the most frequent value (qualitative)
- [x] **Outliers**: detect them with the IQR rule (boxplot rule), then keep, remove or cap them
- [x] **Qualitative variables with many categories**: merge rare categories into "Other"
- [x] **Normalization**: none, z-score, or min-max
- [x] **Dummification**: one-hot encoding of qualitative variables
- [x] **Class imbalance**: show the class distribution, then offer oversampling, undersampling or class weights
- [x] **Manual drop**: the user can delete any columns they judge useless (`selectInput(multiple = TRUE)`), and rows (select them in the `DT` table, or by condition / by index)
- [x] **Merge columns** when needed:
  - quantitative: combine with an operation (sum, mean, difference, ratio) into a new column
  - qualitative: paste the values together (e.g. `A` + `B` → `A_B`)
  - the user names the new column and chooses whether to keep or drop the originals
- [x] Show the prepared dataset afterwards, with a summary of what was changed

> **Note: history of processing steps (undo)**
> Every operation (drop, merge, imputation, outliers, normalization, dummification, rebalancing…) is recorded in a **history list**:
> - each entry stores the **step name**, its **parameters**, and the **dataset before the step** (or enough to replay it)
> - the history is shown in the interface as an ordered list (step 1, step 2…)
> - an **"Undo"** button goes back one step, and the user can **return to any earlier step** (everything after it is removed)
> - a **"Reset"** button goes back to the raw data
> - in Shiny: a list inside `reactiveValues`, e.g. `rv$history <- list(list(step = "drop_cols", params = ..., data = ...))`
> - the same history can be reused to **describe the pipeline** in the case study
>
> **Done (2026-11-09)**: `R/historique.R` + `modules/mod_preparation.R`. Each step stores a **copy of the data** (simple and reliable for datasets of a few MB; for very large files, replaying the operations from `params` would be the alternative). History resets when new data are validated in tab 1.
>
> **Suggestions from tab 1 are reused**: alerts table, column drop list pre-filled (empty, constant, ID, too many NA, duplicate), buttons "drop the N rows with too many NA" / "drop the N duplicate rows" (counts recomputed on the current data, IDs ignored for duplicates), manual row selection in the table.

## Phase 3: Exploratory analysis (brief point 2)

Reuse what we saw in class (`cours1_unidimensionelle`, `Cours2_bidimensionel`).

The course scripts hard-code column names (EF/CA, Sex/Level). We make them work for **any variable the user picks**, using `selectInput`.

**One variable at a time** (course 1)
- [x] Statistics table: counts, cumulative counts, frequencies, cumulative frequencies
- [x] Quantitative: histogram (bin rule Sturges/Scott/FD, counts or density, density curve, quantile lines), boxplot + `rug`, empirical cumulative distribution (ECDF), centre and spread (min, max, mean, median, Q1, Q3, variance, standard deviation)
- [x] Qualitative: bar chart, pie chart, frequency table

**Pairs of variables** (course 2)
- [x] Quant × Quant: scatter plot, back-to-back histograms (`Hmisc::histbackback`), covariance, **Pearson correlation r**, regression line `lm`
- [x] Quant × Qual: boxplots side by side (base R + ggplot with jitter), **correlation ratio η** (between-group vs within-group variance)
- [x] Qual × Qual: contingency table, bar charts (stacked, side by side, profile plot with `position="fill"`), **χ², Φ², Cramér's V**
- [x] Optional: correlation matrix (heatmap) for all quantitative variables

## Phase 4: Modelling (brief point 2)

- [x] Choose the models (at least 2), for example **logistic regression** + **Random Forest** (a decision tree, SVM or k-NN could be a third)
- [x] Train/test split (adjustable ratio), and cross-validation if possible
- [x] **Model parameters set by the user** (one panel per model, with sensible defaults and short help texts):
  - common: train/test ratio, random seed (`set.seed`), **positive class**, **decision threshold** (default 0.5), stratified split on/off, number of folds if cross-validation
  - logistic regression: variables to include, interactions on/off, regularization (optional, `glmnet`)
  - Random Forest: `ntree` (number of trees), `mtry` (variables tried at each split), `nodesize` (minimum leaf size), `maxnodes`, class weights (`classwt`) for imbalance
  - any extra model (decision tree, k-NN, SVM): its main parameters (`cp` / `maxdepth`, `k`, `cost` / `kernel`)
  - optional: **"Auto-tune"** button that tries a small grid of values (e.g. `mtry`) by cross-validation and keeps the best
  - the chosen parameters are saved in the **model results history** (see note below)
- [x] Metrics for each model: **Precision, Recall, F-score**, confusion matrix
- [x] **ROC curve + AUC**, with the models plotted on the same graph
- [x] Table comparing the models side by side
- [x] **Feature importance** (Random Forest importance, logistic coefficients)
- [x] Handle multi-class targets (macro-averaged metrics, one-vs-rest ROC), or limit the app to binary targets and say so

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
| 2026-10-27 | Reviewed the demo `app.R`: keep functions + reactive chain, fix `?` loading, data leakage, positive class, stratified split, type detection. See the review section. |
| 2026-10-27 | Phase 4: user-set model parameters (common + per model, optional auto-tune). First `mod_donnees` written locally (`R/lecture.R`, `modules/mod_donnees.R`), not pushed yet. |
| 2026-10-27 | Target variable: last column by default, changeable by the user. |
| 2026-11-02 | Phase 1: type detection (`R/types.R`, with reasons + manual override) and content checks (`R/verifications.R`) done in `mod_donnees`. Checks only warn; dropping comes in Phase 2 with the undo history. Local, not pushed. |
| 2026-11-02 | Target variable: dropdown (last column by default), class table + bar chart, checks (one class, continuous, ID, NA, imbalance). `mod_donnees` now returns `list(data, cible)`. Local, not pushed. |
| 2026-11-02 | Summary table per variable (`R/resume.R`): role, type, distinct, % NA, outliers (IQR), short summary, alerts. **Phase 1 complete**. Pushed with the 5-tab skeleton; demo moved to `docs/demo_achraf.R`. |
| 2026-11-09 | Phase 2 started: processing history (`R/historique.R`: snapshot of the data at each step; undo / go back / reset) in `mod_preparation`, with column drop as first operation. History resets when tab 1 data change. Local, not pushed. |
| 2026-11-09 | Tab 1: "Valider et passer à la préparation" button. Refused if the target has an error; only validated data/target reach tab 2; status shows validated / not validated / changes not validated; app switches to tab 2 after validating. Local, not pushed. |
| 2026-11-09 | Tab 2 reuses tab 1 suggestions: alerts table, column drop list pre-filled (empty, constant, ID, too many NA, duplicate), buttons to drop rows with too many NA / duplicates (counts recomputed on current data, IDs ignored for duplicates). Manual row selection still to do. Local, not pushed. |
| 2026-11-09 | Manual row drop: click rows in the tab 2 table (`input$apercu_rows_selected`), delete through the history; detail keeps the original row numbers. Manual drop item done (drop by condition not done). Local, not pushed. |
| 2026-11-10 | **Phase 2 complete**: added IQR outliers (keep/cap/drop), rare categories grouping into "Autre", column merging (sum/mean/diff/ratio/concat) in `R/nettoyage.R`, and learned transformations config + train-only fit/apply in `R/transformations.R` + `modules/mod_preparation.R`. |
| 2026-11-10 | **Phase 4 complete**: created `R/modelisation.R` (stratified split, zero-leakage train/test preprocessing, logistic regression + Random Forest with auto-tune, metrics, ROC, feature importance, run history) and `modules/mod_modelisation.R`, connected in `app.R`. |
| 2026-11-10 | **Phase 3 complete**: created `R/exploration.R` and `modules/mod_exploration.R` (Course 1 univariate tables/plots + Course 2 bivariate Quant×Quant, Quant×Qual, Qual×Qual + correlation heatmap), connected in `app.R`. |
