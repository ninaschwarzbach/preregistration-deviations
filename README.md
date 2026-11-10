# Deviating from Preregistration: Does It Hurt Researchers' Credibility?

Analysis code for the paper *Deviating from Preregistration: Does It Hurt Researchers' Credibility?* (link to the paper will follow after publication).

We surveyed 994 researchers about their experiences with and perceptions of deviations from preregistration, and asked them to rate the credibility of a study and the trustworthiness of its author in one of four scenarios (2 × 2 between-subjects design: preregistered or not × deviation reported or not). All analyses are descriptive and exploratory.

Materials, the preregistration, and the data are available on OSF: https://osf.io/sxhr7/

## Contents

```
preregistration-deviations/
├── analysis.R                        # all analyses, tables and figures
├── data_cleaning.R                   # creates the OSF data file from the raw data
├── preregistration-deviations.Rproj  # RStudio project file
├── data/
│   └── Data_PreregDevation_OSF.txt   # survey data (also on OSF)
└── figures/                          # figures are saved here by analysis.R
```

## How to run the code

1. Install [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/).
2. Install the required packages (only needed once). In the RStudio console, run:
   ```r
   install.packages(c("tidyverse", "ggstats", "rstudioapi"))
   ```
3. Open `preregistration-deviations.Rproj`, then open `analysis.R` and click **Source**.

The results are printed in the console, and the figures are saved in the `figures/` folder.

## Where to find what

| Section in `analysis.R` | Manuscript |
|---|---|
| 1. Sample description | Methods, Demographic information |
| 2. Expected consequences of reporting deviations | Table 1 |
| 3. Concerns about professional reputation | Figure 1 |
| 4. Perceived credibility and trustworthiness | Figure 2, Supplementary Table 1 |
| 5. Evaluation of the deviation | Table 2, Supplementary Table 3 |
| 6. Factors considered when assessing credibility and trustworthiness | Figure 3, Supplementary Figure 1 |
| 7. Perceived prevalence of deviations | Table 3 |
| 8. Cultural norms | Figure 4, Supplementary Table 2 |

## Notes

- `data_cleaning.R` shows how the shared data file was created from the raw survey export. Participants who completed less than 98% of the survey or did not give consent were excluded. Participants with 98% answered all questions but did not click the final submit button. The raw data are not shared but are available upon request.
- Open-text responses are not shared to protect participants' privacy. The code that uses them is commented out. These data are available upon request.

## License

The code is released under the MIT License (see `LICENSE`).
