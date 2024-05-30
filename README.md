This repository contains the code associated with prediction of codling moth population dynamics from phenology models and field data (Ecol. Modell. 493:110763, 10.1016/j.ecolmodel.2024.110763).

The codling moth capture data set used in this study is available from the Okanagan-Kootenay Sterile Insect Release Board (https://www.oksir.org/) upon reasonable request to sirinfo@oksir.org.

The provided folder should contain the following files:

1. "meansCM.csv"
2. "varCM.csv"
3. "DDs.csv"
4. "prp_DDs.csv"
5. "DDsC.csv"
6. "prp_DDsC.csv"
7. "cumprp_DDsC.csv"
8. "Validation_data.RData"

The data files should be saved directly in the folder "Data" and follow the directions in the code's annotations.

The folder "Analyses" contains the code required to run parameter estimation and model sensitivity and validation analyses. "Var-mean_model.R" is the procedure used for Taylor's power Law parameter estimation, "Parameter_estimation.R" includes the functions used for parameter estimation of JohnsonSB distributions and distinction of generations with mixture models, long- and short-term validation is in "Validation_LongTerm.R" and "Validation_ShortTerm.R", respectively, and "Validation_means.R" contains the code for the concordance correlation analysis.

The folder "Models" contains several versions of the models: "Moth_Capture_Celsius.R" is the moth capture model in C, "Moth_Capture_Fahrenheit.R" is the moth capture model in F, "Phenology_Celsius.R" is the phenology-based model in C, and "Phenology_Fahrenheit.R" is the phenology-based model in F

"Model_test.R" describes the procedure to test the models through simulations, "Functions.R" contains all required supporting functions, and "Figures.R" produces the figures in the manuscript.

Both the phenology-based model and the moth-capture model are also available as shiny apps:

- Phenology-based model in Fahrenheit: https://diego-rincon.shinyapps.io/draft2/.
- Moth-capture model in Fahrenheit: https://diego-rincon.shinyapps.io/draft_app/.