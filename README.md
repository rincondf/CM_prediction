This repository contents the code associated with prediction of codling moth population dynamics from phenology models and field data.

The codling moth capture dataset used in this study is available from the Okanagan-Kootenay Sterile Insect Release Board (https://www.oksir.org/) upon reasonable request to sirinfo@oksir.org.

The provided folder should contain the following files:

1. "meansCM.csv"
2. "varCM.csv"
3. "DDs.csv"
4. "prp_DDs.csv"
5. "DDsC.csv"
6. "prp_DDsC.csv"
7. "cumprp_DDsC.csv"

"Var-mean_model.R" is the procedure used for Taylor's power Law parameter estimation.
"Parameter_estimation.R" includes the functions used for parameter estimation of JohnsonSB distributions and distinction of generations with mixture models
"Model_test.R" describes the procedure to test the models through simulations.
"Moth_Capture_Celsius.R" is the moth capture model in C
"Moth_Capture_Fahrenheit.R" is the moth capture model in F
"Phenology_Celsius.R" is the phenology-based model in C
"Phenology_Fahrenheit.R" is the phenology-based model in F
"Convert.R" contents handy and required functions to convert from Celsius degree-days to Fahrenheit and vice versa