# Dorheim et al. GMD H2 Manuscript

Materials, scripts and data used to prepare the H2 Hector GMD manuscript for submission to GMD.

Brief description of the repository contents 
     * data: a combination of raw and intermediate data products 
     * inputs: hector input materials, ini and input csv files required for hector
     * rslts: where the figures and the results text is written out to 
          * figures are currently numbered by how they appear in the manuscript 
          * rslts_DATE.txt will contain the results 
     * scripts: the collection of scripts that run the stand alone hector simulations, run analyses, and make figures for the manuscripts
     * session_info.txt: info about the packages and general environment 
     

To launch all the manuscript scripts source `scripts/run_all.R`, users can launch 
the different scripts one at a time if they would like so long as the previous 
level (indicated with the number prefix) has been run already. 

| Name        | Description                                    |
|------------:|-----------------------------------------------:|
|0.env_fxns.R |Load the required packages and common helpful functions.       |
|1.inputs.R   |Process the HyED emissions for use in Hector | 
|2.GWP.R      |Launch the hector runs to calculate H2 GWP100  |
|2.historical.R |Run the historical hector simulations with and without H2 chemistry, process results and make figures |
