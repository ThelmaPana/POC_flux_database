# POC_flux_database

Collect POC flux data from various model outputs and compare them.

## Repo organisation

### Data

For data. Directory `raw` contains raw data inputs.

### Scripts

-   `00.read_and_format_model_outputs`: Read and process POC flux model outputs from Doney at al., 2024

-   `01.read_and_format_obs`: Read and process POC flux observations.

-   `02.analyse models`: Analyse and plot model outputs.

-   `03.compare_mod_obs`: Compare POC fluxes from models and from observations.
