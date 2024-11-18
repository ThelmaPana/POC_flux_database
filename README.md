# POC_flux_database

Collect POC flux data from various model outputs and compare them.

## Repo organisation

### Data

For data. Directory `raw` contains raw data inputs.

### Scripts

-   `00.read_and_format_obs-based`: Read and process POC flux from observation based models listed in Doney et al., 2024

-   `00.read_and_format_model_outputs`: Read and process POC from RECCAP2 models

-   `02.read_and_format_obs`: Read and process POC flux observations.

-   `03.analyse models`: Analyse and plot model outputs.

-   `04.compare_mod_obs`: Compare POC fluxes from models and from observations.
