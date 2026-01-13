This repository hosts a suite of Shiny web applications for the Soil Security Assessment Framework (SSAF) Self-Evaluation Connectivity Tool. The tool is designed to assist different user groups (farmers, policymakers, land managers) in assessing soil security.

For more information see Pachon, J. C., Leonard, E., Field, D., McRobert, K., Heath, R., & McBratney, A. (Under Review). Quantifying the Connectivity Dimension of the Soil Security Assessment Framework. Soil Security Journal.

## Project Structure

-   `/farmers`: Shiny app for farmers.
-   `/policymakers`: Shiny app for policymakers.
-   `/landmanagers`: Placeholder for the land managers app.
-   `/index`: Main landing page app.
-   `utils.R`: Shared utility functions for database connections, translations, etc.
-   `shared_app_components.R`: Shared UI and server logic for the Shiny apps.
-   `global.R`: Global objects and settings for the Shiny apps.
-   `country_data/`: Folder containing country-specific data for the map functionality.
-   `renv.lock`: Captures the exact versions of R packages used, allowing for an exact replication of the environment.

## Offline Mode and Database Configuration

The applications can be run in "online" or "offline" mode. The mode is set in each application's `app.R` file.

-   In **online mode**, the apps connect to a PostgreSQL database.
-   In **offline mode**, they use a local SQLite database. The database file, `TestDatabase.sqlite`, is not included in this repository and will be created automatically in the project's root directory when an application is run in offline mode for the first time.

## Database Tables

The applications use two main tables:

-   `farmer_responses`: Stores raw data and scores from the farmers app.
-   `policymakers_responses`: Stores raw data and scores from the policymakers app.

## Setup and Replication Instructions

1.  **Install renv** (if not already installed):
    ```R
    install.packages("renv")
    ```

2.  **Restore the R environment**:
    ```R
    renv::restore()
    ```

3.  **Run the Shiny apps**: Each application can be run individually by pointing the Shiny server to the respective app directory (e.g., `/srv/shiny-server/farmers`).

## Dependencies

-   **R Version**: 4.4.1
-   **R Packages**: The R package dependencies are managed by `renv`, as specified in the `renv.lock` file. Key packages include `shiny`, `DBI`, `RPostgres`, `RSQLite`, `dplyr`, `leaflet`, and `readxl`.
