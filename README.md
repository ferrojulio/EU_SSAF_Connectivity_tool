
# SSAF Self-Evaluation Connectivity Tool



This repository hosts a Shiny web application for the Connectivity Self Evaluation Tool. The application is designed to assist with specific evaluations based on provided data and dependencies. 

For more information see Pachon, J. C., Leonard, E., Field, D., McRobert, K., Heath, R., & McBratney, A. (Under Review). Quantifying the Connectivity Dimension of the Soil Security Assessment Framework. Soil Security Journal.


## Project Structure

- **app.R**: Main application file for the Shiny app.
- **www/**: Folder containing supporting files, including FAQ, images and styles.
  - **header.png**: Header image displayed at the top of the Application.
- **202308_SS_Connectivity_EvalTool_StrataInfo2.csv**: CSV file with necessary input data.
- **Dockerfile**: Defines the Docker container for replicable deployment.
- **renv.lock**: Captures the exact versions of R packages used, allowing for an exact replication of the environment.
- **farminstitute-soilconnectivity.Rproj**: RStudio project file, which helps set up the correct working environment automatically when opened.
- **TestDatabase.sqlite**: Local SQLite database file used in offline mode (automatically generated if not present when running web application).

## Offline Mode and Database Configuration
This application was originally containerized using a Dockerfile and deployed on Heroku. For local usage, this application includes an offline mode for users without a remote SQL server. When running in offline mode, the app creates and uses an SQLite database file (TestDatabase.sqlite) stored locally in the project directory.

Enabling Offline Mode: The application is set to offline mode by default, indicated by mode="offline" in app.R. In this mode, the app bypasses the need for an external SQL connection, instead relying on the local SQLite database.
SQLite File Path: The SQLite database path is set to ./TestDatabase.sqlite, relative to the project root. This file will be created automatically if it does not already exist.

## Database Tables
The application creates and manages two primary tables within the SQL databases, each serving a distinct purpose in the data flow: Evaltool_data for collecting raw user input and EvalTool_Score for storing validated and quantified scores.

1. Evaltool_data Table: User Input Data
The Evaltool_data table holds the raw input data from users. This table captures initial responses and data entries from various sections of the app, including demographic, geographic, and environmental information. Key columns include:

	User and Session Information:
		session_token: Unique identifier for each session.
		start_time, End_time, Submission_time: Timestamps for session start, end, and submission.

	Demographics:

	Geographic Data:

Connectivity Indicator data, see Pachon et al., (under review) Table S2:

2. EvalTool_Score Table: Quantified and Validated Scores
The EvalTool_Score table stores quantified scores derived from user input after applying validation checks and adjustments (see Pachon et al., (under review) Figure2). May be connected to Evaltool_data Table via session_token.

## Setup and Replication Instructions

You have two main options for setting up this app: using Docker or restoring the R environment via `renv`.

### Option 1: Running with Docker

1. **Build the Docker image**:
   ```bash
   docker build -t connectivity_eval_tool .
   ```

2. **Run the Docker container**:
   ```bash
   docker run -p 3838:3838 connectivity_eval_tool
   ```
   - The Shiny app should now be accessible at `http://localhost:3838`.

This setup installs dependencies based on the `renv.lock` file, creating a consistent environment for replication.

### Option 2: Running with `renv` in an R Environment

1. **Open the Project**
   For best results, open the project by double-clicking on farminstitute-soilconnectivity.Rproj. This will set up the working directory automatically in RStudio.

2. **Install `renv` if not already installed**:
   ```r
   install.packages("renv")
   ```

2. **Restore the environment**:
   With the project open in RStudio, run:
   ```r
   renv::restore()
   ```

3. **Run the Shiny app**:
   After restoring the environment, open `app.R` in the project and click "Run App" or run:
   ```r
   shiny::runApp("app.R")
   ```

### Data File

The app requires `202308_SS_Connectivity_EvalTool_StrataInfo2.csv`, which is already included in the project root. This file contains necessary data for the map that users get after putting their postal code and should not be moved. This only works for Australia.

## Dependencies

- **R Version**: 4.3.2
- **System Dependencies**: The Dockerfile installs necessary system libraries (`libmysqlclient-dev`, `libudunits2-dev`, `libgdal-dev`) required by some R packages.
- **R Packages**: The R package dependencies are managed by `renv`, as specified in the `renv.lock` file.

## Additional Notes

- The Docker setup relies on `renv` to install R packages, ensuring a controlled environment.
- **Expose Port**: Ensure that port 3838 is available on your host machine for Docker to serve the app.
  
## License


This work is licensed under the MIT License.


## Citation
to be updated once DOI is obtained for version 1.0.
