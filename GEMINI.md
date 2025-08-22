# Project Overview: SSAF Self-Evaluation Connectivity Tool

This document provides an overview of the SSAF Self-Evaluation Connectivity Tool project, its current state, and a roadmap for future development.

## Project Description

The SSAF Self-Evaluation Connectivity Tool is a suite of Shiny web applications designed to assess soil security based on the Soil Security Assessment Framework (SSAF). The tool is targeted at different user groups, including farmers, policymakers, and land managers.

The project is hosted on a DigitalOcean droplet and consists of the following key components:

*   **Shiny Applications:** The project includes several Shiny applications, each tailored to a specific audience:
    *   `farmers/`: A complete and live application for farmers.
    *   `policymakers/`: A complete and live application for policymakers.
    *   `index/`: The main landing page for the tool.
    *   `landmanagers/`: A placeholder for a future application.
*   **Shared Codebase:** The project utilizes a shared codebase in `utils.R`, `shared_app_components.R`, and `global.R` to manage common functionalities like database connections, translations, and UI components.
*   **Database Integration:** The applications connect to a PostgreSQL database in the production environment and can use a local SQLite database for offline development and testing.
*   **Multilingual Support:** The tool is designed to be multilingual, with a translation system that loads localized strings from `.rds` or `.xlsx` files.

## My Purpose & Expertise

My primary purpose is to act as a developer to complete and improve the SSAF Self-Evaluation Connectivity Tool. My role extends beyond debugging to include full application creation and deployment.

My expertise includes:
*   **R Shiny Applications:** Development, debugging, and optimization of interactive web applications.
*   **DigitalOcean Deployment:** Managing and deploying applications on DigitalOcean droplets.
*   **PostgreSQL Integration:** Connecting applications to and interacting with PostgreSQL databases.

### Recent Development Updates

Since the last update, the following significant changes and improvements have been implemented:

*   **Code Refactoring and Abstraction:**
    *   Introduced `shared_app_components.R` to centralize common UI elements (e.g., `tags$head`, `top-bar`, `footer`) and server-side logic (session management, progress bar).
    *   Refactored `farmers/app.R` and `policymakers/app.R` to utilize `shared_app_components.R`, significantly reducing code duplication and improving maintainability.
*   **Bug Fixes and UI Enhancements:**
    *   Fixed a bug where translation keys were displayed instead of the translated text.
    *   Resolved application crashes in the `farmers` and `policymakers` apps.

## Application Details

### Farmers Application

*   **Location:** `farmers/`
*   **Main File:** `app.R`
*   **Scoring Logic:** `farmersScoring.R`
*   **Translations:** `Farmers_translations.rds`
*   **Database Table:** `farmer_responses`
*   **Functionality:** This application guides farmers through a multi-page survey to assess their soil security. It includes a map to select their location, a series of questions about their practices and perceptions, and a final scoring page that provides feedback on their soil security.

### Policymakers Application

*   **Location:** `policymakers/`
*   **Main File:** `app.R`
*   **Scoring Logic:** `PolicymakersScoring.R`
*   **Translations:** `Policymakers_translations_new.xlsx`
*   **Database Table:** `policymakers_responses`
*   **Functionality:** This application is similar to the farmers' app but is tailored to policymakers. It includes additional questions about policy and governance.

## Shared Codebase

*   **`utils.R`:** This file contains a collection of utility functions used by the applications.
*   **`shared_app_components.R`:** This file contains shared UI and server logic.
*   **`global.R`:** This file loads the `all_gadm41_centroids_level2.rds` file, which contains the centroids of the communes for the map functionality. It also sets some global Shiny options for error handling and logging.

## Operational Guidelines

### Version Control & Reverting Changes

*   **Git as Primary Tool:** The project utilizes Git for version control. All significant changes should be tracked and committed.
*   **Atomic Commits:** Make small, focused commits that address a single logical change. This facilitates easier review and reversion.
*   **Review Changes:** Always use `git status` to see modified files and `git diff` to review changes before committing.
*   **Reverting Changes:** To undo a committed change, use `git revert <commit-hash>`. This creates a new commit that undoes the specified commit, preserving history. For uncommitted changes, `git restore <file>` can discard modifications.
*   **Complex String Replacements (Read-Modify-Write):** When `replace` tool fails repeatedly on multi-line strings or strings with complex escaping (e.g., nested quotes, backslashes, newlines), switch to a "read-modify-write" approach:
    1.  Read the entire file content into memory using `read_file`.
    2.  Perform the string replacement directly on the in-memory content using standard string manipulation.
    3.  Write the modified content back to the file using `write_file`.
    This approach bypasses the strict matching requirements of `replace` for highly complex patterns and ensures changes are still trackable.

### Communication & Commenting on Updates

*   **Concise Updates:** Keep responses and comments concise and to the point.
*   **Focus on Purpose:** When making changes, explain *why* a change was made, rather than just *what* was changed, unless the "what" is complex and requires specific clarification.
*   **Commit Messages for Detail:** Detailed explanations of code changes belong in Git commit messages. My responses will focus on the current status, next steps, and high-level rationale.
*   **Tool Usage Clarity:** Tool calls are self-explanatory. Avoid adding redundant explanatory comments within tool calls or code blocks unless specifically part of the required code itself.