# **Project Contributors**

-   Student ID: s205869, GitHub username: BachBerg

-   Student ID: s235342, GitHub username: isab4635

-   Student ID: s244854, GitHub username: karo3420

-   Student ID: s245618, GitHub username: malaikakhiljee

# **Link to presentation**

[See presentation](https://teaching.healthtech.dtu.dk/22100/rstudio1/files/projects/Group-30-Final-Project/doc/presentation.html#/title-slide)

# **Data retrieval**

To access and download the biospecimen data used in this project, follow the steps below.

1.  **Request Data Access**

    Apply for access to the PPMI dataset by following the instructions provided here:\
    <https://www.ppmi-info.org/access-data-specimens/download-data>\

2.  **Log In to the Data Portal**\
    Once your access request has been approved, log in to the PPMI data portal:

    <https://ida.loni.usc.edu/login.jsp?project=PPMI>\

3.  **Navigate to the Biospecimen Data**\
    After logging in:

    1.  In the top navigation bar, select **Download → Study Data**.

    2.  In the left-hand panel, navigate to **Biospecimen → Biospecimen Analysis**.

    3.  Click on "**Current Biospecimen Analysis Results**" to download the raw data file (CSV format).

4.  **Store the Data**\
    Make sure there is a folder called `_raw` in the folder `data`. Place the downloaded `.csv` file into the `_raw` directory.

# Rendering the project

After you have rendered the project (or any qmd file); (e.g. `quarto render 00_all.qmd`), you need to run the R-script cleanup.R in the terminal to make sure that the HTML files is in the right directory.

Example (in terminal):

`quarto render 00_all.qmd`

`Rscript cleanup.R`
