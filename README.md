Final project
================
Group 1
10/5-2021

## Description

This project does different R analysis on the heart data set:
<https://archive.ics.uci.edu/ml/datasets/Heart+Disease?fbclid=IwAR3SbR6Eo3ow8dYal4mTv321XOUAUg8yw5g_WY-eDz9hUQS_0jJ0-qTR9v8>
The data set was pulled 27. April 2021 by our group. This data set is
collected from people that have suspected heart issues in Long Beach,
Hungary, Cleveland and Switzerland. A total of 920 patients and 14
variables with location information was used.

Datasets from all four countries are included in our analysis. Before
using the data, our R script joins all the datasets cleans it, and
augments it using tidyverse methods (scripts 02\_clean.R and
03\_augment.R). This data is used in different analysis that creates
plots which are converted into png files and loaded into an R markdown
IO slides presentation.

## Data

Attribute Information:

1.  The age of the participates in the study in years

2.  The gender of the participates

    -   0 = female

    -   1 = male

3.  The type of chest pain

    -   1 = typical angina

    -   2 = atypical angina

    -   3 = non-anginal pain

    -   4 = asymptomatic

4.  The resting blood pressure in mm Hg on admission to the hospital

5.  The serum cholestoral in $\\frac{mg}{dl}$

6.  The fasting blood sugar

    -   0 = fasting blood sugar $&lt; 120 \\frac{mg}{dl}$

    -   1 = fasting blood sugar $&gt; 120 \\frac{mg}{dl}$

7.  The result of the resting electrocardiographic

    -   0 = normal

    -   1 = ST and/or T wave abnormality

    -   2 = showing probable or definite left ventricular hypertrophy

8.  The maximum heart rate achieved

9.  Exercise induced angina

    -   1 = yes

    -   0 = no

10. ST depression induced by exercise relative to resting stage

11. The slope of the peak exercise ST segment

    -   1 = upsloping

    -   2 = flat

    -   3 = downsloping

12. The number of major vessels colored by flourosopy

    -   0 ≤ *M**a**j**o**r* *v**e**s**s**e**l**s* ≤ 3

13. The defect type of thalassemia

    -   3 = normal

    -   6 = fixed defect

    -   7 = reversable defect

14. The diagnosis of heart disease

    -   0 = no heart disease present

    -   \[1;4\] = types of heart disease

## Installation

In order to get the all the project files clone the following repository
from git: git clone
<https://github.com/rforbiodatascience21/2021_group01_final_project>

## Software:

The following software needs to be installed for running the code
locally:

-   R

For running non-local, an Rstudio Cloud session can be used:
<https://rstudio.cloud/>

## Requirements

The following packages needs to be installed inside R for the scripts to
function:

-   library(tidyverse)
-   library(ggplot2)
-   library(ggpubr)
-   library(broom)
-   library(cowplot)
-   library(GGally)
-   library(rlang)
-   library(viridis)

## Usage

Make sure that in an R session, the project files are all cloned and
every requirement is installed. Run the 00\_doit.R script found under
the “R” folder. This will run all necessary scripts to make the raw data
into png files and generate the presentation document. The presentation
document is found in the “doc” folder named
“Presentation\_IOslides.html”.
