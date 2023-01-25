---------------------------------------
README
---------------------------------------

# Are Many Sets of Eyes Better Than One? Evaluating Multiple Databases of Armed Actors in Colombia.

**Javier Osorio**

**University of Arizona**

email: josorio1@arizona.edu

<br>
<br>

# DESCRIPTION 

These supplementary materials present the replication of files of the article "Are Many Sets of Eyes Better Than One?".

<br>

# REQUIREMENTS

The replication materials run in a computational environment with the following characteristics:  

* R version: 4.1.0 (or later)
* Operating System: Windows 10 x64 (also runs in camOS or Ubuntu)
* Platform: 64-bit
* Cores: 10


The script requires the following packages:
* `assertthat_0.2.1`
* `base` 
* `cli_3.0.0` 
* `colorspace_2.0-2` 
* `compiler_4.1.0` 
* `crayon_1.4.1` 
* `datasets` 
* `DBI_1.1.1` 
* `dplyr_1.0.7` 
* `ellipsis_0.3.2` 
* `fansi_0.5.0` 
* `generics_0.1.0` 
* `ggplot2_3.3.5` 
* `glue_1.4.2` 
* `grDevices` 
* `graphics` 
* `grid_4.1.0` 
* `gtable_0.3.0` 
* `lifecycle_1.0.0` 
* `magrittr_2.0.1` 
* `methods` 
* `munsell_0.5.0` 
* `pacman_0.5.1` 
* `pillar_1.6.1` 
* `pkgconfig_2.0.3` 
* `purrr_0.3.4` 
* `R6_2.5.0`
* `rlang_0.4.11` 
* `rstudioapi_0.13` 
* `scales_1.1.1` 
* `stats` 
* `tibble_3.1.2` 
* `tidyselect_1.1.1` 
* `tools_4.1.0` 
* `utf8_1.2.1` 
* `utils` 
* `vctrs_0.3.8` 

<br>

# FOLDER STRUCTURE

The replications materialas include the following folders:
* `data_final` 
* `graphs`
* `scripts`
* `tables`

<br>

# HOW TO RUN THE PROJECT

1. Open the `osorio_similarity_rep.Rproj` R project file.
2. Go to `scripts` folder and run the `script similarity_fv.R` script.
3. The scrip is structured according to the following sections and should be executed progressively:

* `0. SETUP`
* `1. SPIDER WEB PLOT`
* `2. GET THE DATA`
* `3. ASSESS COVERAGE`
* `4. EXPLORE MISSIGNESS` 
* `5. EXPLORE JACARD AND SORENSEN`
* `6. JACCARD INDIEX OVER YEARS FOR GUERRILLA AND PARAMILITARIES`
* `7. VISUALIZE JACCARD OVER YEARS FOR GUERRILLA AND PARAMILITARIES`
* `8. AVERAGE JACCARD SIMILARITY GUERRILLA AND PARAMILITARIES` 
* `10. LOCAL JACCARD SIMILARITY PER ARMED GROUP` 
* `11. AVERAGE JACCARD SIMILARITY PER ARMED GROUP OVER TIME`
* `12. STATISTICAL ANALYSIS OF HOMICIDES` 
* `13. REPLICATE DUBE AND VARGAS WITH DIFFERENT MEASURES` 
* `14. JACCARD FOR EACH PAIR OF MEASUREMENT SETS`

<br>

# LICENSE

MIT License

Copyright (c) 2023 Javier Osorio

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

---------------------------------------
