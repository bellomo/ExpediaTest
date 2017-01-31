# ExpediaTest
R code for the Expedia test

test.R
------

It reads the CVS file and processes the data to check for similar partners comparing the booking value distributions and checking compatibility at stochastic level using the Wilcoxon-Mann-Whitney test. The similary matrix produced by the test is saved on disk, it can be reloaded for repeated processing setting the flag "loadSim=1". A few plots are saved showing market sharing and similary matrix.

The code procudes a JSON file formatted as requested in the test, listing all partners and for each partner the top 10 markets and the top 10 most similar parterns. JSON file writing is controlled by a flag "writeJSON".

plot.R
------

It reads the CVS data and the JSON file produced by test.R to make plots and tables.

myjson.json
-----------

JSON file requested in the test

The most similar partner to "de1201d4d1b6" is found to be "t35tde1201"

This data is used in to quickly inspect for each partner the top markets and the similar partners.
