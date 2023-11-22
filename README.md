# Climate and Inequality
Are countries with higher inequality more adversely affected by climate change?

A simple R project exploring whether countries with higher inequality, measured through the Gini Index, are more adversely affected by natural disasters. The dependent variable 
is the death toll resulting from disasters like floods and extreme temperature, among others. The independent variables include the Gini index, the share agriculture contributes to GDP, 
and public spending. 

Data for natural disaster mortality is taken from EM-DAT, while data for all the other variables is taken from the World Bank. 

All data went through a tidying process using the tidyverse package, and a simple linear regression was run. 
