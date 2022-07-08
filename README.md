# ethical-investing
R markdown and R script analyzing an investment database to examine ethical investing

This script explores the question of whether ethical investments lead
 to better returns based on ESG scores

Here I will open and explore two .csv files:
  1) Funds_info.csv contains basic information about EFTs and mutual funds
  2) Funds_ESG_ratings.csv contains the corresponding ESG rating and their
    risk of exposure to a list of controversial topics.

Addressing questions such as: 
 1) Do ethical investments lead to better returns?
 2) Can we actually predict returns based on these ratings?
 3) What do these ESG scores reflect in terms of the fund's involvement in certain
  controversial issues?

The script is split into three main sections:
 1) Data Exploration
 2) Modelling via linear regression, linear regression with penalty and ridge penalty, and random forest
 3) Discussion

The data exploration section examines the structure
 of the data and explores the relationships between the variables.
 Ideas of which variables to model will come from this data exploration

The models sections aims to address the questions of whether 
 there is a relationship between the different ESG scores
 and the fund returns. 

 It also then goes on to test whether you can predict the returns
 based off of the scores

 The discussion section goes into a discussion of the results
