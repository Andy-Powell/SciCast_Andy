# SciCast_Andy
Containts Andy's revisions to Ken's cleaning and analysis scripts, esp:
 * Scripts for weekly status reports
 * Analysis scripts for the Y4 Accuracy Incentives Experiment 
Forked the project from Andy-Powell/SciCast_Andy. 

Andy has refactored reused code into separate files, and provides this call
dependency, as of 19-MAR-2015:

The main script and support scripts that do the A/B Brier Score calculations and barchart are:
 * Incentive Experiment Accuracy 150210C1.R, which calls:
   * Get_Data_150203lb.R
   * Incentive Selection 150312.R
   * De-Stuttering A.R
   * Incentive Accuracy Active 141209.R
   * Incentive Accuracy Control 141209B.R

The main scripts and supporting scripts that generate the monthly average Brier scores by question are:
 * Incentive Experiment Trade Brier Generation 150225A.R, which calls:
    * Get_Data_150203lb.R
    * Incentive Selection 150312.R
    * General Data Prep 150304B.R, which calls:
      * Incentive Selection 150312.R  <** Wait, this was already called!
    * #[commented: Incentive Overall Trade Selection 150212.R ]
    * De-Stuttering A.R
    * Incentive Experiment Brier Data 150225.R, which calls:
      * Incentive Accuracy Mechanics Basic 150221.R
    * #[commented: Brier Score vs Choices graph 150304.R]
 * Monthly Time-Weighted Brier Scores by Question 150311A, which calls:
    * Get_Data_150203lb.R
    * General Data Prep 150304B.R
      * ...
    * Incentive Accuracy Mechanics Normal 150221.R

The weekly Accuracy and uniform average scores and graphs:
  * Accuracy and Uniform 150304B  Reset.R
    * Reset_Data_150307.R
    * Incentive Selection 159312.R
    * De-Stuttering A.R
    * Accuracy Overall 141209.R
    * Selecting Question Max Choices.R
