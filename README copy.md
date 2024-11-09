# Small Sample Size - BioSimilarity Comparison Statistics  

For n < 12 - disucssion of how to do set comparisons, with Biosimilarity in mind. 

## Overview

This project provides an approach for biosimilarity assessment when dealing with Investigational New Drug (IND) dossiers, particularly in the case of small analytical sample sizes. The method emphasizes non-parametric techniques that don't rely on assumptions of underlying normality. It also presents the use of rank-based statistics as a robust alternative when sample sizes are small, or normality can't be assumed, which is often the case in early clinical development stages.

## Why is this Important?

Biosimilarity assessment plays a crucial role in determining whether a biosimilar product is sufficiently similar to an originator product, ensuring therapeutic efficacy and safety without conducting extensive clinical trials. Traditional parametric methods assume larger sample sizes and normal distributions, which are often not feasible in early-stage IND submissions. The non-parametric methods used here allow for robust assessments under more restrictive data conditions, ensuring regulatory standards can still be met effectively.

## Key Techniques and Insights

1. Non-parametric methods: These methods are applicable when normality assumptions can't be made, offering a flexible and robust approach to biosimilarity assessments for small sample sizes.

  * Utilizes rank-based statistics and tests like the Mann-Whitney U Test to compare sample distributions.
  * Bootstrap techniques are also employed to estimate variances and confidence intervals, especially for small datasets.

2. Practical Example: The RMarkdown file provides examples of ranked-based statistical analysis, comparing small sample sets (n = 8 and n = 3) using the Mann-Whitney U Test. This approach helps determine whether two small sample sets (reference and test) exhibit significant differences in biosimilarity.

3. Applications:
The methodology is applied to HLX02 (trastuzumab), a biosimilar drug, in a tiered approach following NMPA and USFDA guidelines.
Key attributes are assessed based on their impact on clinical outcomes using both parametric and non-parametric methods.

## Usage
The R scripts provided in this repository can be used to:

* Analyze biosimilarity of small sample sizes.
* Implement rank-based tests such as the Mann-Whitney U Test to compare analytical quality attributes (CQAs).
* Generate bootstrap samples for assessing variability without relying on normality assumptions.

## Conclusion

The analysis framework proposed in this project is crucial for biosimilarity assessments in scenarios where traditional parametric methods are insufficient. By leveraging non-parametric statistics, the approach ensures robust, regulatory-compliant evaluations of biosimilarity for IND dossiers with small analytical sample sizes.

## Source and environment information
packageVersion("rmarkdown") ‘2.22 \
R version 4.2.2 (2022-10-31) \
RStudio.Version()$version ‘2023.6.0.421’

