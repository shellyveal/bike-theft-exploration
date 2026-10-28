# Bicycle Theft Seasonality in the West Midlands

An exploratory analysis of recorded bicycle thefts in the West Midlands, England, using two complementary workflows: one in **R** and one in **Python**. Both projects draw on the same underlying datasets to investigate seasonal patterns and consider how those patterns can inform projections.

## Overview

Bicycle theft is a practical problem for cyclists, local communities, and the agencies responsible for responding to crime. Understanding when thefts are reported most often can help describe how the problem changes over the calendar year and provide a starting point for examining whether those patterns recur.

This repository approaches the question through two separate analyses rather than a single combined script. The R and Python projects use the same broad data foundation, but keep their implementations distinct. That makes the repository useful both as an investigation of bicycle-theft seasonality and as a comparison of analytical workflows across two common data-science environments.

The project is concerned with **recorded bicycle thefts in the West Midlands, England**. It explores seasonality and projection; it is not a live crime-monitoring service, a guarantee of an individual's risk of theft, or a direct causal identification study.

## Questions motivating the analysis

A handful of guiding questions for the exploration:

- How are recorded bicycle thefts distributed over the calendar year?
- What trends, if any, converge between seasonal changes in weather and unemployment and the seasonality of bicycle theft?
- How can historical patterns be summarized and used to develop projections?
- What can recorded crime data tell us—and what remains uncertain because the data capture reported incidents rather than every theft that occurs?


## The two projects

### 1. [R analysis](notebooks/forecasting-script.R)

The R project examines the bicycle-theft data with the main goal of identifying ideal forecasting methodologies for predicting bicycle theft in the West Midlands. View the report summarizing results [here](reports/forecasting-report.pdf).

### 2. [Python analysis](notebooks/stats-analysis-script.ipynb)

The Python project further investigates the relationship between varying weather and economic factors and reported bicycle thefts in the West Midlands, employing varied computational methods to isolate main associations. View the report summarizing Python results [here](reports/report.pdf). 

## Repository structure

```text
bike-theft-exploration/
├── notebooks/       # Analysis notebooks and project workflows
├── outputs/         # Generated analysis outputs, figures
├── reports/         # Report materials
├── requirements.txt # Python dependencies
├── LICENSE          
├── .gitignore
└── README.md
```

## Interpretation and limitations

Recorded theft data are informative, but they are not a complete census of all thefts. Some incidents may never be reported, reporting practices may change over time, and the availability or categorization of records can affect observed trends. A rise or fall in recorded incidents should therefore not automatically be interpreted as an equivalent change in the underlying incidence of theft.

Seasonal patterns also need to be distinguished from broader trends and one-off fluctuations. Projections based on historical data depend on the chosen period, assumptions, and modeling approach; they should be interpreted as exploratory estimates rather than certain predictions. Especially given a lacking factor representing overall bicycle ridership trends, the analyses describe patterns in the data and do not, on their own, establish why those patterns occur or delineate direct causal mechanisms of theft.
