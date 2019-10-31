---
title: "Instructions"
css: "www/css/sandstone.min.css"

output:
  html_document:
      keep_md: yes
---

## What is a sequential testing procedure?

The term of *sequential testing* (or more broadly, sequential analysis) refers to the continuous analysis of data during its collection. Several procedures have been developed to answer somehow different questions from data. For instance, sequential analyses can be conducted until a predefined level of evidence (as quantified by a Bayes factor) is reached (see the *Sequential Bayes Factor* procedure presented in Schönbrodt et al., 2017; Schönbrodt & Wagenmakers, 2018). Alternatively, one can run analyses to either accept or reject some value of interest using a sequential HDI+ROPE procedure (for a practical primer, see Kruschke, 2018) or until reaching a predefined level of precision in the estimate of interest, as expressed by the width of the confidence/credible interval (e.g., see Kruschke, 2015, chapter 13). Overall, sequential analyses usually permit to reach conclusions faster (i.e., with less observations) than classical NHST designs with a priori power analyses, while keeping long-term error rates at a similar or lower level (Schönbrodt et al., 2017, Kruschke, 2015).

## What could possibly go wrong?

During data collection, in the absence of blinding, the experimenter^[If she is also the data analyst and/or she is in contact with the data analyst or is somehow aware of the results of the statistical analyses being conducted.] might (involuntarily) influence the process of data collection itself. By collecting data iteratively, the experimenter-analyst is confronted with data that either confirms or disconfirms her a priori beliefs/expectations (with regards to the hypothesised results of the ongoing study). This "online" source of information might in turn influence the behaviour of the experimenter-analyst, which might in turn influence the behaviour of the participant, and as a result, the data being collected (and analysed).

During data analysis, a priori beliefs/expectations might also lead to confirmation or disconfirmation biases (MacCoun & Perlmutter, 2017). This can translate in the way the experimenter-analyst handle the many *researcher degrees of freedom* (Simmons, Nelson, & Simmonsohn, 2011). More precisely, depending on the current state of the ongoing data collection and analysis (i.e., whether the results are congruent or incongruent with a priori beliefs/expectations), the experimenter-analyst might decide to handle influential observations in a certain way, to use less/more stringent criteria for removing data, etc. This additional flexibility in data analysis might be reduced by either having two seperate agents for data collection and data analysis or by blinding the experimenter-analyst to the current state of the ongoing sequential testing procedure.

## A fully automated, transparent and reproducible protocol for sequential analyses

To tackle these problems, we propose an implementation of a procedure allowing for blind analyses during sequential analyses procedures. This procedure exploits synchronisation features between popular data collection software programs (i.e., PsychoPy and OpenSesame) and the Open Science Framework (OSF). As data are collected, they are uploaded on the OSF continuously. In parallel, we illustrate how all data analyses can be automated. We provide R scripts that can be executed automatically at given intervals (e.g., every hour) to run the planned analysis and to return the results of this analysis to the experimenter-analyst by email. 

## Details of the application

This app contains two main components aiming 1) to facilitate the generation of R scripts to be used to automate blind data collection and data analysis during sequential testing and 2) to provide an illustration of the potential biases that can appear during sequential testing through simulation.

## Tutorial paper

For more information on how to implement such procedures, have a look at our [tutorial paper](https://osf.io/mwtvk/).
