Exercise 1
================

## Part 1

Surveying employees for bonus vs salary raise:

This Google study/experiment seems to mostly resemble a one-group
pretest-posttest design.

NR O1 X O2

NR: All of Google employees were involved in getting the 10% salary
hike. Employees were not assigned (randomly or not randomly) to any
control group.

O1: This is happiness score as measured by Googlegeist before the salary
hike. This can also measure the Attrition rate to other companies before
the salary increase.

X: This is the treatment or in this case, the 10% increase in salary.

O2: This is happiness score as measured by Googlegeist after the salary
hike. This can also measure the Attrition rate to other companies after
the salary increase.

Based on the article, the Googlegeist score increased and the attrition
rate decreased after the 10% salary increase.

Potential threats to inference:

1)  Any event that may have occurred simultaneously with the treatment
    (here the 10% salary increase), could lead to an increase in
    Googlegeist score or a decrease in the attrition rate. Careful
    consideration needs to be taken to ensure that the environment
    before and after the treatment was unchanged and no other factors
    could be at play.

2)  There is no control group present. This makes the only comparison
    point O1 (before the treatment). A control group exposed to the same
    envoronment as other employees would ensure that the treatment is
    the only cause for the result as observed.

3)  The fact that a survey was performed before taking any action may be
    the best supporting argument that the happiness increase is truely
    due to the salary increase.

4)  Another important argument against the validity of this experiment
    could be the fact that a survey was performed before the
    experiment/action. Perhaps the employees would not have been overy
    happy about the 10% salary increase if nothing was explained to them
    and no background was provided. The fact that Google asked their
    employees and then took actual action may be contributing to an
    extra artificial increase in the happiness scores in Googlegeist,
    based on the fact that ???Google really cares for their employees???.
    Because of this, it may be interesting to know what would have
    happened to the happiness scores and the attrition rate if google
    had not taken any survey.

Can this be made better?

Lets look at this scenario,

R O1 XA O2

R O1 XB O2

R O1 O2

Here, Google employees could be randomly assigned to three groups. Group
1 will experience a 10% increase in salary. Group 2 will receive a bonus
and the control group will receive nothing. Before and after of the
happiness scores as measured by Googlegeist of people in the three
groups, will then tell us what the true result is. The observation
before and after in this case can also be the attrition rate (further
detailed below).

Thinking about how this could be made better by Google, a design such as
this may be interesting albeit perhaps unfair to Google employees in the
control group as well as to those who were not selected/randomly
assigned to be in the experiment. It is also important to mention that
in such an experiment, Google may have to stick to these treatments
because of legal obligations. Because of these reasons, I don???t think
this is an experiment that is practical, however, this is the best true
experiment I could think of that measures the effect of salary increase
vs bonus vs nothing.

In an experiment like this, the attrition rate after treating a
particular group using XA or XB would be hard to measure as it is a
company wide phenomenon, which makes it hard to find out the attrition
rate for specific groups (unless specifically measured per group). The
attrition rate before however, will remain the same for all three groups
in this experiment.

It is also important to consider if the group sample size will be
statistically significant when dividing the company into three randomly
assigned groups.

## Part 2

Based on my analysis of the data provided and because of the unclear
nature of the data as well as the question, I found two ways of looking
into the data set. On both the ways, the following was assumed: (1) The
three columns represents a control individual, individual A and
individual B. (2) The rows represent the number of days and (3) The data
provided is some sort of performance score of an individual that is
taken at face value and represents the true performance on an individual
on that day.

Option A: The results obtained were as follows:

1)  On average (across 1000 days), compared to the days when
    treatment/tactic was not deployed on the two individuals, tactic B
    helps the performance of individual B, while tactic A worsens the
    performance of individual A.

2)  Average score of individual A after deploying tactic A decreased by
    1134%, while the average score of individual B after deploying
    tactic B increased by 4340%.

3)  The control???s average score on the days the treatment was provided
    showed negligible improvement/deterioration over the control???s
    average score on the days treatment was not provided.

Option B: The results obtained were as follows:

1)  Compared to the control individual score on the days treatment was
    provided, tactic A resulted in a score that was higher than the
    control???s score 93% of the time (on individual A)

2)  Compared to the control individual score on the days treatment was
    not provided, individual A???s score was higher than the control???s
    score 54% of the time. Almost at 50%, which means it is almost a
    coin toss, who has higher score on a given day.

3)  Compared to the control individual score on the days treatment was
    provided, tactic B resulted in a score that was higher than the
    control???s score 96% of the time (on individual B)

4)  Compared to the control individual score on the days treatment was
    not provided, individual B???s score was higher than the control???s
    score 55% of the time. Almost at 50%, which means it is almost a
    coin toss, who has higher score on a given day.

5)  This method may be flawed because we are comparing scores of two
    different individuals. For example, we are comparing the score of
    control individual to the score of another individual after using a
    particular tactic. This can be a inference issue and threat to the
    validity of the experiment.

Both the methods result in a situation where tactic B is better. However
there are a few key points to keep in mind:

1)  Sample size is too low to make a deduction for all the employees.
    This does not tell that everyone will improve/deteriorate. The
    results obtained may only apply to A and B, not all employees.

2)  Data is unclear as to what it represents and it is hard to make any
    generalized conclusions regarding employee performance. Because of
    the unclarity, the manager and the expert may be doing their own
    version of the analysis causing a discrepancy in their conclusions.

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](Exercise-1_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
