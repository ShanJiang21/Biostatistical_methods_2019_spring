## Logistic Regression

#### From Binary, Bernoulli to Binomial 

Binary GLMs come from trying to model outcomes that can take only two values. Some examples include survival/not, win versus lose at a game and success versus failure of a treatment or study. Often these outcomes only have two dichotomous values, 0 or 1, and we call them **binary** outcomes or **bernoulli** outcomes. 

However, in real life, it's the simpest kind of random variable. So, when we collect a bunch of zeros and ones and they are independent, then the total number of successes or failures follows a **binomial distribution.** 

#### Ravens Data Example 

[Ravens](https://www.baltimoreravens.com/) is one famous football team based in Baltimore, MD. In this example, we get the dataset from [GitHub repository](https://github.com/ShanJiang21/Johns-Hopkins-Data-Science-Specialization-Courses/raw/master/07_RegressionModels/03_02_binaryOutcomes/data/ravensData.rda).  

**1. Read in the data**

```{r}
## import ravens data 
load("~/Downloads/mailman_semester_2/Biotatistics Methods II/logistic project/ravensData.rda")
head(ravensData)
```

In this case, take a quick glimpse of data, then we find the residual is not gaussian distributed, which violated the SLR assumptions, they are obviously not fit for this dataset.



**Basic Concepts** **and models** : SLR 

​							$$RW_i = b_0 +b_1 RS_i+ e_i​$$

$RW_i​$ : Probability of Ravens win, 1 = win, 0 for opposite;

$RS_i$: Number of points Ravens scored;

$b_0$: Probability of Ravens win when they scored zero point

$b_1$: change in probability of Ravens win for a unit change of point scored by Ravens;

$e_i​$: residual variation due 

#### **Model: Logistic Regression**

- $Pr(RW_i) = \frac{exp(b_0 +b_1 RS_i)}{1+ exp(b_0+b_1RS_i)}$ (The **inverse** of log odds: xbit)

- $log( \frac{ pr(RW_i)}{1- pr(RW_i)})$ = $b_0 +b_1 RS_i$ (logit)

- Notations:

  Probability of Ravens win: $P(RW_i) = \pi $, 	Probability of fail:	$P(RF_i) = 1- \pi $, 

  - **Odds**: (0, $\infty$)                                  $\frac{P(RW_i)}{1-P(RW_i)}$ = $\frac{\pi}{1-\pi}$ 

  - **Log odds**: ($-\infty , \infty​$)		$log(\frac{P(RW_i)}{1-P(RW_i)} )​$ = log($\frac{\pi}{1-\pi }​$)

  - **Log Odds Ratio**:                    $b_1$= $b_0 + b_1(RS_i +1) - [b_0 +b_1\dot RS_i] = log (odds_{RS+1} / odds_{RS})$
  - **Odds Ratio**:                          $e^{b_1}$ = $\frac{odds_{RS+1} }{odds_{RS} }$
  - Note: 
    1. about ***logit***, logit (p) = log (odds) = log ($\frac{\pi}{1-\pi}$)
    2. Probability and Odds: odds = $\frac{p}{1-p}$,  probability = $\frac{Odds}{1+Odds}$ 
    3. The odds of success  = 1/ the odds of failure (**reciprocal**)

****

#### Interpretation**: **odds versus **probability** 

Dependent variable as **Continuous**: log odds ratio 

- $b_0$: log odds of Ravens win if they score 0 points; (Compared with SLR, it does have own meanings)
- $b_1$: log odds ratio of win probability for each points earned 
- $exp(b_1)$ : odds ratio of win probability for each point scored

Dependent variable as **binary**: If the covariate is also a binary variable such as gender, the OR  = $e^{b1}$ if $b_1$ is the coefficient for gender, implying the odds of winning for males is  **$e^{b1}$** times that of females.





#### Multinomial Logistic Regression



$\pi_{ij}$ = 



#### Ordinal Data-Proportional Odds model







#### Count data - Possion 