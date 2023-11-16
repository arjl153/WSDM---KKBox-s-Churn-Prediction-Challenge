# KKBOX’s Churn Prediction

## Problem Statement

Predict whether a user of the music streaming service KKBOX will churn after their subscription expires.

## Introduction

- Predict customer churn.
- Analyze user behavior over a selected time-frame.
- Conduct exploratory and predictive data analysis.

## Churn - What is it?

In simple terms, it asks: "Will the user stay or go?" Churn could involve subscription cancellation, membership ending, non-renewal of a service, etc.

## Data

Data comes in the form of four different files:
- `train.csv`: IDs and whether users have churned or not.
- `transactions.csv`: Details like payment method or subscription cancellation.
- `user_logs.csv`: Listening behavior of a user in terms of the number of songs played.
- `members.csv`: User's age, city, and other membership information.

## Individual Feature Visualizations

- Majority of users did not churn, about 63,000 did.
- Gender distribution is slightly skewed, with a slightly higher percentage of male over female users.
- Different registration methods have varying popularity.
- Age distribution peaks among young adults.
- New registrations started rising in 2010, with a decrease in the last couple of years.

## Exploratory Analysis

- Weekends have significantly higher registration numbers compared to weekdays.
- Initial registrations were more in May-July months and Nov-Dec months.
- Data contains 1 to 8 entries (days) per user, with total listening time being roughly log-normally distributed.
- Majority of users did not cancel.

## Churn Rate Analysis

- Highest churn rates happened for users in their 20s, with age 22 having the highest churn rate.
- Gender was not a deciding factor for churning.

## Predictive Analysis - Logistic Regression

Results based on logistic regression with significant features like auto-renew, transaction date, and membership expire date.

### Results

- **Accuracy:** 98.112%
- **Significant Features:**
  - **Auto-renew (Coefficient = 2.286):** For one unit change in auto-renew, the log odds of churn increases by 2.286 times.
  - **Is_cancel (Coefficient = 1.09):** For one unit change in is_cancel, the log odds of churn decreases by 1.09 times.
  - **Transaction_date (Coefficient = 0.006):** For one unit change in transaction_date, the log odds of churn increases by 0.006 times.
  - **Membership_expire_date (Coefficient = 0.005):** For one unit change in membership_expire_date, the log odds of churn increases by 0.005 times.
