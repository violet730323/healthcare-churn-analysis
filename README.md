# Predicting Patient Churn in a Community Healthcare Setting

## Project Overview

Developed predictive analytics models to identify patients at high risk of disengaging from healthcare services, enabling proactive intervention strategies and improving continuity of care.

---

## Business Problem

Patient disengagement creates major operational and healthcare challenges for community clinics. When patients stop attending follow-up appointments:

* Chronic conditions may worsen due to inconsistent care
* Healthcare providers lose visibility into at-risk populations
* Clinics face operational and resource allocation challenges

The goal of this project was to build an interpretable and actionable predictive model capable of identifying patients likely to disengage before long-term churn occurs.

---

## Data

### Data Sources

Analysis was conducted using anonymized and aggregated healthcare operational data, including:

* Patient encounter history
* Appointment activity
* Diagnosis categories
* Demographic indicators

### Privacy & Confidentiality

All sensitive organizational and patient information was removed or anonymized. No protected health information (PHI) or proprietary operational data is included in this repository.

### Feature Engineering

Constructed predictive variables including:

* Missed appointment frequency
* Visit consistency
* Chronic condition indicators
* Distance/travel-related variables
* Employment and socioeconomic indicators

---

## Methodology

### Predictive Models

Built and compared multiple machine learning approaches:

* Logistic Regression
* K-Nearest Neighbors (KNN)

### Modeling Strategy

* Developed multiple churn definitions to distinguish operational disengagement patterns
* Applied class imbalance handling techniques including:

  * Down-sampling
  * Up-sampling
  * SMOTE
* Optimized models using:

  * Accuracy
  * Sensitivity (Recall)
  * Specificity

### Objective

Prioritized identifying at-risk patients early enough for proactive intervention rather than maximizing overall accuracy alone.

---

## Key Findings

### 1. Missed Appointments Strongly Predict Future Disengagement

Repeated no-shows emerged as one of the strongest behavioral indicators of future patient churn.

### 2. Socioeconomic Instability Increases Risk

Patients experiencing employment or financial instability demonstrated higher probabilities of interrupted care and disengagement.

### 3. Chronic Care Patients Require Additional Retention Support

Patients managing chronic conditions were more vulnerable to disengagement due to transportation, financial, and scheduling barriers.

---

## Business Recommendations

### Early Warning Outreach System

Implement automated outreach workflows for patients with repeated missed appointments.

### Targeted Retention Programs

Develop support initiatives for:

* Chronic care populations
* Economically vulnerable patients
* High-risk engagement groups

### Operational Risk Dashboard

Integrate predictive risk scoring into clinic operations to support proactive decision-making and resource prioritization.

---

## Tech Stack

### Programming & Analytics

* Python
* Pandas
* NumPy
* Scikit-learn

### Machine Learning

* Logistic Regression
* KNN Classification
* SMOTE
* Feature Engineering

### Visualization & Reporting

* Tableau
* Matplotlib

### Data Processing

* SQL
* Excel

---

## Disclaimer

This project is presented for portfolio and educational purposes only. All organizational identifiers, sensitive operational details, and protected information have been removed or modified to preserve confidentiality.
