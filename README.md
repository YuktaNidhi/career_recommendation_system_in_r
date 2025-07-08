# 🎓 Career Recommender System

An interactive, intelligent Shiny web application that helps students discover suitable career paths based on their subject-specific strengths using **cosine similarity**, **data normalization**, and a **MySQL-powered database**.

---

## 📌 Features

- 🔗 **Career Recommendation** based on similarity to peer profiles  
- 🧠 **Cosine Similarity Matching** to compare users with existing students  
- 📊 **Interactive Visualizations** (heatmaps) to compare skills and scores  
- 🗃️ **Live Database Integration** with MySQL (`career_db`)  
- 🎛️ **Clean, Modern UI** with sidebar navigation  

---

## 🧠 How It Works

1. Students enter their scores across various categories (STEM, Arts, Business, etc.)  
2. Scores are **normalized** to ensure fairness  
3. The system computes **cosine similarity** between the user and all existing students  
4. Top matches are selected and their **most frequent careers** are recommended  
5. Heatmaps show how the user's profile compares to similar students or job groups  

---

## 🧱 Technologies Used

| Layer        | Tools & Libraries                                                            |
|--------------|------------------------------------------------------------------------------|
| Frontend     | `shiny`, `shinyjs`, `shinydashboardPlus`, `bslib`, `ggplot2`, `shinyWidgets` |
| Backend      | `R`, `dplyr`, `proxy`, `reshape2`                                            |
| Database     | `MySQL`, accessed via `RMySQL` and `DBI`                                     |
| Similarity   | `proxy::dist(method = "cosine")`                                             |

---

## 🗃️ Database Structure

The app connects to a MySQL database named `career_db`. The main table used is `career_data`, which contains:

- `StudentID` – Unique identifier for each student  
- Subject score columns:
  - `STEM_Score`
  - `Business_Finance_Score`
  - `Arts_Media_Score`
  - `Healthcare_Score`
  - `Education_Score`
  - `Social_Services_Score`
  - `Trades_Manufacturing_Score`
  - `Government_Law_Score`
- `Primary_Career_Recommendation` – Suggested career based on profile

---

## 🚀 Running the App

### 1. ✅ Prerequisites

- R installed with the required libraries  
- A running MySQL server with:
  - Username: `root`
  - Password: `password`
  - Database: `career_db` with table `career_data` populated

### 2. 📦 Install R packages

```r
install.packages(c("shiny", "shinyjs", "shinydashboard", "shinydashboardPlus",
                   "DBI", "RMySQL", "dplyr", "proxy", "ggplot2", "reshape2",
                   "shinyWidgets", "bslib"))
