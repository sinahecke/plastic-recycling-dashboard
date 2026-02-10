# Plastic Recycling Dashboard

This dashboard serves as a supplementary visualization tool for the Master's Thesis: **“Does Recycling Reduce Plastic Production? A European Panel Data Analysis of Circular Economy Rebound Effects”** by **Sina Heckenberger**, Hertie School Master of Public Policy.

## Overview

The transition from a linear to a circular economy is widely promoted as a key solution to the adverse environmental impacts from plastic production and consumption. This research investigates the potential for **Circular Economy Rebound Effects (CERE)** in plastic recycling in the EU. 

Circular Economy Rebound effects occur when efficiency gains in recycling might unintendedly drive increased primary plastic production, thereby offsetting the expected environmental benefits. The empirical analysis in this thesis finds preliminary evidence for circular economy rebound effects based on a European panel data analysis of plastic production and recycling volumes and derives policy recommendations accordingly.

By visualizing relationships between plastic production, recycling rates, and economic drivers like trade volumes and prices across Europe, this dashboard aids the interpretation of the relevant relationships found in the analysis.

You can view the full Master's Thesis [here](Heckenberger_Sina_master_thesis.pdf).

## Features

- **Interactive Filtering**: Filter data by Country and Year Range.
- **Trends over Time**: Visualize Production, Recycling, and Waste Generation indices.
- **Correlations & Drivers**: Explore relationships between economic variables (GDP, Taxes) and plastic metrics with regression lines.
- **Price and Trade Analysis**: 
    - Track Producer Prices of primary plastics.
    - Visualize EU Plastic Waste Imports and Exports.
- **Distributions**: Statistical summary of keys metrics across countries using Box Plots.

## Built With

- **Python**: Core programming language.
- **Streamlit**: Web application framework.
- **Plotly**: Interactive charting library.
- **Pandas**: Data manipulation and analysis.

## Setup & Installation

To run this dashboard locally, follow these steps:

1.  **Clone the Repository**:
    ```bash
    git clone https://github.com/sinahecke/plastic-recycling-dashboard.git
    cd plastic-recycling-dashboard
    ```

2.  **Create a Virtual Environment** (Recommended):
    ```bash
    python3 -m venv .venv
    source .venv/bin/activate  # On Windows use: .venv\Scripts\activate
    ```

3.  **Install Dependencies**:
    ```bash
    pip install -r requirements.txt
    ```

4.  **Run the Dashboard**:
    ```bash
    streamlit run dashboard.py
    ```

The dashboard will open automatically in your default web browser (usually at `http://localhost:8501`).

## Data Sources

- **Eurostat**: All data visualized in this dashboard is sourced from Eurostat public datasets.

## License

This project is part of a Master's Thesis submission.