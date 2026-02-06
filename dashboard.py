import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from data_loader import load_and_process_data

# --- PAGE CONFIG ---
st.set_page_config(page_title="Plastic Recycling Dashboard", layout="wide")

st.title("♻️ Plastic Recycling & Production Dashboard")
st.markdown("""
### Data Analysis for Master's Thesis: Circular Economy Rebound Effects
This dashboard serves as a supplementary visualization tool for my Master's Thesis. 
It investigates the **Rebound Effects** in the Circular Economy of plastics, analyzing relationships between production, recycling, and economic drivers.

*Data Source: Eurostat & Provided Dataset*
""")

# --- LOAD DATA ---
@st.cache_data
def get_data():
    return load_and_process_data()

df, prices, imports, exports = get_data()

# --- SIDEBAR FILTERS ---
st.sidebar.header("Filter Settings")

# Country Filter
all_countries = sorted(df['geo'].unique())
selected_countries = st.sidebar.multiselect(
    "Select Countries", 
    options=all_countries, 
    default=["Germany", "France", "Spain", "Italy"] # Defaults
)

# Year Filter
min_year = int(df['TIME_PERIOD'].min())
max_year = int(df['TIME_PERIOD'].max())

start_year, end_year = st.sidebar.slider(
    "Select Year Range",
    min_value=min_year,
    max_value=max_year,
    value=(min_year, max_year)
)

# Apply Filters
filtered_df = df[
    (df['geo'].isin(selected_countries)) & 
    (df['TIME_PERIOD'] >= start_year) & 
    (df['TIME_PERIOD'] <= end_year)
]

# --- TABS ---
tab1, tab2, tab3, tab4 = st.tabs(["📈 Trends over Time", "🔗 Correlations", "💰 Economic & Trade", "📊 Distributions"])

# --- TAB 1: TRENDS ---
with tab1:
    st.header("Trends over Time")
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.subheader("Plastic Production Index")
        # Ensure data is sorted for lines to connect correctly
        filtered_df = filtered_df.sort_values(by=['geo', 'TIME_PERIOD'])
        
        fig_prod = px.line(
            filtered_df, 
            x='TIME_PERIOD', 
            y='Production Index (2015=100)', 
            color='geo',
            markers=True,
            title="Production Index (2015=100)"
        )
        st.plotly_chart(fig_prod, use_container_width=True)
        
    with col2:
        st.subheader("Recycling Index")
        fig_rec = px.line(
            filtered_df, 
            x='TIME_PERIOD', 
            y='Recycling Index (2016=100)', 
            color='geo', 
            markers=True,
            title="Recycling Index (2016=100)"
        )
        st.plotly_chart(fig_rec, use_container_width=True)

    st.subheader("Plastic Waste vs. Recycling volume")
    
    metric_choice = st.radio("Select Metric to Compare:", ["Recycling Volume (Tonnes)", "Waste Generation (Tonnes)", "Recycling Rate (%)"], horizontal=True)
    
    # Check if data exists for valid plotting (avoid empty lines if NAs)
    plot_data = filtered_df.dropna(subset=[metric_choice])
    
    fig_metric = px.line(
        plot_data,
        x='TIME_PERIOD',
        y=metric_choice,
        color='geo',
        markers=True,
        title=f"{metric_choice} over Time"
    )
    st.plotly_chart(fig_metric, use_container_width=True)


# --- TAB 2: CORRELATIONS ---
with tab2:
    st.header("Destinations & Drivers")
    st.write("Explore relationship between different variables to identify potential rebound effects.")
    
    col_corr1, col_corr2 = st.columns(2)
    
    numeric_columns = [
        "Recycling Volume (Tonnes)", "Recycling Rate (%)", 
        "Waste Generation (Tonnes)", "GDP Index (2015=100)", 
        "Environmental Taxes (Million EUR)", "Production Index (2015=100)"
    ]
    
    with col_corr1:
        x_axis = st.selectbox("X Axis", numeric_columns, index=0)
    with col_corr2:
        y_axis = st.selectbox("Y Axis", numeric_columns, index=5) # Default to Production

    # Log transformation toggle
    col_log1, col_log2 = st.columns(2)
    with col_log1:
        log_x = st.checkbox("Log Scale X-Axis", value=True)
    with col_log2:
        log_y = st.checkbox("Log Scale Y-Axis", value=False)

    fig_scatter = px.scatter(
        filtered_df,
        x=x_axis,
        y=y_axis,
        color='geo',
        trendline="ols", # Add regression line
        hover_data=['TIME_PERIOD'],
        log_x=log_x,
        log_y=log_y,
        title=f"Scatter Plot: {y_axis} vs {x_axis}"
    )
    st.plotly_chart(fig_scatter, use_container_width=True)

# --- TAB 3: ECONOMIC & TRADE ---
with tab3:
    st.header("Economic & Trade Analysis")
    
    # Prices
    st.subheader("Primary Plastic Price Index (2015=100)")
    if not prices.empty:
        # Filter prices by time
        prices_filt = prices[(prices['TIME_PERIOD'] >= start_year) & (prices['TIME_PERIOD'] <= end_year)]
        prices_filt = prices_filt.sort_values(by='TIME_PERIOD') # Sort for lines
        
        fig_prices = px.line(
            prices_filt,
            x='TIME_PERIOD',
            y='Price Index (2015=100)',
            color='geo', 
            markers=True,
            title="Producer Prices of Plastic in Primary Forms"
        )
        st.plotly_chart(fig_prices, use_container_width=True)
    else:
        st.warning("Price data not available.")

    col_trade1, col_trade2 = st.columns(2)
    
    with col_trade1:
        st.subheader("EU Plastic Waste Imports (Monthly)")
        if not imports.empty:
            imports_filt = imports[
                (imports['TIME_PERIOD'].dt.year >= start_year) & 
                (imports['TIME_PERIOD'].dt.year <= end_year)
            ]
            fig_imports = px.line(
                imports_filt,
                x='TIME_PERIOD',
                y='Import Quantity (100kg)',
                color='partner', # usually 'Extra-EU'
                title="Imports of Plastic Waste to EU (Extra-EU)"
            )
            st.plotly_chart(fig_imports, use_container_width=True)
    
    with col_trade2:
        st.subheader("Plastic Waste Exports by Country (Annual)")
        if not exports.empty:
            # Now 'geo' in exports should match selected countries
            exports_filt = exports[
                (exports['geo'].isin(selected_countries)) &
                (exports['TIME_PERIOD'] >= start_year) & 
                (exports['TIME_PERIOD'] <= end_year)
            ]
            
            if not exports_filt.empty:
                fig_exports = px.bar(
                    exports_filt,
                    x='TIME_PERIOD',
                    y='Export Quantity (100kg)',
                    color='geo',
                    barmode='group',
                    title="Exports of Plastic Waste"
                )
                st.plotly_chart(fig_exports, use_container_width=True)
            else:
                st.info("No export data available for the selected countries.")

# --- TAB 4: DISTRIBUTIONS ---
with tab4:
    st.header("Data Distributions")
    
    dist_metric = st.selectbox(
        "Select Metric for Distribution", 
        numeric_columns
    )
    
    # Violin plot is much more intuitive than box plot for some, showing density
    fig_violin = px.violin(
        filtered_df,
        x='geo',
        y=dist_metric,
        color='geo',
        box=True, # Show box plot inside
        points="all", # Show all points
        title=f"Distribution of {dist_metric} by Country"
    )
    st.plotly_chart(fig_violin, use_container_width=True)
