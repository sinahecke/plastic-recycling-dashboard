import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from data_loader import load_and_process_data

# --- PAGE CONFIG ---
st.set_page_config(page_title="Plastic Recycling Dashboard", layout="wide")

st.title("♻️ Plastic Recycling & Production Dashboard")
st.markdown("""
This dashboard visualizes data on plastic production, recycling, waste generation, and economic drivers across European countries.
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
        st.subheader("Plastic Production (Index 2015=100)")
        fig_prod = px.line(
            filtered_df, 
            x='TIME_PERIOD', 
            y='production_index2015', 
            color='geo',
            markers=True,
            title="Production Index over Time"
        )
        st.plotly_chart(fig_prod, use_container_width=True)
        
    with col2:
        st.subheader("Recycling Quantities (Index 2016=100)")
        fig_rec = px.line(
            filtered_df, 
            x='TIME_PERIOD', 
            y='recycling_index', 
            color='geo', 
            markers=True,
            title="Recycling Index over Time"
        )
        st.plotly_chart(fig_rec, use_container_width=True)

    st.subheader("Plastic Waste Generation vs Recycling (Tonnes)")
    
    # Dual Axis or just overlay? Let's use scatter/line for one country or aggregate?
    # Better: Faceted View or just simple line chart for chosen metric
    metric_choice = st.radio("Select Metric to Compare:", ["Recycling Tonnes", "Waste Generation", "Recycling Rate (%)"], horizontal=True)
    
    mapping = {
        "Recycling Tonnes": "recycling_tonne",
        "Waste Generation": "plastic_generation",
        "Recycling Rate (%)": "recycling_rate"
    }
    
    fig_metric = px.line(
        filtered_df,
        x='TIME_PERIOD',
        y=mapping[metric_choice],
        color='geo',
        markers=True,
        title=f"{metric_choice} over Time"
    )
    st.plotly_chart(fig_metric, use_container_width=True)


# --- TAB 2: CORRELATIONS ---
with tab2:
    st.header("Correlations & Drivers")
    st.write("Explore relationship between different variables.")
    
    col_corr1, col_corr2 = st.columns(2)
    
    with col_corr1:
        x_axis = st.selectbox("X Axis", ["recycling_tonne", "recycling_rate", "plastic_generation", "GDP_index2015", "env_tax_mioEUR"], index=0)
    with col_corr2:
        y_axis = st.selectbox("Y Axis", ["production_index2015", "recycling_tonne", "plastic_generation"], index=0)

    # Log transformation toggle
    log_x = st.checkbox("Log Scale X-Axis", value=True)
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
        # Filter prices by time as well
        prices_filt = prices[(prices['TIME_PERIOD'] >= start_year) & (prices['TIME_PERIOD'] <= end_year)]
        fig_prices = px.line(
            prices_filt,
            x='TIME_PERIOD',
            y='price_index_2015',
            color='geo', # Most likely only one or few lines if aggregated
            markers=True,
            title="Price Index Trends"
        )
        st.plotly_chart(fig_prices, use_container_width=True)
    else:
        st.warning("Price data not available.")

    col_trade1, col_trade2 = st.columns(2)
    
    with col_trade1:
        st.subheader("EU Plastic Waste Imports (Monthly)")
        if not imports.empty:
            # imports is monthly, maybe aggregate to yearly? Or show full detail
            # Let's show full detail but filter if year range allows
            imports_filt = imports[
                (imports['TIME_PERIOD'].dt.year >= start_year) & 
                (imports['TIME_PERIOD'].dt.year <= end_year)
            ]
            fig_imports = px.area(
                imports_filt,
                x='TIME_PERIOD',
                y='import_quantity_100kg',
                color='partner', # usually 'Extra-EU'
                title="Imports of Plastic Waste to EU"
            )
            st.plotly_chart(fig_imports, use_container_width=True)
    
    with col_trade2:
        st.subheader("Plastic Waste Exports by Country (Annual)")
        if not exports.empty:
            exports_filt = exports[
                (exports['geo'].isin(selected_countries)) &
                (exports['TIME_PERIOD'] >= start_year) & 
                (exports['TIME_PERIOD'] <= end_year)
            ]
            fig_exports = px.bar(
                exports_filt,
                x='TIME_PERIOD',
                y='export_quantity_100kg',
                color='geo',
                barmode='group',
                title="Exports of Plastic Waste"
            )
            st.plotly_chart(fig_exports, use_container_width=True)

# --- TAB 4: DISTRIBUTIONS ---
with tab4:
    st.header("Data Distributions")
    
    dist_metric = st.selectbox("Select Metric for Distribution", ["recycling_rate", "production_index2015", "plastic_generation", "recycling_tonne"])
    
    fig_box = px.box(
        filtered_df,
        x='geo',
        y=dist_metric,
        color='geo',
        title=f"Distribution of {dist_metric} by Country"
    )
    st.plotly_chart(fig_box, use_container_width=True)
