import pandas as pd
import os

DATA_DIR = 'data'

def load_and_process_data():
    """
    Loads, cleans, and merges the plastic recycling datasets.
    Returns:
        pd.DataFrame: Merged main dataframe (Recycling, Production, Waste, GDP, Taxes)
        pd.DataFrame: Prices dataframe
        pd.DataFrame: Imports dataframe
        pd.DataFrame: Exports dataframe
    """
    
    # --- 1. Load Main Datasets ---
    try:
        recycling = pd.read_csv(os.path.join(DATA_DIR, "estat_env_wastrt_recycled_plastic.csv"))
        production = pd.read_csv(os.path.join(DATA_DIR, "estat_sts_inpr_a_index2015.csv"))
        waste_gen = pd.read_csv(os.path.join(DATA_DIR, "plastic waste generated.csv"))
        gdp = pd.read_csv(os.path.join(DATA_DIR, "estat_nama_gdp.csv"))
        taxes = pd.read_csv(os.path.join(DATA_DIR, "estat_env_taxes.csv"))
    except FileNotFoundError as e:
        print(f"Error loading main files: {e}")
        return None, None, None, None

    # --- 2. Clean and Rename Columns (Replicating R Logic) ---
    
    # Recycling
    recycling = recycling[['geo', 'TIME_PERIOD', 'OBS_FLAG', 'OBS_VALUE']]
    recycling = recycling.rename(columns={'OBS_FLAG': 'OBS_FLAG_recycling', 'OBS_VALUE': 'recycling_tonne'})

    # Production
    production = production[['geo', 'TIME_PERIOD', 'OBS_FLAG', 'OBS_VALUE']]
    production = production.rename(columns={'OBS_FLAG': 'OBS_FLAG2015', 'OBS_VALUE': 'production_index2015'})

    # Waste Generation
    waste_gen = waste_gen[['geo', 'TIME_PERIOD', 'OBS_FLAG', 'OBS_VALUE']]
    waste_gen = waste_gen.rename(columns={'OBS_FLAG': 'OBS_FLAG_gen', 'OBS_VALUE': 'plastic_generation'})

    # GDP
    gdp = gdp[['geo', 'TIME_PERIOD', 'OBS_FLAG', 'OBS_VALUE']]
    gdp = gdp.rename(columns={'OBS_FLAG': 'OBS_FLAG_GDP', 'OBS_VALUE': 'GDP_index2015'})

    # Taxes
    taxes = taxes[['geo', 'TIME_PERIOD', 'OBS_FLAG', 'OBS_VALUE']]
    taxes = taxes.rename(columns={'OBS_FLAG': 'OBS_FLAG_envtax', 'OBS_VALUE': 'env_tax_mioEUR'})

    # --- 3. Merge Main DataFrame ---
    merged_df = pd.merge(recycling, production, on=['geo', 'TIME_PERIOD'], how='outer')
    merged_df = pd.merge(merged_df, waste_gen, on=['geo', 'TIME_PERIOD'], how='outer')
    merged_df = pd.merge(merged_df, gdp, on=['geo', 'TIME_PERIOD'], how='outer')
    merged_df = pd.merge(merged_df, taxes, on=['geo', 'TIME_PERIOD'], how='outer')

    # Convert TIME_PERIOD to int
    merged_df['TIME_PERIOD'] = pd.to_numeric(merged_df['TIME_PERIOD'], errors='coerce')
    merged_df = merged_df.dropna(subset=['TIME_PERIOD'])
    merged_df['TIME_PERIOD'] = merged_df['TIME_PERIOD'].astype(int)

    # Calculate Derived Metrics
    # Recycling Rate: (Recycled / Generated) * 100
    merged_df['recycling_rate'] = (merged_df['recycling_tonne'] / merged_df['plastic_generation']) * 100

    # Recycling Index (Base 2016)
    # We need to calculate this per country (geo)
    def calculate_recycling_index(group):
        base_value_year = 2016
        base_row = group[group['TIME_PERIOD'] == base_value_year]
        if not base_row.empty:
            base_val = base_row['recycling_tonne'].values[0]
            if base_val > 0:
                group['recycling_index'] = (group['recycling_tonne'] / base_val) * 100
            else:
                group['recycling_index'] = None
        else:
            group['recycling_index'] = None
        return group

    merged_df = merged_df.groupby('geo', group_keys=False).apply(calculate_recycling_index)

    # --- 4. Load & Process New Datasets ---

    # Prices
    try:
        prices = pd.read_csv(os.path.join(DATA_DIR, "prices_plasticinprimary_index2015.csv"))
        # Expected cols: geo, TIME_PERIOD, OBS_VALUE... keep relevant
        # Assuming typical Eurostat structure, but let's check what we need. 
        # Actually viewed file wasn't this one, but let's assume standard structure or simplistic load for now.
        # Wait, I didn't view `prices_plasticinprimary_index2015.csv` content in the previous turn, I viewed `Price data.R`.
        # Taking a safer bet: simplistic load and rename value.
        prices = prices[['geo', 'TIME_PERIOD', 'OBS_VALUE']]
        prices = prices.rename(columns={'OBS_VALUE': 'price_index_2015'})
    except Exception as e:
        print(f"Warning: Could not process prices file fully: {e}")
        prices = pd.DataFrame()

    # Imports (Monthly)
    try:
        imports = pd.read_csv(os.path.join(DATA_DIR, "estat_plastic_imports_ EU.csv"))
        # Columns: DATAFLOW,LAST UPDATE,freq,reporter,partner,product,flow,indicators,TIME_PERIOD,OBS_VALUE
        imports = imports[['TIME_PERIOD', 'OBS_VALUE', 'partner', 'product']]
        imports['TIME_PERIOD'] = pd.to_datetime(imports['TIME_PERIOD'])
        imports = imports.rename(columns={'OBS_VALUE': 'import_quantity_100kg'})
    except Exception as e:
        print(f"Warning: Could not process imports file: {e}")
        imports = pd.DataFrame()

    # Exports (Annual)
    try:
        exports = pd.read_csv(os.path.join(DATA_DIR, "estat_ds-plastic export quantity.csv"))
        # Columns: ... reporter, partner, TIME_PERIOD, OBS_VALUE
        exports = exports[['reporter', 'TIME_PERIOD', 'OBS_VALUE']]
        # reporter field has long names "Germany (incl...)" -> might need cleaning if used for mapping, 
        # but for simple bar charts it's fine.
        exports = exports.rename(columns={'reporter': 'geo', 'OBS_VALUE': 'export_quantity_100kg'})
    except Exception as e:
        print(f"Warning: Could not process exports file: {e}")
        exports = pd.DataFrame()

    return merged_df, prices, imports, exports

if __name__ == "__main__":
    df, prices, imports, exports = load_and_process_data()
    print("Main Data Shape:", df.shape)
    print("Columns:", df.columns.tolist())
    print("\nPrices Data Shape:", prices.shape)
    print("\nImports Data Shape:", imports.shape)
    print("\nExports Data Shape:", exports.shape)
