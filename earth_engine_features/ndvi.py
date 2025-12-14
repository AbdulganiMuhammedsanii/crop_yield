import ee

ee.Authenticate()
ee.Initialize(project='kansascrops')

# -----------------------------
# Load US counties
# -----------------------------
counties = ee.FeatureCollection("TIGER/2018/Counties")

# Load your uploaded CSV of target counties
# Must contain: STATEFP, NAME
target = ee.FeatureCollection("projects/kansascrops/assets/county_coords_required")

# Join target list to TIGER counties
# (inner join on STATEFP + NAME)
joined = ee.Join.inner().apply(
    counties,
    target,
    ee.Filter.And(
        ee.Filter.equals(leftField='STATEFP', rightField='statefp'),
        ee.Filter.equals(leftField='NAME', rightField='county')
    )
)

# Extract the county geometry from the join
core_counties = ee.FeatureCollection(
    joined.map(lambda f: ee.Feature(f.get('primary')))
)

print("Number of counties:", core_counties.size().getInfo())

# -----------------------------
# MODIS NDVI
# -----------------------------
modis = (
    ee.ImageCollection("MODIS/061/MOD13Q1")
      .select('NDVI')
      .map(lambda img:
           img.multiply(0.0001)
              .copyProperties(img, ['system:time_start'])
      )
)

# -----------------------------
# Growth stages
# -----------------------------
stages = {
    'early':  (4, 5),
    'veg':    (6, 6),
    'flower': (7, 7),
    'fill':   (8, 8),
    'late':   (9, 9)
}

# -----------------------------
# Loop over years
# -----------------------------
for year in range(2001, 2025):
    print(f"Creating NDVI export for {year}")

    yearly = modis.filterDate(f'{year}-01-01', f'{year}-12-31')

    stage_features = []

    for stage, (m1, m2) in stages.items():
        stage_img = (
            yearly
            .filter(ee.Filter.calendarRange(m1, m2, 'month'))
            .mean()
            .rename(stage)
        )

        reduced = stage_img.reduceRegions(
            collection=core_counties,
            reducer=ee.Reducer.mean(),
            scale=250
        )

        reduced = reduced.map(
            lambda f: f.set({
                'year': year,
                'stage': stage
            })
        )

        stage_features.append(reduced)

    merged = ee.FeatureCollection(stage_features).flatten()

    task = ee.batch.Export.table.toDrive(
        collection=merged,
        description=f"NDVI_CoreCounties_{year}",
        folder="NDVI_CoreCounties",
        fileFormat="CSV"
    )

    task.start()

print("All export tasks submitted.")
