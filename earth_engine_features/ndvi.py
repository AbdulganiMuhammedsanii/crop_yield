import ee
ee.Authenticate()
ee.Initialize(project='kansascrops')

counties = ee.FeatureCollection("TIGER/2018/Counties")

target_counties = [
    'Sherman','Cheyenne','Thomas','Wallace','Logan','Gove','Trego','Scott','Wichita','Greeley',
    'Hamilton','Kearny','Finney','Gray','Ford','Hodgeman','Ness','Stanton','Morton','Stevens',
    'Barton','Ellis','Rush','Russell','Rice','Reno','McPherson','Saline','Stafford','Pawnee',
    'Douglas','Franklin','Miami','Johnson','Brown','Atchison','Doniphan'
]

kansas = counties \
    .filter(ee.Filter.eq('STATEFP', '20')) \
    .filter(ee.Filter.inList('NAME', target_counties))

modis = (
    ee.ImageCollection("MODIS/061/MOD13Q1")
      .select('NDVI')
      .map(lambda img:
           img.multiply(0.0001).copyProperties(img, ['system:time_start'])
      )
)

stages = {
    'early':  (4, 5),
    'veg':    (6, 6),
    'flower': (7, 7),
    'fill':   (8, 8),
    'late':   (9, 9)
}

for year in range(2001, 2025):
    print(f"Creating export task for {year}")

    yearly = modis.filterDate(f'{year}-01-01', f'{year}-12-31')

    feature_list = []

    for stage, (m1, m2) in stages.items():
        stage_img = yearly.filter(
            ee.Filter.calendarRange(m1, m2, 'month')
        ).mean().rename(stage)

        reduced = stage_img.reduceRegions(
            collection=kansas,
            reducer=ee.Reducer.mean(),
            scale=250
        )

        # Tag with year + stage
        reduced = reduced.map(lambda f:
            f.set('year', year).set('stage', stage)
        )

        feature_list.append(reduced)

    # Merge all stages for the year
    merged = ee.FeatureCollection(feature_list).flatten()

    # Export one CSV per year
    task = ee.batch.Export.table.toDrive(
        collection=merged,
        description=f"NDVI_Kansas_{year}",
        folder="NDVI_Kansas",
        fileFormat="CSV"
    )
    task.start()

print("All export tasks created. Monitor in Earth Engine Task Manager.")
