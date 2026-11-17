#### **Definition**

In this module, you can view the **model-predicted yearly averages** for tree canopy cover (TCC), canopy moisture (NDMI), and productivity (NDVI, EVI, or MSAVI2) for two different years. For more information about the model, please visit the "About" section of the app.

#### **How to Use**

You can use this module to compare model-predicted annual vegetation values between two years. Start by selecting an island name from the drop-down menu. Next, select a vegetation metric. Finally, select two years to compare. Please note that it is possible that some metrics or years may be missing for your selected island, depending on data availability.

The "Island Info" box provides information about the island that you selected, including its name, country, and area. It also shows information about any invasive species eradication operations included in the model as an intervention.

The app will automatically generate a map based on your selections. The first year that you selected will display on the left side of the map. The second year that you selected will display on the right side of the map. You can slide the grey slider bar to the right to fully view the map for your selected year. You can also slide the grey slider bar fully to the left to view the map for your second selected year.

Click a pixel on the map to see its location. Below the map, the "Selected Pixel" box will provide information about the selected pixel, including its modeled value in each of the two years you selected. Two time series plots below the text box will show the selected pixel's values over time. The plot on the left shows the smoothed trend (i.e., the overall trend). The plot on the right shows the trend with annual variation. Use the "Expand Map" button to view the map at full size. In the top right corner of the map, select the layer control button to switch the basemap between Dark (default), Open Street Map, or satellite imagery.

If you would like to download the classified rasters as a GeoTIFF, select the "Download Data" button.

#### **Interpretation**

Each pixel on the map is color-coded according to its model-predicted yearly average for tree canopy cover, canopy moisture, or vegetation productivity. Pixels with no value (i.e., black pixels) are missing data.

The pixel-level time series results are shown below the map as two graphs. The left graph shows a smoothed trend, which reduces background noise and variation to show the underlying pattern of change, such as if vegetation is increasing or decreasing. The right graph shows the trend with annual variation. In some instances, the graphs may be similar, indicating minimal interannual variation at the selected location. In other instances, the graphs may be very different, highlighting interannual variation.

For both graphs, the average trend is represented by a black line. The grey shaded area around the trend line is the 85% highest posterior density interval: the model places 85% of its probability inside that band. The points on the graph represent the actual measurements estimated from Landsat data. If a graph has a vertical orange dashed line, this marks the year that a modeled conservation intervention (i.e., eradication of an invasive mammal) occurred.
