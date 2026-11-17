#### **Definitions**

In this module, you can view **model-predicted annualized and accumulated change** for tree canopy cover (TCC), canopy moisture (NDMI), and vegetation productivity (NDVI, EVI, or MSAVI2). For more information about the model, please visit the "About" section of the app.

**Annualized Change**

Annualized change is the average change in the selected vegetation index per year, i.e., the annual rate of change. For islands without a modeled conservation intervention (i.e., not invasive species eradication), we predict annualized change across the full time series. For islands with a modeled conservation intervention, we predicted annualized change in the pre-intervention period, post-intervention period, and full time series. We also predict the difference in annual rate of change between the post- and pre-intervention periods (i.e., dynamic baseline). The dynamic baseline can be used to evaluate the effects of the intervention on the selected vegetation metric.

**Accumulated Change**

Accumulated change is the total change in the selected vegetation index that occurred during a time period. For islands without a modeled conservation intervention (i.e., not invasive species eradication), we predict accumulated change across the full time series. For islands with a modeled conservation intervention, we predict accumulated change in the pre-intervention period, post-intervention period, and full time series. We also predict the difference in accumulated change between the post- and pre-intervention periods (i.e., dynamic baseline). The dynamic baseline can be used to evaluate the effects of the intervention on the selected vegetation metric.

**Probability level**

We use the probability levels as filters so that the map only displays pixels where it is confident about a directional trend (i.e., positive or negative change). This means that the map only displays pixels where the model is confident that the change is a real trend, as opposed to statistical noise. For example, a probability level of 85% tests pixels against the 85% highest posterior density interval. The map will only display pixels with at least an 85% probability of a directional change. Pixels with < 85% probability of a directional change are not shown on the map.

This is the same probability level as the one used in the "3. Classified Change" and "4. Management Questions" tabs.

#### **How to Use**

In this module, you can view model-predicted change for a vegetation metric. Start by selecting an island name from the drop-down menu. Next, select a vegetation metric and a trend type. For islands without a modeled intervention, only one trend type will be available (pre-intervention or post-intervention, depending on if a conservation intervention occurred before the modeled time series). For islands with a modeled intervention, the trend type options include Fulltime, Pre-Intervention, Pre/Post1 Delta, and Post-Intervention 1. Islands with a second modeled intervention also offer Post1/Post2 Delta and Post-Intervention 2.

Finally, select a probability level. The probability represents the level of confidence that a modeled trend is a real trend, rather than statistical noise. The models used for Island Vital Signs do not predict a single value for each pixel. Instead, they predict a distribution of plausible values. We summarize that distribution with a highest posterior density interval (HPDI), or the narrowest range of values holding the chosen share of the model's probability. If a pixel's 85% HPDI does not include zero, then at least 85% of that probability falls to one side of zero. This means that there is at least an 85% probability of a directional change. Looking at different probability levels allows you to explore both tentative and model-supported patterns of change.

The "Island Info" box provides information about the island that you selected, including its name, country, and area. It also shows information about any invasive species eradication operations included in the model as an intervention.

The app will automatically generate a map based on your selections. The left side of the map will display all pixels that have been modeled for your selected vegetation metric. The right side of the map will only display pixels that meet your chosen probability level. You can slide the grey slider bar to the right to fully view the map of all modeled pixels. You can also slide the grey slider bar to the left to fully view the map of only the pixels that meet your chosen probability level.

Click a pixel on the map to see its location. Below the map, the "Selected Pixel" box will provide information about the selected pixel, including its modeled change value. Two time series plots below the text box will show the selected pixel's values over time. The plot on the left shows the smoothed trend (i.e., the overall trend). The plot on the right shows the trend with annual variation. Use the "Expand Map" button to view the map at full size. In the top right corner of the map, select the layer control button to switch the basemap between Dark (default), Open Street Map, or satellite imagery.

If you would like to download the classified rasters as a GeoTIFF, select the "Download Data" button.

#### **Interpretation**

Each pixel on the map is color-coded according to how the vegetation is changing in that pixel over the selected trend type (i.e., time period). Positive values mean that the vegetation metric is increasing. Negative values indicate that the vegetation metric is decreasing. Small values close to zero indicate a smaller magnitude of change, and larger values indicate larger magnitudes of change.

If a pixel is present on the left side of the map but does not appear on the right side of the map, the model is not confident about a directional change for that pixel at the selected probability level.

The pixel-level time series results are shown below the map as two graphs. The left graph shows a smoothed trend, which reduces background noise and variation to show the underlying pattern of change, such as if vegetation is increasing or decreasing. The right graph shows the trend with annual variation. In some instances, both graphs may be quite similar, indicating minimal interannual variation at the selected location. In other instances, these graphs may be very different, highlighting interannual variation.

For both graphs, the average trend is represented by a black line. The grey shaded area around the trend line is the 85% highest posterior density interval: the model places 85% of its probability inside that band. The points on the graph represent the actual measurements estimated from Landsat data. If a graph has a vertical orange dashed line, this marks the year that a modeled conservation intervention occurred.
