#### **Definitions**

This module answers five management-related questions using modeled results for your selected island. The maps produced in response to each question show what areas of the island answer the question e.g., where has productivity changed the most? When you select a question, the map will display pixels that answer the question. Pixels that do not meet the criteria for answering a question are not displayed.

In contrast to the other modules, **you do not need to choose a vegetation metric in this module**. Each question in this module relies on a predetermined vegetation metric. Most questions use a measure of productivity (i.e., NDVI, EVI, or MSAVI2). One question uses tree canopy cover.

**Where is productivity high and increasing over the last 5 years?**

Highlights pixels where median productivity across the last 5 years is in the top 25% of median values for that island **and** where the accumulated trend is positive (increasing) at the selected probability level. These pixels may signal areas of vegetation expansion and growth. Depending on island context, rapid vegetation growth could indicate vegetation recovery or encroachment.

**Where is productivity low and decreasing over the last 5 years?**

Highlights pixels where median productivity across the last 5 years is in the bottom 25% of median values for the island **and** where the accumulated trend is negative (decreasing) at the selected probability level. These pixels may signal areas of vegetation loss. Depending on restoration and conservation goals, these areas may require restoration action (such as when vegetation loss destabilizes sediment) or indicate areas where vegetation removal actions (such as removal of an invasive or overgrown plant species) have been effective.

**Where has productivity changed the most?**

Classifies pixels into two categories: areas with the greatest increase in productivity (top 25% of positive accumulated change values, i.e., the most positive accumulated change values) and areas with the greatest decrease in productivity (bottom 25% of negative accumulated change values, i.e., the most negative accumulated change values). These pixels highlight the strongest signals of change in either direction.

**Where does vegetation appear to resist change?**

Highlights pixels with no model-predicted trend (i.e., no directional change at the selected probability level) and low variability within predicted annual values for productivity (i.e., coefficient of variance for all annual productivity values within the time series is within the bottom 25%). These pixels signal areas where productivity is relatively stable, and does not increase, decrease, or fluctuate annually. Note that this question asks where a trend is *absent*, so the probability level reads the other way round here than it does elsewhere: a wider interval is easier to overlap with zero, so 95% classifies more pixels than 75% does, not fewer. On some islands no pixel qualifies at any level — see *Interpretation* below.

**Where is forest potentially regenerating or declining?**

Highlights pixels where tree canopy is increasing or decreasing. Areas of increasing tree canopy cover may signal potential forest regeneration while areas of decreasing tree canopy cover may signal potential forest decline.

**Probability level**

We use the probability levels as filters so that the map only displays pixels where it is confident about a directional trend. This means that the map only displays pixels where the model is confident that the change is a real trend, as opposed to statistical noise. For example, a probability level of 85% tests pixels against the 85% highest posterior density interval. The map will only display pixels with at least an 85% probability of a directional change. Pixels with < 85% probability of a directional change are not shown on the map.

The exception is the resist-change question described above, which tests for the absence of a directional trend rather than its presence, and so classifies more pixels as the level rises.

This is the same probability level as the one used in the "2. Change Over Time" and "3. Classified Change" tabs.

#### **How to Use**

Start by selecting an island name from the drop-down menu. If you already selected an island on another tab, that selection carries over to this module. Next, select a question and probability level. This is the probability level used to filter pixels from the map so that only pixels with at least the selected probability of a directional change are displayed. Once your selections are made, the app will run a matching function on the modeled data for your island. It will then generate a map with pixels that meet the criteria for answering the selected question.

The "Island Info" box provides information about the island that you selected, including its name, country, and area. It also shows information about any invasive species eradication operations included in the model as an intervention.

Click a pixel on the map to see its location. Below the map, the "Pixels Shown" box will provide information about the selected pixel and other pixels displayed on the map. Two time series plots below the text box will show the selected pixel's values over time. The plot on the left shows the smoothed trend (i.e., the overall trend). The plot on the right shows the trend with annual variation. Use the "Expand Map" button to view the map at full size. In the top right corner of the map, select the layer control button to switch the basemap between Dark (default), Open Street Map, or satellite imagery.

If you would like to download the classified rasters as a GeoTIFF, select the "Download Data" button.

#### **Interpretation**

In this module, the produced maps show **where** pixels answer a management question. The box below the map reports how many pixels meet the criteria for the selected question and the proportion of the island represented by classified pixels.

The first three productivity questions are **relative to the selected island**. This means they will always result in a map with classified pixels, and results should not be compared directly between islands.

In contrast, the forest trend question is **not** relative to the selected island. This question only produces a map when the model is confident about a directional change in tree canopy cover for at least one pixel on the island. As a result, this question may produce a map with many pixels, a moderate number of pixels, very few pixels, or no pixels. If no pixels meet the criteria for answering the question, the app will display the message "No pixels on this island answer the selected question."

The resist-change question is **half relative and half absolute**. The low-variability half is relative to the island (the least variable 25% of pixels), but the no-trend half is an absolute test, so it too can empty the map. On eight of the sixty-seven modeled islands — Mona and Rathlin among them — the model finds a directional trend in every pixel it modeled at the 75% and 85% levels, so this question returns no pixels there. Raising the level to 95% brings a handful back on four of those islands.

You should note that the first two questions combine a status test and a trend test (i.e., where is productivity high *and* increasing or where is productivity low *and* decreasing). As a result, the criteria for answering these questions are strict and a pixel must satisfy both criteria to be displayed on the map. If one of these questions returns the message "No pixels on this island answer the selected question.", you should consider using the "3. Classified Change" tab to visualize where productivity is high, where productivity is low, and where productivity is increasing or decreasing.
