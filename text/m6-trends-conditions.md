#### **Definitions**

In this module, you can ask questions about vegetation trends and conditions for an island. The module includes a list of questions that ask what areas on the island meet certain criteria, e.g., where is a vegetation metric increasing. When you select a question, the map will display pixels that answer the question. Pixels that do not meet the criteria for answering a question are not displayed.

The six questions fall into three pairs.

**Condition questions — "Where is this metric high / low?"**

These questions look at the **full modeled time series**. For every pixel, we calculate the median value across all modeled years in the time series. We then compare values for all of the modeled pixels on the island. "High condition" refers to the top 25% of median values, while "low condition" refers to the bottom 25% of median values. These questions describe the **long-term baseline condition of selected vegetation metric on the island.**

**Status questions — "Where has this metric been high / low in the last 5 years?"**

These questions work the same as the condition questions, but we calculate the median values across **only the last 5 years** of the time series, rather than all years in the time series. Once again, we use "high status" to refer to the top 25% of median values and "low status" to refer to the bottom 25% of median values. These questions describe **recent conditions for the vegetation metric on the island**, which may differ from its long-term baseline condition if environmental or biological factors have changed.

**Trend questions — "Where is this metric increasing / decreasing?"**

These questions look at model-predicted **accumulated change**, rather than model-predicted annual values. We classify a pixel as "increasing" if its accumulated change is positive at the selected probability. We classify a pixel as "decreasing" if it is negative at the selected probability level.

**Probability level**

We use the probability levels as filters so that the map only displays pixels where it is confident about a directional trend. This means that the map only displays pixels where the model is confident that the change is a real trend, as opposed to statistical noise. For example, a probability level of 85% tests pixels against the 85% highest posterior density interval. The map will only display pixels with at least an 85% probability of a directional change. Pixels with < 85% probability of a directional change are not shown on the map.

This is the same probability level as the one used in the "2. Change Over Time" and "4. Management Questions" tabs.

#### **How to Use**

Start by selecting an island name from the drop-down menu, then select a vegetation metric. The metric list only contains metrics that have been modeled for the selected island. If you already selected an island on another tab, that selection carries over to this module.

Next, select a question. The app will run a matching function on the modeled data for your island and metric. It will then generate a map with pixels that meet the criteria for answering the selected question. If you select one of the two trend questions (i.e., where is this vegetation metric increasing/decreasing), you will also need to select a probability level. This is the probability level used to filter pixels from the map so that only pixels with at least the selected probability of a directional change are displayed.

The "Island Info" box provides information about the island that you selected, including its name, country, and area. It also shows information about any invasive species eradication operations included in the model as an intervention.

Click a pixel on the map to see its location. Below the map, the "Pixels Shown" box will provide information about the selected pixel and other pixels displayed on the map. Two time series plots below the text box will show the selected pixel's values over time. The plot on the left shows the smoothed trend (i.e., the overall trend). The plot on the right shows the trend with annual variation. Use the "Expand Map" button to view the map at full size. In the top right corner of the map, select the layer control button to switch the basemap between Dark (default), Open Street Map, or satellite imagery.

If you would like to download the classified rasters as a GeoTIFF, select the "Download Data" button.

#### **Interpretation**

In this module, the produced maps show **where** vegetation metric values are low, high, increasing, or decreasing, depending on the selected question. The box below the map reports how many pixels meet the criteria for the selected question and the proportion of the island represented by classified pixels (i.e., proportion of the island where the vegetation metric is low, high, increasing, or decreasing).

The condition and status questions are **relative to the selected island**. A pixel is classified as "high" condition or status when its value is in the top 25% of the selected island's modeled pixels, not because it crosses an absolute threshold. This means the condition and status questions will always result in a map with classified pixels, and results should not be compared directly between islands.

In contrast, the trend questions are **not** relative. These questions only produce a map when the model is confident about a directional change for at least one pixel on the island. As a result, these questions may produce a map with many pixels, a moderate number of pixels, very few pixels, or no pixels. If no pixels meet the criteria for answering a question (i.e., no pixels are increasing or no pixels are decreasing), the app will display the message "No pixels on this island answer the selected question."

Comparing answers to multiple questions may provide useful insight about changes in the vegetation community on an island. For example, pixels that are both "low" status and "decreasing" trend may signal areas where the vegetation community is degrading, while pixels that answer "low" status but "increasing" trend may signal areas that are recovering.
