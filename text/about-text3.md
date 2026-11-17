Landsat data are collected at a 30 m spatial resolution, meaning each pixel represents a 30 m x 30 m area on the ground. When possible, we analyze the data at the same resolution. However, for larger islands (i.e., islands greater than 5 sq km in area), the time needed to analyze the data at this resolution is too long. To reduce analysis time for large islands, we group together neighboring pixels and assign the median value of the pixels in each group. This is a process known as aggregation.

For all islands, we do not analyze areas within 30 m of the coastline. Island coastlines often shift from year to year due to erosion and accretion. Thus, censuring each island’s coastal edge from the analysis ensures that we only captured vegetation change that occurs on land.

We then use time series analysis to detect how vegetation has changed on each island over time. For all islands included in the Island Vital Signs dataset, we use time series models to predict the long-term trend in tree canopy cover, canopy moisture, and productivity. For islands where an invasive species eradication has occurred within the modeled time series, we include the eradication operation as a conservation intervention within the model. This approach allowed us to separate the long-term background trend from vegetation changes associated with the intervention.

Thus, our models are able to detect:

- Long-term trends: How vegetation changed over time, including all years before (“pre-intervention”) and after (“post-intervention”) a conservation intervention, if applicable.
- Post-intervention changes: How the trend shifted after a conservation intervention occurred.

- Year-to-year carryover effects: How vegetation in one year influenced vegetation in the next year.

Our models also account for natural variation across years (due to, for example, annual difference in weather patterns) and differences between locations. From the model, we can detect island-wide patterns and local changes in vegetation.

Using these models, we can predict:

- Annual vegetation values: Model predicted values for tree canopy cover, canopy moisture, and productivity for each pixel in each year (“per-pixel annual vegetation”).

- Annual rates of change: Average amount of change per year (“annualized trends”). We calculate annual rates of change for the pre-intervention period, post-intervention period, and the full time series. Additionally, we show how the annualized trend changed from before to after a conservation intervention (“delta”).

- Total change: The amount that vegetation changed across a time period (“accumulated change”). We calculated change that occurred over the pre-intervention period, the post-intervention period, and the full time series. Additionally, we show how the total change trend changed from before to after a conservation intervention (“delta”).

### **Acknowledgments**

We would like to acknowledge the support from the Salesforce Nature Accelerator and NASA Biological Diversity and Ecological Conservation, Award 80NSSC23K1539, which provided the funding that made this decision-support tool possible. We also want to thank the following end-users who provided early input into framework design and validation: Lara Brenner, Paula A. Castaño, Cielo Figuerola, Katie Franklin, Nick Holmes, Paul Jacques, John Knapp, Stefan Kropidlowski, Amy Levine, Annie Little, Cozette Romero, Alex Wegmann, Cameron Williams, and Coral Wolf.

