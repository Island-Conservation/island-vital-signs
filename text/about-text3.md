For more information on our framework, please view our methods described by Wails et al. (In Prep) and Honzák et al. (2024).

To ensure we were capturing vegetation change on land, we left out areas within 30 meters of the coast. These coastal edges can shift from year to year due to erosion, storms, and other processes. Occasionally, clouds block the satellites' view, so not every pixel will have data for every year. We analyzed pixels that had at least five years of data (no invasive species removal) or at least ten years of data (at least one invasive species removed).

For larger islands (more than 5 km2 in area), we grouped neighboring pixels together ("aggregated") to reduce the time needed to analyze data. In this aggregation process, the aggregated pixel was assigned the median value of the original 30-m pixels.

We then used an interrupted time series analysis to detect changes in vegetation before and after a conservation intervention (usually, invasive mammal removal). This approach allowed us to separate the long-term background trend from changes associated with the removal itself. Our models account for:

-   Long-term trends: How vegetation changed over time, including years before the intervention ("pre-intervention") and years after the intervention ("post-intervention").

-   Post-intervention changes: How the trend shifted after the intervention.

-   Year-to-year carryover effects: Capture how vegetation in one year can influence the next.

Our models also account for natural variation across years (like unusual weather) and differences between locations. This allows us to detect both island-wide patterns and localized changes.

Using these models, we provide:

-   Annual vegetation values: These are model predictions of canopy cover, canopy moisture, and productivity for each pixel we included in analysis for each year ("per-pixel annual vegetation"). We use model predictions because this method allows us to reduce interference.

-   Annual rates of change: This is the average amount of change per year ("annualized trends"). We calculate annual rates of change for the pre-intervention period, post-intervention period, and the full time series. Additionally, we show how much the annual rates of change before and after an intervention ("delta").

-   Total change: This is how much vegetation has changed ("accumulated change"). We calculate how much change has occurred over the pre-intervention period, the post-intervention period, and the full time series. Additionally, we show how much the total change has changed before and after an intervention ("delta").

In the annualized trends and accumulated change modules, the map is split into two panels. The left side of the leaflet map shows the average change for each pixel we analyzed. The right side of the leaflet map shows only the locations where the change is clear enough that it's unlikely to be just noise or small fluctuations. We identify these clear signals of change using highest posterior density intervals, which is the range of values that contains the true mean. If the interval does not cross or include zero, it means we can be confident that there was a real increase or decrease.

Selecting 75% will show more areas that are changing, but it's likely that some of the area isn't truly changing. Selecting 95% will show fewer areas are changing, but we can be much more confident that those changes are real. Looking at different probability levels allows you to explore both the potential and confirmed patterns of change.

### **Acknowledgments**

We would like to acknowledge the support from the Salesforce Nature Accelerator and NASA Biological Diversity and Ecological Conservation, Award 80NSSC23K1539, which provided the funding that made this decision-support tool possible. We also want to thank the following end-users who provided early input into framework design and validation: Lara Brenner, Paula A. Castaño, Cielo Figuerola, Katie Franklin, Nick Holmes, Paul Jacques, John Knapp, Stefan Kropidlowski, Amy Levine, Annie Little, Cozette Romero, Alex Wegmann, Cameron Williams, and Coral Wolf.

