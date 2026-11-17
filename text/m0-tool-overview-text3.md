#### Probability Levels

The maps in this app are interactive. In Modules 2, 3, and 4, you may have the option to choose how strict to set the confidence level (“probability level”) of the data. Each probability level reflects the associated highest posterior density interval, which is a range of model-predicted values that are likely to contain a true value for each vegetation metric.

Here is what each probability level means:

**0.75: Exploratory:** We are fairly (75%) certain that the model-predicted change is real, but there is a 25% chance that we could be wrong. Useful for spotting early signals that warrant additional investigation.

**0.85: Balanced:** We are moderately (85%) certain that the model-predicted change is real, though there is a 15% chance that we could be wrong. Intended to provide enough confidence to trust the results without overlooking emerging patterns.

**0.95: Strict:** We are highly (95%) certain the model-predicted change is real and there is a 5% chance that we could be wrong. Focuses only on the most certain changes.

We use the probability levels as filters so that you only view data where the model is confident about a predicted trend. This means that a module map will only display data where the model is confident that the predicted change is a real trend, as opposed to statistical noise. For example, selecting a probability level of 85% will test data against the 85% highest posterior density interval. The module map will then only display data with at least an 85% probability of a directional change.
