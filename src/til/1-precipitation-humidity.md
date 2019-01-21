---
date: 2017-11-15
layout: layouts/blog.liquid
title: "Precipitation and Humidity"
tags: [TIL]
permalink: til/precipitation-humidity/
---

# Precipitation

Precipitation is just the falling of stuff (ice, snow, rain, etc.) due to [water cycle](https://en.wikipedia.org/wiki/Water_cycle).

<img class="ui large image" src="https://pmm.nasa.gov/education/sites/default/files/article_images/Water-Cycle-Art2A.png" alt="Watercycle" />

### What does the precipitation percentage on weather apps mean?

This confused me a bit, I used to think its just the probability that some form of *precipitation*is going to occur. But its a little more subtle... (Occur where?)

Precipitation is measured in terms of [Probability of Precipitation (PoP)](https://www.weather.gov/ffc/pop):

$$PoP = C * A$$

where
  **C** = the confidence that precipitation will occur somewhere in the forecast area, and where
  **A** = the percent of the area that will receive measureable precipitation, **if it occurs at all.**

Thus, if we see "Precipitation: 40%" in some weather app for the city of Austin, it can mean:

- The forecaster is sure that precipitation will occur, and expects that, if it does occur, it will produce rain over 40% of Austin, the PoP (chance of rain) is 40% (1 * 0.4)
- The forecaster is 80% sure that rain will occur, and expects that, if it does occur, it will produce rain over 50% of Austin, the PoP is 40% (0.8 * 0.5)
- Any combination of the two factors...

So the app is answering this question: "Given that I am in Downtown Austin, the chance of it raining *here* is 40%".

# Humidity

Humidity is a measure of how much water vapor is present in the air.
If there is more moisture in the air, its harder for your body to let the hot molecules fly away and cool you down.

Humidity helps answer "How does it feel outside?" Sticky, yucky, wet, etc. But how do you know if its gonna feel that way? Give me numbers...

2 ways of measuring Humidity
## Absolute humidity
$AH (g/m^3) = \frac{\text{mass of water vapour}}{\text{volume of air}}$
Very similar to density.
Absolute humidity is absolute, but hard to measure because of [gaslaw](https://www.wikiwand.com/en/Ideal_gas_law) $PV = nRT$. (Volume is affected by temperature)

## Relative humidity
$RH (\%) = \frac{\text{water vapor in air}}{\text{max amount possible (or saturation point)}}$

 The issue with Relative humidity is that it varies with temperature and is not an absolute measure. Higher the temperature, higher the saturation point. (More molecules of water can jostle around in gas state)
i.e. if RH is 50% at 60F and 65F, 65F will feel more humid. A better measure is **dewpoint**.

## Dewpoint
For a given amount of water vapor in air, dewpoint is the temperature at which the RH will become 100%.
In other words, its an absolute measure of humidity. Higher the dewpoint, the more humid it is.

Rule of thumb -> dewpoint > 18C (or 65F) means its gonna start being sticky and meh...

### References
- [Humidity wiki](https://www.wikiwand.com/en/Humidity)
- [SciShow](https://www.youtube.com/watch?v=SGHRz8wpj3E)

