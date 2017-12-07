![ExxonMobil Refinery, Baton Rouge](https://upload.wikimedia.org/wikipedia/commons/e/e7/Exxon_Mobil_oil_refinery_-_Baton_Rouge%2C_Louisiana.jpg)


####My project: Who bears the burden of U.S. oil refining?

Some seven million Americans live near oil refineries. These “fenceline” or “frontline” communities face some of nation’s greatest burdens of pollution — stuff like toxins and lung-damaging particulates. 

As North America expands its role as a global petroleum producer, living in these communities will only become more dangerous. This data visualization documents the health risks that those who live closest to oil refineries face, and the demographic makeup of those people — who often are poorer and more diverse than average. 

As Dr. Robert Bullard, author of “Dumping in Dixie: Race, Class, and Environmental Quality,” put it: “What we have is energy apartheid, where poor communities and poor communities of color are still getting the dirtiest of the dirty energy.”

####My data and our challenges
I requested and received data from the U.S. Environmental Protection Agency on oil refineries and the demographic makeup of the communities surrounding them. This alone took a lot of wrangling — most of which I did toward the end of the summer and early on in the semester. This dataset alone, though, wasn't enough. Not only did I want to see where in the U.S. refineries are located and who's living around them, but I wanted to see who was living around the dirtiest refineries.

To this end, I searched for data that a). would quantify a refinery's pollution and b). would be compatible with the data I received from the EPA. One promising dataset was the U.S. Department of Energy's list of operating refineries by the number of barrels they refine per day. But after speaking with a DOE flak, I found out that (of course) DOE and EPA use two different systems of identifying refineries. Therefore, the data from each agency would be incompatible.

I settled on EPA data, from the Toxic Release Inventory, that quantifies each refinery's "on-site and off-site" release of toxins. A drawback of this metric is that it treats each toxin the same, when one pound of Toxin A may be much more dangerous than 100 pounds of Toxin B. However, it works better than greenhouse gas releases, for example, because those releases don't immediately affect the surrounding population.

After getting all the refinery data I needed — a project in itself — I spent more than a week cleaning it and preparing for a join. In order to get a common column on which to join, I had to join two separate TRI datasets and then join that with my original EPA demographics data. By the end of this process, I had one table with all of the demographic data for the communities within a 1, 3 and 5-mile radius of each refinery, along with the name of the refinery and the amount (in pounds) of on and off-site toxic release.

Next, I used the R tidycensus package to pull Census data on poverty and race. I then compared statewide rates of poverty and states' demographics to the area surrounding refineries. This showed me that the populations within 5 and fewer miles of a refinery tend to be poorer and more diverse than the state average. I used a map and a few charts to visualize this.

####My map and charts
This map and five charts show where in the United States refineries are located and concentrated, and they show the relationship between refinery location and the presence of poor, diverse communities. I used various R packages to create each of them.

######Where are U.S. refineries located?
This map, a last-minute addition to the bunch, is useful only for providing an overview of where in the United States refineries are located, and where the largest ones are. The circles are scaled by the amount of toxic waste each generates and the color indicates the percentage of people in the surrounding community who live in poverty. As you can see, refineries are all over the U.S. The plurality, however, are located in Texas, Louisiana and California. You'll also notice that there are a few large refineries in Delaware, New Jersey and Illinois. 

######Who lives near the country's largest, most polluting refineries?
The following charts represent the bulk of my work on the project (outside the initial data wrangling). These two charts show the 15 largest, most polluting refineries. I have sized the circles by the amount, in millions of pounds, that the refineries release. The points on the chart represent the state average (of either people in poverty or the racial minority population) and the arrows represent the percentage present in the communities within three miles of those refineries. 

It's clear from the first chart that the country's largest, most polluting refineries are located in communities that are much poorer than their state average. Take the ExxonMobil refinery in Baton Rouge, Louisiana, for example. Statewide, about 18 percent of people live in poverty. But near this refinery, more than 60 percent live in poverty. 

The relationship between refinery location and the population of racial minorities is less clear. The second chart shows a less dramatic difference in the statewide and fenceline minority populations. Although nine of the 15 most polluting refineries are located near communities more diverse than the state as a whole, the relationship appears weaker.

######In general, how do fenceline communities compare to their states?
These scatter plots show the overall relationship between refinery location and the economic and demographic makeup of a fenceline community (as opposed to the charts above, which highlighted only the 15 largest). It takes a bit of work to interpret them, so they may be more useful as reporting tools, rather than publishable visualizations.

The circles are sized according to the population of people in poverty or racial minorities.

The y-axis represents the amount, in millions of pounds, the refineries release. The x-axis shows the percentage of poor people or minorities in fenceline communities, compared to the state average. 

For example, in the first chart you'll see ExxonMobil releases about 2 million pounds of toxic waste and the community in a 3-mile radius is just over 3.5 times poorer than the state average. Again, this chart — showing the relationship between poverty and refinery location — is striking. You'll see that only 10 refineries are located in areas that aren't poorer than their state average. 

In the second chart, the relationship between refinery location and the presence of racial minorities is less dramatic. Seventy-five refineries are located in areas with fewer racial minorities. However, 91 refineries — 55 percent of them — are located near communities more diverse than the state. 

I created the third chart, which looks specifically at the relationship between refinery location and the population of African Americans, after reading that a 1991 EPA-commissioned report found that toxic waste-producing facility were located in predominantly African American communities. I wanted to see what the numbers were like now and for refineries in particular.

I found that, while less than 50 percent of refineries are located in communities with more than the statewide percentage of African Americans, there are many refineries located in areas with four, five, six, and seven times the black population share, compared to statewide demographics. One Texas refinery is located in a community with 7.7 times the black population share statewide — so much that the x-axis runs off my chart.


