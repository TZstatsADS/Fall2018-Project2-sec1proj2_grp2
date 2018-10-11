# Project 2: Shiny App Development Version 2.0

![screenshot](data/screenshot.png)

Term: Fall 2018

+ Team : Section 1, Group 2
+ **NYC Taxi Data**: 
+ Team members
	+ Yingqiao Zhang yz3209@columbia.edu
	+ Hongru Liu hl3148@columbia.edu
	+ Liu Han lh2862@columbia.edu
	+ Hengyang Lin hl3116@columbia.edu
	+ Bailey Pierson bp2471@columbia.edu

+ **Link to app**:
 https://aaronlin.shinyapps.io/app3/

+ **Project summary**: Building upon a prior version of an application, we improved app usability by collapsing multiple tabs into one central window for interactive map exploration and by extending functionality to include new data elements and well as a heat map visualization and statistical Analysis.

+ **Project Improvement**: We improved this project by adding one function and two statistical exploration. 
  + The function is added in interactive map. In details, users could look over taxi pick-up or drop-off density locally by showed heatmap, or could search best locations for them to get a taxi with in 250 meters. The locations are recommended by kmeans algorithm using pick-up and drop-off data in three hours, and we provide a button for users to locate themselves as well as a tool to measure the distance between the point of departure and the points we recommended. 
  + The two statistical explorations are added in two new taps, which consist of heatmaps (focus on taxi driving direction analysis) and statstical graphs (focus on fare). From these exlorations, users are able to get a rough estimation on how much fare and time they would cost. 
  + The layers of map have also been adjusted to make the interaction more clearly and logically.

+ **Contribution statement**: All team members approve our work presented in this GitHub repository including this contributions statement. The tab ""Interactive Map", tab "Dynamic Map" and tab "Data Explorer" are from old version. The contribution for every team member is as following:
	+ Yingqiao Zhang: Write heatmap function and Scatter plots in tab "Statistical Analysis" (server.R); Add Layers in tab "Dynamic Map" (ui.R); Presenter
	+ Hongru Liu: Arrange code (global.R); Integrate functions to tab "Interactive Map" (ui.R); Adjust UI (ui.R)
	+ Liu Han: Write heatmap function in tab "Localized Map" (server.R).
	+ Hengyang Lin: Design and write tab "Localized Map" and tab "Statistical Analysis" (Both server.R and ui.R); Write Picking-up taxi position recommendation function (server.R).
	+ Bailey Pierson:
	
Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
└── output/
```

Please see each subfolder for a README file.

