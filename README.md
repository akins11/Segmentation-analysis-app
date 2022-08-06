### **Segmentation Analysis app** 
      
      This application can be used for performing market segmentation analysis 
      with the use cluster analysis, it consist of dividing a market into groups
      of similar characteristics like age, income, personality, purchasing behavior,
      general behavior etc. you can read more on market segmentation on [Wikipedia](
      https://en.m.wikipedia.org/wiki/Market_segmentation) and also 
      [cluster analysis](https://en.m.wikipedia.org/wiki/Cluster_analysis)
      
      The steps involved in running this analysis are::
      
      1. Uploading a clean data set which have all columns as variables and
         all rows as observations, rows with missing values will be droped.

      2. Select numerical (more preferred) and character variables, which will 
         be used for creating clusters within the data. You don't need to select 
         all the available variables in the data. Furthermore few number of variables 
         selected will result in less time for the algorithm to compute the analysis.
      
      3. Choosing a cluster algorithm and also the number of centers to 
         group the data into. center selection can be done by running an analysis
         which determin the optimal number of centers in the data or setting a 
         preferred number of centers manually. After all important inputs have been 
         supplied the next operation will be to run a cluster analysis.

      4. Creating cluster summaries using the avaliable variables in the dataset,
         although this is optional but it will help in creating a visual image of
         how the segments should turn out.

      5. Asign preferred segments to the data manually or use an aggregate
         summary. Results of the segments created can be downloaded and summaries
         of each segments can be viewed in the segmentation summary panel.
         
Link to the [Application](https://akinwandeayomide.shinyapps.io/SegmentationApp/)
