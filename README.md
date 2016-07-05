# Flickr-API
R code to download data associated with photos uploaded on Flickr.
This code uses flickr.photos.search API tool to download the metadata associated with photos uploaded on Flickr. 
It includes loops through months and years to avoid API limitations in the number of results returned; loops through pages of results. 
It uses geographic restrictions and keywords to select which photos to download.
It returns a dataframe
