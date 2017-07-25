# Data-raw

This folder contains raw data brought in for the project, as well as the [code](/wrangle) used to clean this data. Each data source has its own folder. ANES data never ended up being used, but is included here for posterity.

Files inside `wrangle` will not run on their own, because they are called from files within the [`/code`](../code) subdirectory. The file calls in `/wrangle` are written to reflect the working directory when running the files inside `/code`.

The NC voter files are not included here, because each is over 3 GB. These data are publicly available from the [North Carolina State Board of Elections](http://dl.ncsbe.gov/index.html). Code to download and recreate these files is found in [`/nc_voter`](/nc_voter).