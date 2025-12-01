This file outlines the steps needed to reproduce the calculations associated with the paper

Open R and install the REALIO package provided alongside the research article.
Make sure to install it by giving the correct input directory for the location of the package file

Open the R script file Results_clean.R. This is the file we'll use to get results.

The first command in the file assigns the directory where data i supposed to be located.
Change this directory to where all the data files will be located.
You will need the ICIO files from OECD named according to their default schema, i.e. "YYYY_SML.csv" where YYYY represents the year.

If you have downloaded the ICIO files directly from the OECD and not used the zip version provided with this article, it'll not have the Leontief Inverse matrices included.
You can calculate them by using the function calc_L_mat()
You can give the function a range (e.g. 1995 to 2020) and it'll output a list with L matrices for every year as an element of the list.
Save those matrices in 'rds' format following the naming scheme: "L_mat_YYYY.rds", where YYYY represents the year.

Make sure you have the ICIO csv files, the L matrix rds files, the concordance ICIO_BEC_conc.csv file, the IMF-WEO population outlook pop.csv file in your data folder.

Once your data is in place and the data_dir set to the correct location, you can run the file.
These calculations can take more than 10 minutes altogether, therefore we recommend running the script in chunks of 10-20 lines at a time.