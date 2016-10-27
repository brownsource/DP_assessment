#### Import fonts and embed in PDF tester

#install.packages('extrafont')
#library(extrafont)
#font_import()
#fonts() # Vector of font family names
#fonttable() # Show entire table

# Only necessary in session where you ran font_import()
loadfonts()
# For PostScript output, use loadfonts(device="postscript")
# Suppress output with loadfonts(quiet=TRUE)

# Needed only on Windows - run once per R session
# Adjust the path to match your installation of Ghostscript
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")

# Print a test graph with the font
pdf("font_plot.pdf", family="Arial", width=4, height=4)
plot(mtcars$mpg, mtcars$wt, 
     main = "Fuel Efficiency of 32 Cars",
     xlab = "Weight (x1000 lb)",
     ylab = "Miles per Gallon")
dev.off()
embed_fonts("font_plot.pdf", outfile="font_plot_embed.pdf")


