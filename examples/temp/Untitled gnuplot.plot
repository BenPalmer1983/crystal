#################################################################################
# Gnuplot
#
#################################################################################
# Output
#============================================
# 1. EspLaTeX monochrome
# set terminal epslatex monochrome
# set output "plot.tex"
# 2. png
# set terminal pngcairo size 1024,768 enhanced font 'Verdana,10'
# set output "plot.png"
# 3. ps - colour
# set terminal postscript eps enhanced color size 10,6 font 'Helvetica,20' linewidth 2
# set output "plot.eps"
# 4. ps - monochrome
# set terminal postscript eps monochrome enhanced blacktext  size 8,6
# set output "plot.eps"
#
#
set terminal postscript eps monochrome enhanced blacktext  size 8,6
set output "plot.eps"
#
# Set multiple plot layout
#============================================
set multiplot layout 1,1 rowsfirst
#
# Plot title
#============================================
set title "Title"
#
# Grid settings
#============================================
set grid xtics mxtics ytics y2tics mytics back
#
# Axes
#============================================
set xlabel "X-axis Label"
set ylabel "Y-axis Label"
set ytics nomirror tc lt 1
set y2tics nomirror tc lt 2
#
# Data file
#============================================
#set datafile separator "	"
set datafile separator ","
#
# Plot 1
#============================================
set title "Plot 1"
set label 1 '' at graph 0.92,0.9 font ',8'
set xlabel "X-axis Label"
set ylabel "Y-axis Label"
set y2label "Y-axis Label"
set ytics nomirror tc lt 1
set y2tics nomirror tc lt 2
plot \
'data.csv' using 1:2 with linespoints axes x1y1 title "Dataset 1", \
'data.csv' using 3:4 with linespoints axes x1y2 title "Dataset 2"
#
# Plot 2 etc
#============================================
