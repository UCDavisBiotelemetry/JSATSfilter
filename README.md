# ChangeLog: data.table branch
JSATS filtering algorithm: data.table version changelog. Dates may be circa. Functionality is not guaranteed at any archived stage.
Primary author is Matthew Pagel, Gabriel Singer provided early versions and consulted

Version 2.5.3.2 2018-07-16
* Reorganization of runtime constants
* Added function to extract the serial number from the filename
* Better missing file handling
* Switch to fwrite for filtered (final) data
* If temp file from ATS xlsx->csv conversion exists, don't re-create
* Switch ATS to use generalized code

Version 2.5.3 2018-07-10
* More code reorganization
* Added columns to results and rejected data
* Even better handing for missing columns in TagFile

Version 2.5.2 2018-07-09
* Bugfix: don't eliminate first detection of each tag
* Better handling for missing columns in TagFile

Version 2.5.1.5 2018-07-05
* Bugfix: date time format string should not have both %S and %OS
* Better handling of missing serial numbers in ATS files
* Handle Lotek datestamps within generalized code
* Added "2.6" functions, but did not activate (memory issues)

Version 2.5.1.4 2018-06-25
* Code reordering, package library reduction
* Elimination of mode functions
* Comments added, debugging removed
* Added another header possibility for TagID
* Added default values for InnerWrap variables
* Made code more generalized (rolled Lotek into Teknologic)

Version 2.5.1.3.x 2018-04-13 through 2018-07-03
* Major Bugfix: Extract TagID from the proper part of the ATS files (2018-07-03 subversion)
* Elimination of redundant code by use of wrapper
* readTags converted to data.table (mostly)
* Added additional identifiers for blank/useless data lines
* Code added to merge partial seconds into the datetime column
* Set values for various technologies before calling wrapper function for clarity
* Colbycomputer version uses single tag file
* Removed "Detection" column from ATS datafiles (up until 2018-06-21)
* Handle Detection column prescence/absence more adaptively (2018-06-21)
* Handle ATS csv files in addition to xlsx (2018-06-21)
* Addition of release group code for TagFile (2018-06-21)

Version 2.5.1.2 2018-03-21
* Header of file modified to be more descriptive, other comments added
* More adaptive tag file handling
* Change of single tag file to array of tag files
* Handling of shore station data added
* Condensation of data table parsing from file
* WinProgressBar now guagues progress based on file sizes
* Data source helps determine what timezone should be used to interpret datestamps
* Improvement of indicies used while processing realtime files
* Shift to fread rather than read.csv to speed processing
* Multiple changes to realtime file cleaning
* Better handling for dput vs fwrite files
* Improvement of wrappers for cleaning, reduction in redundant code
* Regression: tag file cleared before filtration

Version 2.5.1.1.x 2018-01-21 through 2018-03-06
* Added realtime data handling (of preprocessed data) by 2018-01-21 version
* GS version: change in realtime file handling, output directory
* Added handling of realtime data in a more raw form in 2018-01-29 version
* Correction of realtime data to starting in UTC
* Condensation of mode function in 2018-03-06 version
* Stop removal of tags file from memory before filtration (2018-03-06)

Version 2.5.1 2017-11-27
* Adjustment of "FLOPFACTOR" to generate similar number of positive hits as Gabe's version
* Unmatched parenthesis fixed
* Elimination of some debugging code

Version 2.5 2017-11-27
* Addition and use of runtime constants for number of hits per window, timing of multipath

Version 2.4 2017-11-22
* Fix for data.table cleaned data file on re-read via dget
* Elimination of redundant and inefficient variables
* POSIXct timezone adjusted
* Addition of more WinProgressBars

Version 2.3b (first operational data.table version) 2017-11-31
* Runtime constants added
* Code added to properly parse data.table temporary files
* Much optimization of magicFunc re: data.table and elimination of data.frame reliant code
* Added logic to allow for direct processing of data without writing cleaned data out to a intermediate file
* Cleaning wrappers and counters added

Version 2.3a minor (inoperative) 2017-11-16
* Added working directory

Version 2.3a (inoperative) 2017-11-16
* Various portion of Gabe's version 2+ code brought in, including ATS and Lotek file handling and "functionized"
* Constants added
* Change in cleaned file output extension to distinguish data.table version from data.frame
* File iterators numbering improved for output files

Version 1.4c (inoperative) 2017-09-13
* Alternate mode determination
* Replacement for additional data.table lines in magicFunc

Version 1.4b (inoperative) prior to 2017-10-01
* "magicFunc" for filtering altered to incorporate data.table
* Various streamlining to SUM file processing

Version 1.4a (inoperative) 2017-08-31
* First data.table version
* Added constants for indicating cleaning of certain file types
* Changed loops to functions called from simpler loops, with redundant items removed
* SUM files (in addition to JSTs) checked for multipath

data.frame ancestors (Gabe Singer author)
Version 1.3b	2017-08-24
* Adds in a "mode" function for determining the most frequent PRI encountered in a given window.
* Reads in a taglist file
* Can process SUM files in addition to JSTs
* Small date printing change

Version 1.3a	2017-06-29
* At this time the algorithm is intended to processes Teknologic JST files (cleaning and filtering)
