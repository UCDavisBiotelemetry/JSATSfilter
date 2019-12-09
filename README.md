# JSATSfilter
An attempt to make a repo of any code versions of "teknofilter" we can dig up.

## ChangeLog summary
### data.frame version
JSATS filtering algorithm: data.frame version changelog. Dates may be circa. Functionality is not guaranteed at any archived stage.
Primary author is Gabriel Singer, with some early code written by Damien Caillaud

Version 2.3.2 (Matt Pagel version author)	2018-09-25
* Slight modification to PRI interval determination + cutoffs 

Version 2.3 (Matt Pagel version author)	2018-07-09
* Change in package loading to prevent RStudio from load-cycling/restarting of R
* Augmentation of periodic debugging output lines for large files
* Taglist Hex column name adjustment
* Separation of technology types into their own folders
* Change in Lotek serial number handling

Version 2.2.1 ATSfix lotekFixing (Colby Hause version editor, Matt Pagel assist)	2018-06-26
* Adjustment for serial number extraction for Lotek files
* Adjustment in handling of taglist file for Lotek

Version 2.2.1 ATSfix (Colby Hause version editor, Matt Pagel assist)	2018-06-19
* Major Bug Fix - TagID extraction from G72xxxx\#\# strings in ATS files corrected

Version 2.2b		2018-07-03
* Regression in taglist handling
****This may have been how we initially processed 2017 data****

Version 2.2a		2017-11-16
* Change in taglist handling for TagID column
* Added "Detection" to headers of ATS files
* Include all columns in ATS other than "Internal" and the "SiteName" columns and altered names to be more consistent with other datafile types

Version 2.1		2017-11-08
* Lubridate added to package requirements
* Fishing for ATS serial number adjusted
* Inclusion of additional ATS columns in cleaned output
* Change in filtering threshold

Version 2.0		2017-10-25 (or 09-29)
* Casting of TagID column to a character string to enforce neither factor/level nor number
* Correction to Lotek serial number extraction
* Handling of Lotek detection timestamps altered
* Lotek cleaning algorithm improved

Version 1.6		2017-09-27
* Lotek cleaning added, albeit ineffective
* Change in kept columns for ATS files
* Multipath check added for ATS
* Counting variable aids in output filename determination

Version 1.5		2017-09-26 (or 09-20)
* Reading for ATS xlsx files added

Version 1.4		2017-09-14
* Cleaning loop skips/short files with 0 detections of any tags in the taglist
* Change in output filename determination 

Version 1.3b	2017-08-24
* Adds in a "mode" function for determining the most frequent PRI encountered in a given window.
* Reads in a taglist file
* Can process SUM files in addition to JSTs
* Small date printing change

Version 1.3a	2017-06-29
* At this time the algorithm is intended to processes Teknologic JST files (cleaning and filtering)
