Photocop
========

A simple program to sort and copy photos from one directory to another

It was work in progress and I didn't work on it for a long time...
(I'm not even sure I pushed the last version)

The idea was

1) Make a list of all jpegs in a directory and subdirectories
2) fetch their date and time according to EXIF data
3) sort them by groups. A photo belongs to a group if it's within a set
   period from the last photo in that group
4) copy the photos to the destination directory, group by group, creating
   new directories as needed.
   directories created were "yyyy/yyyy-mm-dd"
   if there is several photo groups the same say, more directories
   are created with a name like "yyyy/yyyy-mm-dd n"
   where yyyy is the year, mm the month, dd the day, and n an increasing number.


