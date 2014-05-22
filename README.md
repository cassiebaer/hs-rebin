# hs-rebin

hs-rebin is a simple Haskell program for rebinning binned data without the original sample elements.

This is an implementation of a "sweep"-style algorithm in which we sweep from left to right along the X-coordinate.
Tallies from split bins are divided according to a linear weight based on the amount of overlap two bins share.

The current implementation reads all necessary data from three separate files to populate x1, y1, x2 respectively.
The filenames should be provided as command-line arguments.

Writing a new Main executable would be easy for anybody who has different needs; this was just a quick job for a friend in need.

Sample Usage:

```
hs-rebin x1.txt y1.txt x2.txt
```