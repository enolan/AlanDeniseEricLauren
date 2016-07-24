AlanDeniseEricLauren is an implementation of the ADEL algorithm for efficiently
finding the minimal subset of an input set satisfying some arbitrary
upward-closed property. "Upward-closed" means if the property is true of some
set S it is true of all supersets of S. My implementation is trivially extended
to maps (dictionaries).

This can be used for e.g. narrowing down bugs by finding the minimal subset of
a complex failing test case that still exhibits the issue. In addition, I
provide a method for finding the minimal set of *changes* between a known-good
and known-bad example needed to trigger a bug. (Equivalently, a set where the
property is false and a set where it's true.)

The ADEL algorithm is due to Philippe Laborie in his paper "An Optimal Iterative
Algorithm for Extracting MUCs in a Black-box Constraint Network" published in
ECAI 2014. doi:10.3233/978-1-61499-419-0-1051. The paper is available at
http://ebooks.iospress.nl/publication/37115.

The project's homepage is https://github.com/enolan/AlanDeniseEricLauren. Bug
reports and PRs can be submitted there.

As of July 2016, I am looking for work. If your company needs a good programmer,
my email is echo@echonolan.net. My resume is available
[here](http://www.echonolan.net/resume/cv.html).
