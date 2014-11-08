# flockture

There is a piece of genetic analysis software called 
[_flock_](http://www.bio.ulaval.ca/fileadmin/documents/Photos_professeurs/Julie_turgeon/Publications_PDF/DuchesneTurgeon_JoH_FlockKproblem.pdf) that claims
to be quite novel in its approach to clustering individuals on the basis
of their genotypes.  In a manuscript, we have showed with a simple mathematical
analysis that the underlying model
used in the FLOCK approach is one of the models implemented in the program
[_structure_](http://pritchardlab.stanford.edu/structure.html).

Another way of demonstrating the similarity between the programs is to
modify the source code os _structure_ so that it works like _flock_.
That is what we do here.  This is in development.


We have started with the _structure_ distribution as the very first commit.
We also have a branch called _structure_ that points to that first commit.

In the master branch we are making changes to convert structure to flock, i.e.,
flockture.

Note, don't get the _structure_ source from here.  It was written by Jonathan Pritchard
and his collaborators and is available here:

http://pritchardlab.stanford.edu/structure.html

It is posted here with permission.  

We repeat, don't use this as a source for structure's code.

## Running stuff

Here is a little primer on how to run it and get some results
from it.

1.  First make flockture
```sh
# from within src in the shell
make
```

```r
# then open RStudio and do this:
source("R/flockture.R")  # source some functions
```
Then execute the R commands in the file `fun_devel_stuff.R` to get a 
picture of how to use the package and run flockture.  

