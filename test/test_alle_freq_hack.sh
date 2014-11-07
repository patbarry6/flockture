

# run this in the top directory of the repository.
# It shows that estimated allele frequencies are quite similar
# between FREQSCORR = 1 and FREQSCORR = 0.  The latter case
# is using our hack to make allele frequencies pretty much 
# like what you would get from Paetkau's method.  This just shows
# that we didn't introduce a bug when we did this.

cd src
make || (echo "Making failed!!"; exit 1)

cd ../data

# do it with freqscorr = 0
../src/structure -m testdata_mainparams.txt  -e extraparams_no_freqs_corr -o no_fc

# do it with freqscorr = 1
../src/structure -m testdata_mainparams.txt  -o with_fc

# then spit out the two output files side by side, adjusting things
# so that the Q-value and allele frequency lines line up.
# Visual inspection shows that it is all good.
awk '/Allele frequencies uncorrelated/ {printf("\n\n");} /^Locus 1 :/ {printf("\n");} {print}' no_fc_f | paste -   with_fc_f 



