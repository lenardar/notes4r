model in "model.txt"
data in "data.txt"
compile, nchains(3)
parameters in "inits1.txt", chain(1)
parameters in "inits2.txt", chain(2)
parameters in "inits3.txt", chain(3)
initialize
adapt 1000
update 1000
monitor alpha, thin(1)
monitor beta1, thin(1)
monitor beta2, thin(1)
update 5000
parameters to "out1.Rdump", chain(1)
parameters to "out2.Rdump", chain(2)
parameters to "out3.Rdump", chain(3)
coda *, stem(sim.1/CODA)
samplers to sim.1/samplers.csv
update 0
model clear
exit
