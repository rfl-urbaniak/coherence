#This loads packages; if you require any, please add them there
source("utils//Libraries.R")

#This defines binary confirmation measures
source("utils//ConfirmationMeasures.R")

#This contains abbreviations used to define CPTs in a BN
source("utils//CptCreate.R")

#This contains the Robbers and its analysis
source("bns//Robbers.R")

#This contains the Penguin case (but it needs fixing)
source("bns//Penguins.R")

#This contains my attempts at coherence for two propositions, not sure which to choose and why
source("utils//RafalsMeasures.R")


### Let's think of things to DO and DISCUSS before we start writing:


#- Prepare the list of all the measures used together with references and perhaps one sentence of explanation for each
#- Prepare the list of desiderata as they arise from challenges (and so, a list of challenges), together with our assessment
#- figure out or own measure of coherence. Our problem was that the one that we used could not be negative; but I still think that the confirmation of the REST of the conjunction by each of the conjuncts sort of makes sense; I don't remember how this is realated to Douven's. I'm tired now and don't remember, why couldn't this one be negative?
#- apply the coherence measures to Sally Clark as originally from the book. Do all of them the intuitive result?
# - figure out correlation of coherence with which values we're interested in
  # - prior of the whole scenario?
  # - mean prior of each element?
  # - posterior of the whole scenario given optimal evidence?
  # - mean posterior of each element, given optimal evidence?
  # - anything else?
#- investigate these correlations using scatterplots and Pearson's R with singificance testing, based on randomized BNs resulting from Sally Clark
    # - do we want to randomize all CPTs? 
    # - do we want to keep some of the CPTs fixed? (say, unconditional ones)
#- Think back about the usual paradoxes and difficulties and which and how we would resolve.