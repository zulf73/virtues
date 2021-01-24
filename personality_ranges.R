# what is the range
# of personalities in the human race?
library(ghyp)
library(pracma)
library(dplyr)
Bvals = c( 25, -8, -4, -3, -9,
           3, 10, 11, 5, -3,
           5, 10, 14, -3, -7,
           -5, 3, 1, 10, -5,
           1, -4, -11, 4, 11)

B = matrix( Bvals/100.0, nrow=5 )

# fitted parameters of big five
lambda <- 1.000000
alpha.bar <-  1.788706

mu <-c( 3.053392,
        3.021512, 
        3.183587,
        3.154377,
        3.308383)

sigmaBaseVec <- c( 0.1314515387, 0.0003453978, 0.03400488, 0.02127298, 0.01147807,
                   0.0003453978, 0.5063454311, 0.05195018, 0.06297821, 0.02006035,
                   0.0340048782, 0.0519501804, 0.14269251, 0.03386428, 0.02592150,
                   0.0212729771, 0.0629782131, 0.03386428, 0.16951071, 0.04414651,
                   0.0114780685, 0.0200603541, 0.02592150, 0.04414651, 0.16969360 )

sigmaBase <- matrix( sigmaBaseVec, nrow=5)
sigmaBase <- sigmaBase + diag(5)*0.30

gamma <- c( -0.030213436, 0.002608754, -0.028674928, -0.031492846, -0.043415295)

prob.fem <- ghyp(lambda=1, alpha.bar=alpha.bar, mu=mu, sigma=sigmaBase, gamma=gamma)


big_five_to_burke_factors <-c(
  18, 16, 26, 33, 10,
  17, 0, 0, -55, 0,
  26, 18, 20, -16, -16,
  0, -38, 5, -46, 42,
  -19, 0, 15, -13, -40
)

PreFactor <- matrix(big_five_to_burke_factors/100.,
                    nrow=5, ncol=5,byrow=T)
# This is from Celia Burke Ph. D. 1997
# Oklahoma "Feminity Reformulated: Big Five 
# and Gender Role
GenTraitCorr <- c(77,-10,4,4,-2,
                  73,-21,0,-8,-8,
                  71,18,-19,1,1,
                  68,25,-3,-5,-5,
                  66,20,-9,-22,-22,
                  66,12,-2,20,20,
                  66,3,8,-10,-10,
                  65,23,-2,-15,-15,
                  63,13,0,-15,-15,
                  62,27,6,-5,-5,
                  61,1,-5,21,21,
                  60,9,28,-5,35,
                  60,33,1,-18,-2,
                  53,-2,27,-13,37,
                  49,16,16,8,0,
                  46,45,4,-9,3,
                  45,6,-34,13,10,
                  44,-12,8,-2,9,
                  18,72,7,-5,8,
                  19,70,8,2,8,
                  -4,65,6,0,2,
                  11,50,14,14,-2,
                  37,45,2,-12,-8,
                  25,44,-13,-22,3,
                  16,43,40,22,13,
                  38,42,2,-19,-3,
                  4,6,71,-4,-4,
                  21,7,70,-7,12,
                  17,34,62,8,31,
                  18,29,60,11,34,
                  0,29,56,49,11,
                  1,4,48,39,13,
                  22,31,48,26,7,
                  6,43,36,23,11,
                  14,14,28,60,0,
                  4,-5,6,53,-43,
                  8,-12,1,53,-24,
                  8,11,10,51,15,
                  4,-6,-21,50,7,
                  -4,25,41,50,30,
                  9,-4,-4,42,5,
                  -18,-4,11,43,29,
                  32,14,-9,24,68,
                  46,-3,17,-9,-65,
                  9,-2,22,16,57,
                  0,14,21,26,48
                  
)

B0 <- matrix( GenTraitCorr/100.0, ncol=5)

bsri_desc <- c(
  "warm",
  "tender",
  "gentle",
  "compassionate",
  "sympathetic",
  "helpful",
  "affectionate",
  "sensitive to needs of others",
  "eager to soothe hurt",
  "understanding",
  "friendly",
  "cheerful",
  "sincere",
  "happy",
  "likeable",
  "loyal",
  "yielding",
  "love children",
  "Self-sufficient",
  "Self-reliant",
  "independent",
  "individualistic",
  "reliable",
  "conscientious",
  "willing to take a stand",
  "truthful",
  "shy",
  "Soft-spoken",
  "have leadership abilities",
  "act as a leader",
  "dominant",
  "solemn",
  "strong personality",
  "assertive",
  "forceful",
  "moody",
  "jealous",
  "unpedictable",
  "secretive",
  "aggressive",
  "unsystematic",
  "conceited",
  "masculine",
  "feminine",
  "athletic",
  "competitive"
)


action_probs<-function( vf ){
  actVals <-c(
    29,-6,14,-4,-2,
    -5,33,29,-5,-15,
    -7,16,45,22,-7,
    8,4,24,18,-18,
    27,-22,-6,-9,53
  )
  actMtx <- matrix( actVals, nrow=5, ncol=5,byrow=TRUE)
  out <- actMtx %*% t(vf)
  out<-abs(out)/sum(abs(out))
  t(out)
}

virtue_desc <- c(
  "Appreciation",
  "Fairness",
  "Persistence",
  "Creativity",
  "Love",
  "Self-Regulation",
  "Gratitude",
  "Leadership",
  "Judgment",
  "SocialIntelligence",
  "Forgiveness",
  "Spirituality",
  "Teamwork",
  "Bravery",
  "Curiosity",
  "Kindness",
  "Hope",
  "Integrity",
  "Perspective",
  "Prudence",
  "Humor",
  "Humility",
  "LoveOfLearning",
  "Zest"
)

virtue_levels <- function( bfv ){

ivirtPFvec <- c(
  38, 14, 21, 16,	15,
  27, 18,	4,	25,	52,
  24,	2,	46,	15,	15,
  13,	40,	25,	37,	37,
  -17, -45,	-23,	-14,	-10
)


virtVec <- c(
  9, 11, 9,	11,	6,
  -4, 15,	7,	19,	11,
  16,12,	1,	15,	40,
  27,	8,	17,	8,	15,
  9,	3,	21,	22,	15,
  7,	15,	-3,	3,	16,
  19,	12,	11,	16,	19,
  19,	9,	16,	17,	13,
  4,	7,	-10,	12,	18,
  15,	13,	25,	20,	17,
  7,	26,	8,	19,	14,
  10,	12,	6,	17,	19,
  3,	7,	17,	15,	7,
  18,	-7,	9,	9,	9,
  28,	13,	15,	21,	24,
  6,	16,	20,	28,	17,
  21,	20,	14,	18,	22,
  8,	20,	7,	31,	24,
  19,	23,	7,	19,	21,
  5,	19,	-5,	25, 24,
  14,	12,	47,	17,	4,
  -11,	20,	-11,	18,	11,
  10,	4,	-4,	5,	9,
  18,	15,	12,	12,	27
)

virtMtx <- matrix( virtVec,ncol=5, nrow=24)
ivirtPFmtx <- matrix( ivirtPFvec, nrow=5, 
                      ncol=5)

virtPFmtx <- inv(ivirtPFmtx)
ans <- (virtMtx/100.) %*% (virtPFmtx/100.) %*% bfv
ans
}

prob.fem <- ghyp(lambda=1, alpha.bar=alpha.bar, mu=mu, sigma=sigmaBase, gamma=gamma)
vf <- rghyp( 2000, prob.fem )
jvf <- t(jungTransform( vf ))
typesf <- getPersonalityType( jvf, level = 2, ghfit )
pact <- action_probs( vf )
pspec <- B0 %*% PreFactor %*% t(vf)
pvirt <- virtue_levels(t(vf))
dfp <- data.frame( cbind( vf, pact, jvf,scale(t(pspec)),scale(t(pvirt)) ))
dfp[,"ptype"] <- typesf

names(dfp) <- c( "o", "c","e","a","n",
                 "po", "pc", "pe", "pa", "pn",
                 "j1", "j2", "j3", "j4", 
                 "j5", bsri_desc, virtue_desc,
                 "ptype")
dfp1 <- dfp %>% group_by( ptype ) %>% summarise_all(mean)
