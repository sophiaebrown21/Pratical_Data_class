
#####PDA final paper code

##GG stats plot libs
library(ggstatsplot); packageVersion("ggstatsplot") #0.11.0
library(tidyverse); packageVersion("tidyverse") #2.0.0
library(ggpubr)
library(car)
install.packages("FSA")
library(FSA) #for Dunn test
##Violin plots libs
library(ggplot2)
library(colorspace)
hcl_palettes(plot=TRUE)
q4 <- qualitative_hcl(4, palette = "Dark 3")
#NMDS libs
library(vegan); packageVersion("vegan") #2.6-4
library(permute, lattice,)
library(ggplot2)
library(dplyr); packageVersion("dplyr") #1.1.1
library(ggdist)
library(ISwR)
library(lme4)
library(AICcmodavg)
library(colorspace)
library(tidyverse, MetBrewer)

setwd("/Users/sophie/Dropbox/My Mac (Sophia’s MacBook Air)/Desktop/PDA_final_paper_files")

###SCL analysis from Kelley et al
SCL_data = read.csv("~/Desktop/PDA_final_paper_files/Kelley_et_al_UCF_COPY.csv")
#what column headers mean
  #EID = assigned identifyer
  #FP = a binary list of whether a turtle had the tumor disease FP or not (0=no, 1=yes)
  #FP_YESNO = whether a turtle had the tumor disease FP or not (Yes or No)
  #Year = year that the sample was collected
  #SCL = Straight Carapace Length of the turtle sampled (in centimeters)
  #SCLbinned = the size bin that the turtle was included in based on 5 cm incrimental bins of SCL
  #Season = the northern hemisphere season in which the turtle was sampled (Spring, Summer, Fall, or Winter)
  #Columns with seasons broken out = binary list of 0 (not present) or 1 (present) for each season (for model purposes)
  #y0, y1, y2 = presence/absence recaptures 0 - 2 years after the first encounter (0 = not present; 1 = present).

#data checking
SCL_data %>% count(FP_YESNO, sort = TRUE)
#FP_YESNO    n
#1      Yes 2123
#2       No 2026

subset_FP = subset(SCL_data, FP_YESNO!="No") #subsetting to only FP points
subset_FP %>% count(Season, sort = TRUE)
#Season   n
#1 Winter 695
#2 Spring 570
#3   Fall 480
#4 Summer 378
SCL_data %>% count(Season, sort=TRUE)
#Season    n
#1 Winter 1339
#2 Spring 1155
#3 Summer  853
#4   Fall  802

boxplot(SCL~FP_YESNO, data=SCL_data) #SCL plot

ggbetweenstats(data = SCL_data, 
               x = FP_YESNO,
               y = SCL,
               outlier.tagging = TRUE,
               outlier.label = EID)

ggbetweenstats(data = SCL_data, 
               x = FP_YESNO,
               y = SCL,
               outlier.tagging = TRUE,
               outlier.label = EID,
               outlier.label.args = list(color = "red"), ## label color
               outlier.coef = 1.5,## coefficient for Tukey's rule 			
               ggtheme = ggplot2::theme_gray(), ## a different theme 
               package = "yarrr", ## package from which color palette 	p
               palette = "info2", ## choosing a different color palette 	
               title = "FP Incidence based on Straight Carapace Length (SCL)", 
               xlab = "Presence of FP",
               ylab = "SCL (cm)")

p1 <- ggplot(SCL_data, aes(x=FP_YESNO, y=SCL, fill=FP_YESNO)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(color="black", fill=NA)+
  scale_fill_brewer(palette = "Set3")+
  labs(
    title = "FP Incidence based on Straight Carapace Length (SCL)", 
    x = "Presence of FP",
    y = "SCL (cm)"
  )

p1 + stat_summary(fun.data=mean_sdl, mult=1, 
                 geom="pointrange", color="red")
  
p1

boxplot(SCL~Season, data=SCL_data) #Season plot

ggbetweenstats(data = SCL_data, 
               x = Season,
               y = SCL,
               outlier.tagging = TRUE,
               outlier.label = EID)

ggbetweenstats(data = SCL_data, 
               x = Season,
               y = FP,
               outlier.tagging = TRUE,
               outlier.label = EID,
               outlier.label.args = list(color = "red"), ## label color
               outlier.coef = 1.5,## coefficient for Tukey's rule 			ggtheme = ggplot2::theme_gray(), ## a different theme 
               package = "yarrr", ## package from which color palette 	p
               palette = "info2", ## choosing a different color palette 	
               title = "FP Incidence based on Season", 
               xlab = "Season",
               ylab = "Presence of FP",
               caption = "Whatever caption I want")

p2 <- ggplot(SCL_data, aes(x=Season, y=FP)) + 
  geom_violin()+
  scale_fill_brewer(palette = "Set3")+
  labs(
    title = "FP Incidence Based on Season", 
    x = "Season",
    y = "Presence of FP"
  )

p2 + stat_summary(fun=mean, geom="point", shape=23, size=2, color = "red", fill ="red")

p2

##Normality tests and Homogeneity tests
#SCL
ggqqplot(SCL_data$SCL) 
shapiro.test(SCL_data$SCL) #test
##	Shapiro-Wilk normality test
#data:  SCL_data$SCL
#W = 0.96011, p-value < 2.2e-16
#data is NOT normal

#Season
ggqqplot(SCL_data$FP)
shapiro.test(SCL_data$FP) #test
##Shapiro-Wilk normality test
#data:  SCL_data$FP
#W = 0.63642, p-value < 2.2e-16
#data is super NOT normal

#SCL
fligner.test(SCL~FP_YESNO, data = SCL_data)
#fligner.test(SCL~FP_YESNO, data = SCL_data)
#Fligner-Killeen test of homogeneity of variances
#data:  SCL by FP_YESNO
#Fligner-Killeen:med chi-squared = 384.56, df = 1, p-value < 2.2e-16

#Season
fligner.test(FP~Season, data = SCL_data)
#fligner.test(FP~Season, data = SCL_data)
#Fligner-Killeen test of homogeneity of variances
#data:  FP by Season
#Fligner-Killeen:med chi-squared = 19.572, df = 3, p-value = 0.0002082

# SCL data was not normal - run a Mann-Whitney U Test 
wilcox.test(SCL~FP_YESNO, data = SCL_data)
#Wilcoxon rank sum test with continuity correction

#data:  SCL by FP_YESNO
#W = 2780832, p-value < 2.2e-16 *
#alternative hypothesis: true location shift is not equal to 0

#Data was not normal for Season - run nonparametic tests - Kruskal-Wallis and Dunn 

kruskal.test(FP ~ Season, data = SCL_data)

#Kruskal-Wallis rank sum test

#data:  FP by Season
#Kruskal-Wallis chi-squared = 42.04, df = 3, p-value = 3.934e-09

dunnTest(FP ~ Season,
         data=SCL_data,
         method="bonferroni")

#Dunn (1964) Kruskal-Wallis multiple comparison
#p-values adjusted with the Bonferroni method.

#Comparison               Z      P.unadj        P.adj
#1   Fall - Spring  4.569378 4.891747e-06 2.935048e-05 *
#2   Fall - Summer  6.318348 2.643736e-10 1.586241e-09 *
#3 Spring - Summer  2.231546 2.564501e-02 1.538701e-01
#4   Fall - Winter  3.559691 3.712913e-04 2.227748e-03 *
#5 Spring - Winter -1.272064 2.033504e-01 1.000000e+00
#6 Summer - Winter -3.465735 5.287846e-04 3.172708e-03 *

###Algae Analysis from Van Houtan et al
algae_data = read.csv("~/Desktop/PDA_final_paper_files/VanHoutan_algae.csv")

algae_data %>% count(Location_type, sort = TRUE)
#Location_type    n
#1     eutrophic 13
#2  oligotrophic 12

ggbetweenstats(data = algae_data, 
               x = Location_type,
               y = Algae_Arg_N_per_dry,
               outlier.tagging = TRUE,
               outlier.label = Algae_sp,
               outlier.label.args = list(color = "red"), ## label color
               outlier.coef = 1.5,## coefficient for Tukey's rule 			ggtheme = ggplot2::theme_gray(), ## a different theme 
               package = "yarrr", ## package from which color palette 	p
               palette = "info2", ## choosing a different color palette 	
               title = "Arginine levels based on nutrient load", 
               xlab = "Nutrient level",
               ylab = "% Arginine in Algae per dry mass",
               caption = "Whatever caption I want")

p3 <- ggplot(algae_data, aes(x=Location_type, y=Algae_Arg_N_per_dry, fill=Location_type)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(color="black", fill=NA)+
  scale_fill_brewer(palette = "Set2")+
  labs(
    title = "Arginine levels based on nutrient load", 
    x = "Nutrient level",
    y = "%Arginine"
  )

p3


ggqqplot(algae_data$Algae_Arg_N_per_dry) 
shapiro.test(algae_data$Algae_Arg_N_per_dry) #test
#Shapiro-Wilk normality test
#data:  algae_data$Algae_Arg_N_per_dry
#W = 0.90887, p-value = 0.02876
#data not normal 

fligner.test(Algae_Arg_N_per_dry~Location_type, data = algae_data)
#Fligner-Killeen test of homogeneity of variances

#data:  Algae_Arg_per_dry by Location_type
#Fligner-Killeen:med chi-squared = 3.2798, df = 1, p-value = 0.07014
#can proceed with ANOVA

#anova
anova(lm(Algae_Arg_N_per_dry~Location_type,data=algae_data))  

#Analysis of Variance Table

#Response: Algae_Arg_N_per_dry
#               Df  Sum Sq  Mean Sq F value    Pr(>F)    
#Location_type  1 0.28042 0.280417  14.379 0.0009416 ***
#Residuals     23 0.44854 0.019502                      
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##NMDS code for algae data
class(algae_data$Algae_Arg_N_per_dry) #numeric
class(algae_data$X45_kg_diet)
class(algae_data$X45_kg_arg)#all numeric 
class(algae_data$Invasive_algae)

qual = algae_data[c(2,3,4,6)] #subset 
meas = algae_data[c(7,8,11,13)] #subset 

meas.mds = metaMDS(comm = meas, distance = "euclidean", trace = FALSE, autotransform = FALSE, k = 2)
plot(meas.mds$points)

#cut off values: higher than 0.2 = poor, 0.1-0.2 = fair, 0.05-0.1 = good
meas.mds$stress #0.001174328 #stress is good - close to zero though 

nmds = data.frame(meas.mds$points)
nmds = cbind(nmds,qual) #connecting for ggplot

#plot for sample island and nutrient load (location_type)
ggplot(nmds, aes(MDS1, MDS2)) +
  geom_point(size=3, stroke=1, color="black", #this line changes background of graph; also points 
             aes(shape = Location_type, fill = Sample_island)) + theme_classic(base_size=17,base_family="serif") + 
  scale_shape_manual(values = c(23,24)) + #this line changes fill color of points; makes the difference by sex and site
  scale_fill_manual(values = c("#8FC3F0","#A50F3F","orange")) + #custom colors 
  stat_ellipse(aes(linetype = Location_type, color= Sample_island),lwd=1.1) + #puts ellipses around points 
  scale_color_manual(values = c("#8FC3F0","#A50F3F", "orange")) #custom colors 
  guides(fill = guide_legend(override.aes = list(colors = c("#8Fc3f0", "#A50f3f", "orange"))))
  color = guide_legend(override.aes = list(shape = 21)) #creates legend 

#plot for invasive algae and nutrient load (location_type) - using this for final paper 
ggplot(nmds, aes(MDS1, MDS2)) +
    geom_point(size=3, stroke=1, color="black", #this line changes background of graph; also points 
               aes(shape = Location_type, fill = Invasive_algae)) + theme_classic(base_size=17,base_family="serif") + 
    scale_shape_manual(values = c(23,24)) + #this line changes fill color of points; makes the difference by sex and site
    scale_fill_manual(values = c("#8FC3F0","#A50F3F")) + #custom colors 
    stat_ellipse(aes(linetype = Location_type, color= Invasive_algae),lwd=1.1) + #puts ellipses around points 
    scale_color_manual(values = c("#8FC3F0","#A50F3F")) #custom colors 
  guides(fill = guide_legend(override.aes = list(colors = c("#8Fc3f0", "#A50f3f"))))
  color = guide_legend(override.aes = list(shape = 21)) #creates legend 

#Tukey test for Sample Island 
aovFactor_ArgN = aov(Algae_Arg_N_per_dry~Sample_island+Location_type, data = algae_data)
summary(aovFactor_ArgN)
#summary(aovFactor_ArgN)
#               Df  Sum Sq Mean Sq  F value  Pr(>F)   
#Sample_island  2   0.0628 0.03141   1.472 0.25220   
#Location_type  1   0.2181 0.21806  10.220 0.00434 **
  #Residuals     21 0.4481 0.02134                   
#---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
TukeyHSD(aovFactor_ArgN)
#  Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = Algae_Arg_N_per_dry ~ Sample_island + Location_type, data = algae_data)

#$Sample_island
#                   diff        lwr        upr     p adj
#Maui-Hawaii  0.16128571 -0.1170364 0.43960785 0.3294426
#Oahu-Hawaii  0.08633333 -0.2014906 0.37415724 0.7333595
#Oahu-Maui   -0.07495238 -0.2322584 0.08235366 0.4656605

#$Location_type
#                             diff        lwr         upr     p adj
#oligotrophic-eutrophic -0.1649267 -0.2865337 -0.04331981 0.0102487


#Algae sp v nutrient load ( location type) - this is for combined factors
aovFactor_ArgN2 = aov(Algae_Arg_N_per_dry~Algae_sp*Location_type, data = algae_data)
summary(aovFactor_ArgN2)
#summary(aovFactor_ArgN2)
#                       Df  Sum Sq Mean Sq F value   Pr(>F)    
#Algae_sp                4 0.26462 0.06615   8.279 0.000984 ***
  #Location_type           1 0.28089 0.28089  35.153 2.77e-05 ***
  #Algae_sp:Location_type  4 0.06358 0.01590   1.989 0.147935    
#Residuals              15 0.11986 0.00799                     
---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
TukeyHSD(aovFactor_ArgN2)
#Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = Algae_Arg_N_per_dry ~ Algae_sp * Location_type, data = algae_data)

#$Algae_sp
#                                   diff         lwr         upr     p adj
#A_spicifera-A_glomerata    0.050500000 -0.14468184  0.24568184 0.9269030
#H_musciformis-A_glomerata  0.290666667  0.09548483  0.48584851 0.0027348
#P_capilacea-A_glomerata    0.074000000 -0.15137657  0.29937657 0.8450953
#U_lactuca-A_glomerata      0.077285714 -0.11319226  0.26776369 0.7220660
#H_musciformis-A_spicifera  0.240166667  0.08080136  0.39953197 0.0024610
#P_capilacea-A_spicifera    0.023500000 -0.17168184  0.21868184 0.9954748
#U_lactuca-A_spicifera      0.026785714 -0.12678254  0.18035396 0.9817240
#P_capilacea-H_musciformis -0.216666667 -0.41184851 -0.02148483 0.0262975
#U_lactuca-H_musciformis   -0.213380952 -0.36694920 -0.05981270 0.0049535
#U_lactuca-P_capilacea      0.003285714 -0.18719226  0.19376369 0.9999979

#$Location_type
#diff        lwr        upr    p adj
#oligotrophic-eutrophic -0.2088713 -0.2851443 -0.1325984 3.27e-05

#$`Algae_sp:Location_type`
#                                                             diff         lwr
#A_spicifera:eutrophic-A_glomerata:eutrophic          1.276667e-01 -0.25171001
#H_musciformis:eutrophic-A_glomerata:eutrophic        4.456667e-01  0.06628999
#P_capilacea:eutrophic-A_glomerata:eutrophic          8.150000e-02 -0.32088974
#U_lactuca:eutrophic-A_glomerata:eutrophic            1.277500e-01 -0.23957989
#A_glomerata:oligotrophic-A_glomerata:eutrophic      -4.500000e-02 -0.44738974
#A_spicifera:oligotrophic-A_glomerata:eutrophic      -8.666667e-02 -0.46604335
#H_musciformis:oligotrophic-A_glomerata:eutrophic     7.566667e-02 -0.30371001
#P_capilacea:oligotrophic-A_glomerata:eutrophic      -3.100000e-02 -0.49563964
#U_lactuca:oligotrophic-A_glomerata:eutrophic        -6.000000e-02 -0.43937668
#H_musciformis:eutrophic-A_spicifera:eutrophic        3.180000e-01  0.04974018
#P_capilacea:eutrophic-A_spicifera:eutrophic         -4.616667e-02 -0.34609027
#U_lactuca:eutrophic-A_spicifera:eutrophic            8.333333e-05 -0.25085075
#A_glomerata:oligotrophic-A_spicifera:eutrophic      -1.726667e-01 -0.47259027
#A_spicifera:oligotrophic-A_spicifera:eutrophic      -2.143333e-01 -0.48259316
#H_musciformis:oligotrophic-A_spicifera:eutrophic    -5.200000e-02 -0.32025982
#P_capilacea:oligotrophic-A_spicifera:eutrophic      -1.586667e-01 -0.53804335
#U_lactuca:oligotrophic-A_spicifera:eutrophic        -1.876667e-01 -0.45592649
#P_capilacea:eutrophic-H_musciformis:eutrophic       -3.641667e-01 -0.66409027
#U_lactuca:eutrophic-H_musciformis:eutrophic         -3.179167e-01 -0.56885075
#A_glomerata:oligotrophic-H_musciformis:eutrophic    -4.906667e-01 -0.79059027
#A_spicifera:oligotrophic-H_musciformis:eutrophic    -5.323333e-01 -0.80059316
#H_musciformis:oligotrophic-H_musciformis:eutrophic  -3.700000e-01 -0.63825982
#P_capilacea:oligotrophic-H_musciformis:eutrophic    -4.766667e-01 -0.85604335
#U_lactuca:oligotrophic-H_musciformis:eutrophic      -5.056667e-01 -0.77392649
#U_lactuca:eutrophic-P_capilacea:eutrophic            4.625000e-02 -0.23828251
#A_glomerata:oligotrophic-P_capilacea:eutrophic      -1.265000e-01 -0.45504984
#A_spicifera:oligotrophic-P_capilacea:eutrophic      -1.681667e-01 -0.46809027
#H_musciformis:oligotrophic-P_capilacea:eutrophic    -5.833333e-03 -0.30575693
#P_capilacea:oligotrophic-P_capilacea:eutrophic      -1.125000e-01 -0.51488974
#U_lactuca:oligotrophic-P_capilacea:eutrophic        -1.415000e-01 -0.44142360
#A_glomerata:oligotrophic-U_lactuca:eutrophic        -1.727500e-01 -0.45728251
#A_spicifera:oligotrophic-U_lactuca:eutrophic        -2.144167e-01 -0.46535075
#H_musciformis:oligotrophic-U_lactuca:eutrophic      -5.208333e-02 -0.30301742
#P_capilacea:oligotrophic-U_lactuca:eutrophic        -1.587500e-01 -0.52607989
#U_lactuca:oligotrophic-U_lactuca:eutrophic          -1.877500e-01 -0.43868409
#A_spicifera:oligotrophic-A_glomerata:oligotrophic   -4.166667e-02 -0.34159027
#H_musciformis:oligotrophic-A_glomerata:oligotrophic  1.206667e-01 -0.17925693
#P_capilacea:oligotrophic-A_glomerata:oligotrophic    1.400000e-02 -0.38838974
#U_lactuca:oligotrophic-A_glomerata:oligotrophic     -1.500000e-02 -0.31492360
#H_musciformis:oligotrophic-A_spicifera:oligotrophic  1.623333e-01 -0.10592649
#P_capilacea:oligotrophic-A_spicifera:oligotrophic    5.566667e-02 -0.32371001
#U_lactuca:oligotrophic-A_spicifera:oligotrophic      2.666667e-02 -0.24159316
#P_capilacea:oligotrophic-H_musciformis:oligotrophic -1.066667e-01 -0.48604335
#U_lactuca:oligotrophic-H_musciformis:oligotrophic   -1.356667e-01 -0.40392649
#U_lactuca:oligotrophic-P_capilacea:oligotrophic     -2.900000e-02 -0.40837668
#                                                             upr     p adj
#A_spicifera:eutrophic-A_glomerata:eutrophic          0.50704335 0.9537524
#H_musciformis:eutrophic-A_glomerata:eutrophic        0.82504335 0.0154387
#P_capilacea:eutrophic-A_glomerata:eutrophic          0.48388974 0.9985060
#U_lactuca:eutrophic-A_glomerata:eutrophic            0.49507989 0.9442838
#A_glomerata:oligotrophic-A_glomerata:eutrophic       0.35738974 0.9999881
#A_spicifera:oligotrophic-A_glomerata:eutrophic       0.29271001 0.9963492
#H_musciformis:oligotrophic-A_glomerata:eutrophic     0.45504335 0.9986708
#P_capilacea:oligotrophic-A_glomerata:eutrophic       0.43363964 0.9999999
#U_lactuca:oligotrophic-A_glomerata:eutrophic         0.31937668 0.9997861
#H_musciformis:eutrophic-A_spicifera:eutrophic        0.58625982 0.0143581
#P_capilacea:eutrophic-A_spicifera:eutrophic          0.25375693 0.9998284
#U_lactuca:eutrophic-A_spicifera:eutrophic            0.25101742 1.0000000
#A_glomerata:oligotrophic-A_spicifera:eutrophic       0.12725693 0.5433266
#A_spicifera:oligotrophic-A_spicifera:eutrophic       0.05392649 0.1773831
#H_musciformis:oligotrophic-A_spicifera:eutrophic     0.21625982 0.9989316
#P_capilacea:oligotrophic-A_spicifera:eutrophic       0.22071001 0.8574442
#U_lactuca:oligotrophic-A_spicifera:eutrophic         0.08059316 0.3077925
#P_capilacea:eutrophic-H_musciformis:eutrophic       -0.06424307 0.0118080
#U_lactuca:eutrophic-H_musciformis:eutrophic         -0.06698258 0.0082538
#A_glomerata:oligotrophic-H_musciformis:eutrophic    -0.19074307 0.0007156
#A_spicifera:oligotrophic-H_musciformis:eutrophic    -0.26407351 0.0000843
#H_musciformis:oligotrophic-H_musciformis:eutrophic  -0.10174018 0.0038656
#P_capilacea:oligotrophic-H_musciformis:eutrophic    -0.09728999 0.0088625
#U_lactuca:oligotrophic-H_musciformis:eutrophic      -0.23740684 0.0001522
#U_lactuca:eutrophic-P_capilacea:eutrophic            0.33078251 0.9997332
#A_glomerata:oligotrophic-P_capilacea:eutrophic       0.20204984 0.9043145
#A_spicifera:oligotrophic-P_capilacea:eutrophic       0.13175693 0.5754873
#H_musciformis:oligotrophic-P_capilacea:eutrophic     0.29409027 1.0000000
#P_capilacea:oligotrophic-P_capilacea:eutrophic       0.28988974 0.9852330
#U_lactuca:oligotrophic-P_capilacea:eutrophic         0.15842360 0.7624583
#A_glomerata:oligotrophic-U_lactuca:eutrophic         0.11178251 0.4775364
#A_spicifera:oligotrophic-U_lactuca:eutrophic         0.03651742 0.1270461
#H_musciformis:oligotrophic-U_lactuca:eutrophic       0.19885075 0.9982027
#P_capilacea:oligotrophic-U_lactuca:eutrophic         0.20857989 0.8348106
#U_lactuca:oligotrophic-U_lactuca:eutrophic           0.06318409 0.2370751
#A_spicifera:oligotrophic-A_glomerata:oligotrophic    0.25825693 0.9999261
#H_musciformis:oligotrophic-A_glomerata:oligotrophic  0.42059027 0.8811789
#P_capilacea:oligotrophic-A_glomerata:oligotrophic    0.41638974 1.0000000
#U_lactuca:oligotrophic-A_glomerata:oligotrophic      0.28492360 1.0000000
#H_musciformis:oligotrophic-A_spicifera:oligotrophic  0.43059316 0.4816382
#P_capilacea:oligotrophic-A_spicifera:oligotrophic    0.43504335 0.9998840
#U_lactuca:oligotrophic-A_spicifera:oligotrophic      0.29492649 0.9999957
#P_capilacea:oligotrophic-H_musciformis:oligotrophic  0.27271001 0.9846773
#U_lactuca:oligotrophic-H_musciformis:oligotrophic    0.13259316 0.6933950
#U_lactuca:oligotrophic-P_capilacea:oligotrophic      0.35037668 0.9999996

aovFactor_ArgN2 = aov(Algae_Arg_N_per_dry~Algae_sp+Location_type, data = algae_data) #this is testing factors separately
summary(aovFactor_ArgN2)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Algae_sp       4 0.2646 0.06615   6.852  0.00137 ** 
  #Location_type  1 0.2809 0.28089  29.094 3.32e-05 ***
  #Residuals     19 0.1834 0.00965                     
---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
TukeyHSD(aovFactor_ArgN2)
#Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = Algae_Arg_N_per_dry ~ Algae_sp + Location_type, data = algae_data)

#$Algae_sp
#                                   diff         lwr          upr     p adj
#A_spicifera-A_glomerata    0.050500000 -0.15843745  0.259437448 0.9475689
#H_musciformis-A_glomerata  0.290666667  0.08172922  0.499604115 0.0040603
#P_capilacea-A_glomerata    0.074000000 -0.16726018  0.315260184 0.8847296
#U_lactuca-A_glomerata      0.077285714 -0.12661636  0.281187785 0.7838408
#H_musciformis-A_spicifera  0.240166667  0.06956995  0.410763379 0.0036372
#P_capilacea-A_spicifera    0.023500000 -0.18543745  0.232437448 0.9969262
#U_lactuca-A_spicifera      0.026785714 -0.13760539  0.191176819 0.9873669
#P_capilacea-H_musciformis -0.216666667 -0.42560411 -0.007729218 0.0398827
#U_lactuca-H_musciformis   -0.213380952 -0.37777206 -0.048989847 0.0075028
#U_lactuca-P_capilacea      0.003285714 -0.20061636  0.207187785 0.9999986

#$Location_type
#                             diff        lwr        upr p adj
#oligotrophic-eutrophic -0.2088713 -0.2911998 -0.1265429 4e-05


  