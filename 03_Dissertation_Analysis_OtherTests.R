# sc1=knowledgeble
# sc2=trustworthy
# sc3=credible
# sc4=expertise

# RQ1. What is the effect of machine versus human journalist for news message with controversial topics on credibility?
# RQ2. Is source/message discredibility distinct from source/message credibility?
# RQ3. What is the relationship between source/message credibility/discredibility and hostile media effect?
# RQ4. What is the implication of machine-generated news on hostile media effect?
# And from the literature review and research questions, this study proposes the following hypotheses:
# H1: Source credibility, message credibility, trust and distrust exhibits discriminant validity in journalism context.
# H2a: Source credibility of machine-journalism is lower than that of human journalism.
# H2b: Message credibility of machine-journalism is lower than that of human journalism.
# H2c: Trust for machine-journalism is lower than that of human journalism.
# H2d: Distrust for machine-journalism is higher than that of human journalism.

# H31a: Issue stance predicts lower source credibility or trust.
# H31b: Issue stance predicts higher distrust.
# H4a: Issue stance predicts higher perceived bias.
# H5a: Source credibility or trust predicts lower hostile media effect.
# H5b: Distrust predicts higher hostile media effect.
# H6: Perceived bias predicts lower message credibility. 
# H7: Perceived positively predicts perception of fake news.
# H8: Fake news and media credibility are negatively related.

# H9a. Disposition to trust positively predicts trust.
# H9b. Disposition to distrust positively predicts distrust.

#DEMO
count(my.data$White)
count(my.data$Black)
count(my.data$Hispanic)
count(my.data$Asian)
count(my.data$Native)
count(my.data$Other)

my.data %>% # take the diamonds data.fram and group it
        dplyr::group_by(Gender, Group.G1I2E3) %>% # 56 groups
        dplyr::summarize(count = n())

ageANOVA<-aov(age~Group.G1I2E3,data=my.data)#these two are the same
summary(ageANOVA)
TukeyHSD(ageANOVA)
anova_apa(ageANOVA)

incomeANOVA<-aov(income~Group.G1I2E3,data=my.data)#these two are the same
summary(incomeANOVA)
TukeyHSD(incomeANOVA)
anova_apa(incomeANOVA)

#Preliminary analysis report
scANOVA<-aov(sc~Group.M1H2*Group.G1I2E3,data=my.data)#these two are the same
summary(scANOVA)
TukeyHSD(scANOVA)
anova_apa(scANOVA)

msgANOVA<-aov(msg~Group.M1H2*Group.G1I2E3,data=my.data)#these two are the same
summary(msgANOVA)
TukeyHSD(msgANOVA)
anova_apa(msgANOVA)

trustANCOVA<-aov(trust~d.trust+Group.M1H2*Group.G1I2E3,data=my.data)#covariate comes first
print(summary(trustANCOVA))
car::Anova(trustANCOVA, type = 2)
summary.lm(trustANCOVA)#contrasts
emmeans(trustANCOVA,~Group.M1H2*Group.G1I2E3)

distrustANCOVA<-aov(distrust~d.distrust+Group.M1H2*Group.G1I2E3,data=my.data)#covariate comes first
print(summary(distrustANCOVA))
car::Anova(distrustANCOVA, type = 2)
summary.lm(distrustANCOVA)#contrasts
emmeans(distrustANCOVA,~Group.M1H2*Group.G1I2E3)

# HMEprealtANOVA<-aov(HMEprealt~Group.M1H2*Group.G1I2E3,data=my.data)#these two are the same
# summary(HMEprealtANOVA)
# TukeyHSD(HMEprealtANOVA)
# anova_apa(HMEprealtANOVA)
HMEindexANOVA<-aov(HMEindexB~Group.M1H2*Group.G1I2E3,data=my.data)#these two are the same
summary(HMEindexANOVA)
TukeyHSD(HMEindexANOVA)
anova_apa(HMEindexANOVA)

HMEindexANOVA<-aov(HMEindexB~Group.M1H2*Group.G1I2E3,data=my.data)#these two are the same
summary(HMEindexANOVA)
TukeyHSD(HMEindexANOVA)
anova_apa(HMEindexANOVA)

# bias.2altANOVA<-aov(bias.2alt~Group.M1H2*Group.G1I2E3,data=my.data)#these two are the same
# summary(bias.2altANOVA)
# TukeyHSD(bias.2altANOVA)
# anova_apa(bias.2altANOVA)
IIIndexANOVA<-aov(IIIndex~Group.M1H2*Group.G1I2E3,data=my.data)#these two are the same
summary(IIIndexANOVA)
TukeyHSD(IIIndexANOVA)
anova_apa(IIIndexANOVA)

HMEANCOVA<-aov(HMEindexB~IIIndex+Group.M1H2*Group.G1I2E3,data=my.data)#covariate comes first
print(summary(HMEANCOVA))
car::Anova(HMEANCOVA, type = 2)
summary.lm(HMEANCOVA)#contrasts
emmeans(HMEANCOVA,~Group.M1H2*Group.G1I2E3)


PIANOVA<-aov(LibCon~Group.G1I2E3,data=my.data)#these two are the same
summary(PIANOVA)
TukeyHSD(PIANOVA)
anova_apa(PIANOVA)
#The main effect of news topic was not significant,  F(2, 393) =   0.65, p = .521, petasq < .01.

HMEindexregression<-lm(HMEindexB~LibCon, data=my.data)
summary(HMEindexregression)
apa.reg.table(HMEindexregression, filename = "HMELibcon.doc", table.number = 2)
# Regression analysis was used to test if the political ideology significantly predicted participants' hostile media perception. The results of the regression indicated weak but significant relationship (R2=.01, F(1,394) = 4.73, β = .05, p = .03). 

PIANCOVA<-aov(HMEindexB~LibCon+Group.G1I2E3,data=my.data)#covariate comes first
print(summary(PIANCOVA))
car::Anova(PIANCOVA, type = 2)
summary.lm(PIANCOVA)#contrasts
emmeans(PIANCOVA,~Group.G1I2E3)


#The one-way ANCOVA for hostile media index with political ideology as covariate and news topic as factor revealed main effect of article topic; F(2, 602) = .866, p = .421.

#H1: EFA
FactorVars<-c(
	"ben1", "ben2","ben3",
	"comp1","comp2","comp3","comp4",
	"int1","int2","int3","int4",
	"mal1","mal2","mal3",
	"incomp1","incomp2","incomp3",
	"dect1","dect2","dect3",
	"msg1","msg2","msg3",
	"sc1","sc2","sc3","sc4"
	)
factor.data1<-my.data[,(names(my.data)%in%FactorVars)]

FactorVars<-c(
	"ben1", "ben2","ben3",
	"comp1","comp2","comp3","comp4",
	"int1","int2","int3","int4",
	"msg1","msg2","msg3",
	"sc1","sc2","sc3","sc4"
	)
factor.data2<-my.data[,(names(my.data)%in%FactorVars)]

FactorVars<-c(
	"mal1","mal2","mal3",
	"incomp1","incomp2","incomp3",
	"dect1","dect2","dect3",
	"msg1","msg2","msg3",
	"sc1","sc2","sc3","sc4"
	)
factor.data3<-my.data[,(names(my.data)%in%FactorVars)]

parallel <- psych::fa.parallel(factor.data1, fm = 'minres', fa = 'fa')

apa.cor.table(factor.data1, filename="Table1_APA.doc", table.number=1)

allfactor<-fa(factor.data1, nfactors=5, rotate="oblimin", fm="miners")
print(allfactor)

# H2a: Credibility of machine-journalism is lower than that of human journalism.
H2a.<-t.test(sc~Group.M1H2, data=my.data, var.equal=TRUE)
tvalue<-H2a.$statistic%>%formatC(digits=2,format="f")
pvalue<-H2a.$p.value%>%formatC(digits=2,format="f")
df<-H2a.$parameter
md<-my.data%>%group_by(Group.M1H2)%>%dplyr::summarize(
    N=length(sc),
    Mean=mean(sc),
    SD=sd(sc)) 
td<-my.data%>%dplyr::summarize(
    Group.M1H2="Total",
    N=length(sc), 
    Mean=mean(sc),
    SD=sd(sc))
H2a.t<-rbind(md,td)
#H2b
H2b.<-t.test(msg~Group.M1H2, data=my.data, var.equal=TRUE)
tvalue<-H2b.$statistic%>%formatC(digits=2,format="f")
pvalue<-H2b.$p.value%>%formatC(digits=2,format="f")
df<-H2b.$parameter
md<-my.data%>%group_by(Group.M1H2)%>%dplyr::summarize(
    N=length(msg),
    Mean=mean(msg),
    SD=sd(msg)) 
td<-my.data%>%dplyr::summarize(
    Group.M1H2="Total",
    N=length(msg), 
    Mean=mean(msg),
    SD=sd(msg))
H2b.t<-rbind(md,td)

# H2c: Trust for machine-journalism is lower than that of human journalism.
H2c.<-t.test(trust~Group.M1H2, data=my.data, var.equal=TRUE)
tvalue<-H2c.$statistic%>%formatC(digits=2,format="f")
pvalue<-H2c.$p.value%>%formatC(digits=2,format="f")
df<-H2c.$parameter
md<-my.data%>%group_by(Group.M1H2)%>%dplyr::summarize(
    N=length(trust),
    Mean=mean(trust),
    SD=sd(trust)) 
td<-my.data%>%dplyr::summarize(
    Group.M1H2="Total",
    N=length(trust), 
    Mean=mean(trust),
    SD=sd(trust))
H2c.t<-rbind(md,td)

H2c.ben<-t.test(ben~Group.M1H2, data=my.data, var.equal=TRUE)
tvalue<-H2c.ben$statistic%>%formatC(digits=2,format="f")
pvalue<-H2c.ben$p.value%>%formatC(digits=2,format="f")
df<-H2c.ben$parameter
md<-my.data%>%group_by(Group.M1H2)%>%dplyr::summarize(
    N=length(ben),
    Mean=mean(ben),
    SD=sd(ben)) 
td<-my.data%>%dplyr::summarize(
    Group.M1H2="Total",
    N=length(ben), 
    Mean=mean(ben),
    SD=sd(ben))
H2c.ben.t<-rbind(md,td)

H2c.comp<-t.test(comp~Group.M1H2, data=my.data, var.equal=TRUE)
tvalue<-H2c.comp$statistic%>%formatC(digits=2,format="f")
pvalue<-H2c.comp$p.value%>%formatC(digits=2,format="f")
df<-H2c.comp$parameter
md<-my.data%>%group_by(Group.M1H2)%>%dplyr::summarize(
    N=length(comp),
    Mean=mean(comp),
    SD=sd(comp)) 
td<-my.data%>%dplyr::summarize(
    Group.M1H2="Total",
    N=length(comp), 
    Mean=mean(comp),
    SD=sd(comp))
H2c.comp.t<-rbind(md,td)

H2c.int<-t.test(int~Group.M1H2, data=my.data, var.equal=TRUE)
tvalue<-H2c.int$statistic%>%formatC(digits=2,format="f")
pvalue<-H2c.int$p.value%>%formatC(digits=2,format="f")
df<-H2c.int$parameter
md<-my.data%>%group_by(Group.M1H2)%>%dplyr::summarize(
    N=length(int),
    Mean=mean(int),
    SD=sd(int)) 
td<-my.data%>%dplyr::summarize(
    Group.M1H2="Total",
    N=length(int), 
    Mean=mean(int),
    SD=sd(int))
H2c.int.t<-rbind(md,td)

# H2d: Distrust for machine-journalism is higher than that of human journalism.
H2d.<-t.test(distrust~Group.M1H2, data=my.data, var.equal=TRUE)
tvalue<-H2d.$statistic%>%formatC(digits=2,format="f")
pvalue<-H2d.$p.value%>%formatC(digits=2,format="f")
df<-H2d.$parameter
md<-my.data%>%group_by(Group.M1H2)%>%dplyr::summarize(
    N=length(distrust),
    Mean=mean(distrust),
    SD=sd(distrust)) 
td<-my.data%>%dplyr::summarize(
    Group.M1H2="Total",
    N=length(distrust), 
    Mean=mean(distrust),
    SD=sd(distrust))
H2d.t<-rbind(md,td)

H2d.mal<-t.test(mal~Group.M1H2, data=my.data, var.equal=TRUE)
tvalue<-H2d.mal$statistic%>%formatC(digits=2,format="f")
pvalue<-H2d.mal$p.value%>%formatC(digits=2,format="f")
df<-H2d.mal$parameter
md<-my.data%>%group_by(Group.M1H2)%>%dplyr::summarize(
    N=length(mal),
    Mean=mean(mal),
    SD=sd(mal)) 
td<-my.data%>%dplyr::summarize(
    Group.M1H2="Total",
    N=length(mal), 
    Mean=mean(mal),
    SD=sd(mal))
H2d.mal.t<-rbind(md,td)

H2d.incomp<-t.test(incomp~Group.M1H2, data=my.data, var.equal=TRUE)
tvalue<-H2d.incomp$statistic%>%formatC(digits=2,format="f")
pvalue<-H2d.incomp$p.value%>%formatC(digits=2,format="f")
df<-H2d.incomp$parameter
md<-my.data%>%group_by(Group.M1H2)%>%dplyr::summarize(
    N=length(incomp),
    Mean=mean(incomp),
    SD=sd(incomp)) 
td<-my.data%>%dplyr::summarize(
    Group.M1H2="Total",
    N=length(incomp), 
    Mean=mean(incomp),
    SD=sd(incomp))
H2d.incomp.t<-rbind(md,td)

H2d.dect<-t.test(dect~Group.M1H2, data=my.data, var.equal=TRUE)
tvalue<-H2d.dect$statistic%>%formatC(digits=2,format="f")
pvalue<-H2d.dect$p.value%>%formatC(digits=2,format="f")
df<-H2d.dect$parameter
md<-my.data%>%group_by(Group.M1H2)%>%dplyr::summarize(
    N=length(dect),
    Mean=mean(dect),
    SD=sd(dect)) 
td<-my.data%>%dplyr::summarize(
    Group.M1H2="Total",
    N=length(dect), 
    Mean=mean(dect),
    SD=sd(dect))
H2d.dect.t<-rbind(md,td)

t_apa(H2a.)
H2a.t
t_apa(H2b.)
H2b.t
t_apa(H2c.)
H2c.t
t_apa(H2c.ben)
H2c.ben.t
t_apa(H2c.comp)
H2c.comp.t
t_apa(H2c.int)
H2c.int.t
t_apa(H2d.)
H2d.t
t_apa(H2d.mal)
H2d.mal.t
t_apa(H2d.incomp)
H2d.incomp.t
t_apa(H2d.dect)
H2d.dect.t

