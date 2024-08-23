# Madison Sparks sta 100 hw 6
#1
Ybar = aggregate(iq ~ group, data = IQ, mean)$iq
ni = aggregate(iq ~ group, data = IQ, length)$iq

the.model = lm(iq ~ group, data = IQ)
ANOVA.table = anova(the.model)
mse = ANOVA.table[2,3]

alpha = 0.05
I = 3 
k = choose(I, 2)
n. = sum(ni)
t_ag = qt(1-.05/6,42)

BON12 = c(Ybar[1] - Ybar[2] - t_ag*sqrt(mse*(1/ni[1] + 1/ni[2])), 
          Ybar[1] - Ybar[2] + t_ag*sqrt(mse*(1/ni[1] + 1/ni[2])))
BON13 = c(Ybar[1] - Ybar[3] - t_ag*sqrt(mse*(1/ni[1] + 1/ni[3])), 
          Ybar[1] - Ybar[3] + t_ag*sqrt(mse*(1/ni[1] + 1/ni[3])))
BON23 = c(Ybar[2] - Ybar[3] - t_ag*sqrt(mse*(1/ni[2] + 1/ni[3])), 
          Ybar[2] - Ybar[3] + t_ag*sqrt(mse*(1/ni[2] + 1/ni[3])))

pvalue12 = 2*pt(abs(Ybar[1] - Ybar[2])/sqrt(mse*(1/ni[1] + 1/ni[2])), df = n.-k, lower.tail = F)
pvalue13 = 2*pt(abs(Ybar[1] - Ybar[3])/sqrt(mse*(1/ni[1] + 1/ni[3])), df = n.-k, lower.tail = F)
pvalue23 = 2*pt(abs(Ybar[2] - Ybar[3])/sqrt(mse*(1/ni[2] + 1/ni[3])), df = n.-k, lower.tail = F)
allpvalues = c(pvalue12, pvalue13, pvalue23)

BON_all = rbind(BON12, BON13, BON23)
all.result = cbind(BON_all, allpvalues)
rownames(all.result) = c("A vs. B", "A vs. C", "B vs. C")
colnames(all.result) = c("Lower", "Upper", "P-value")
knitr::kable(all.result)



#2
hand.table = table(hand$wash)
hand.table
y=hand.table[2]
n=sum(hand.table)

prop.test(y+2,n+4,conf.level = 0.99,correct = FALSE)$conf.int
