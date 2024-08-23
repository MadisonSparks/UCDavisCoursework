#1.a
t.test(Response ~ Treatment, data=DRP, conf.level=.95)
#(-18.67588, -1.23302)

#1.b
t.test(Response ~ Treatment, data=DRP, conf.level=.95, alternative="less")
#t-value = -2.310889

#1.c
#p-value= 0.01319121

#1.d
#we reject the null because alpha>p-value


#2.a
iqdata=lm(iq~group, data=IQ)
anova(iqdata)
# f-value=20.016

#2.b
# p-value= 7.843e-07