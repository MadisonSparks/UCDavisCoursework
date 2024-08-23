#STA hw2
# Madison Sparks

#1.a
eye <- table(colors$Eye)
barplot(eye)
# Green eyes are least common.

#1.b
fmeye <- table(colors$Sex, colors$Eye)
barplot(fmeye, beside=TRUE, legend.text=c("Female","Male"))
#males have a higher probability of brown eyes

#1.c
table(colors$Sex)
mosaicplot(fmeye)
# There are more females in the study

#1.d
# there are is a higher probability of females having blue eyes

#1.e
# Green eyes are lest probable for males

#1.f
hist(colors$GPA)
# the most common interval of GPA is 3.3-3.4

#1.g
boxplot(GPA~Sex, data=colors)
# males have a lower median GPA

#1.h
#H males have more outliars

#1.i
boxplot(GPA~Eye, data=colors)
#Hazel eyes have the highest minimum

#1.j
#Brown eyes have the highest maximum