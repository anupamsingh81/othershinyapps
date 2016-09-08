
A = 18
B=30
C= 28
D=50

Y = ifelse(A>20 & B> 20,"betablocker",ifelse(A>20&B<20,"ace",ifelse(A<20&C>40,"Diuretic",ifelse(A<20&C<40&D<30,"Aspirin","Clopidogrel"))))
Y 