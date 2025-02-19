a = c("lannister","targaryen","baratheon","starck","greyjoy")
length(a)
b = a[c(1,4)]
a[-1]
sort(a)

#exercice 2
a = c(1:100)
c(a,200,201,202)
b = seq(from = 2,to = 100, by=2)

#exercice 3
vec1 = seq(from=2,to=50,by=2)
vec2 = rep(c(0:9),each=3)
vec3 = rep(LETTERS,times = c(1:26))
length(vec3)

#exercice 4
paste("chr",1,sep="")
vec4 = paste("chr",c(1:22,"X","Y"),sep="")

#exercice 5
fac = factor(c("a","b","b","b","a","b","a","a"))
length(which(fac == "a"))
length(which(fac == "b"))
table(fac)

#exercice 6
a = rep(c(0,1),50)
A = matrix(a, ncol = 10,nrow = 10)
dim(A)
ncol(A)
nrow(A)
B = t(A)
line1 = A[seq(2,10,2),]
col0 = B[seq(1,10,2),]
 
#exercice 7
x = list()
x = list(a = rnorm(10), b = rep(1,10))
x[["a"]] 
x$a
y = as.data.frame(x)
z1 = y[c(1:3),]
z2 = y[c(8:10),]
class(z1)
class(z2)
x$chiffre = 1:26
as.data.frame(x)
# on ne peut pas transformer x en dataframe car on a ajouter une colonne chiffre qui a le nombre # de lignes 26 > 10 les nombres de lignes de x
                           
# Student's Sleep Data
data(sleep)
class(sleep)
table(sleep$group)
length(which(sleep$extra<0))
                        
s = rep(c("f","m","m","f","m","f","m","m","f","m"),2)
sleep2 = cbind(sleep,s)
colnames(sleep2)
colnames(sleep2)[4] = "sex"
table(sleep2$sex)
                         
# Exercice 8 
# Création de la matrice A
A = matrix(c(1, 2, 3, 4,
              5, 6, 7, 8,
              9, 10, 11, 12),
            nrow = 3,     
            ncol = 4)
print(A)
write.table(A,file = "matrice.txt")
write.table(A,file = "matrice.txt", row.names = FALSE, col.names = FALSE)
save(A, file = "matriceA.Rdata") 
#la matrice A est rechargé
C = load("matriceA.Rdata")
save(list = ls(),"données.Rdata")

#exercice9

#1
data(iris)
#2
p = table(iris$Species)
#3
pie(p)
barplot(p)
#4
summary(iris)
#5
boxplot(iris[,1:4])
#6
boxplot(iris[,1:4], title="Iris Boxplot", outline=FALSE)
#7
par(mfrow=c(1,2))
boxplot(iris[,1:4], title="Iris Boxplot", outline=FALSE)
pie(p)
#8
pdf("plotIris.pdf")
par(mfrow=c(1,2))
boxplot(iris[,1:4], title="Iris Boxplot", outline=FALSE)
pie(p)
dev.off()
#9
plot(iris$Petal.Length, iris$Petal.Width)
#10
plot(iris$Petal.Length, iris$Petal.Width, cex=5)
plot(iris$Petal.Length, iris$Petal.Width, pch=17, col="red")
abline(h=1, lty=2)
#9
pairs(iris[,1:4], col=as.numeric(iris$Species), pch=16)
#10
pdf("pairs_iris.pdf")
pairs(iris[,1:4], col=as.numeric(iris$Species), pch=16)
dev.off()


#exercice 10
data(airquality)
hist(airquality$Ozone)
hist(airquality$Ozone, freq=FALSE, main="Histogramme Ozone", xlab="Ozone", ylab="Densité", col = "gray",)
hist(airquality$Ozone, freq=FALSE, main="Histogramme Ozone", xlab="Ozone", ylab="Densité", col = "gray",breaks=30)
d = density(airquality$Ozone, na.rm=TRUE)
lines(d)

#exercice 11
#1
help("for")
#2
for (i in 1:10) print(i)
#3
somme = 0
for (i in seq(2,100,2)) {
  somme = somme + i
}
somme

#exerice 12
#1
help("if")
#2
x = 25
y = 0
if (x > 0){
  y = x^2
}else{
  y = x^3
}
#3
for (i in -10:10){
  x = i
  y = 0
  if (x > 0){
    y = x^2
  }
  else{
    y = x^3
  }
  print(paste(i, y, sep=" => "))
}

#exercice 13
#1
matrice = matrix(rnorm(n=100, mean = 0, sd = 2.32),nrow = 10, ncol=10)
#2
nbPos = length(which(matrice > 0))
idxNeg = which(matrice < 0)
nbNeg = length(idxNeg)
matrice[idxNeg] = 0
#3
apply(matrice, 1, sum)
apply(matrice, 2, sum)
#4
apply(matrice, 1, mean)
apply(matrice, 1, mean)
#5
rowSums(matrice); colSums(matrice)
rowMeans(matrice); colMeans(matrice)

#exercice 14
#1
data(iris)
str(iris)
moyenneET(2)
#2
moyenneET <- function(i) {
  moy = mean(iris[,i])
  et = sd(iris[,i])
  return( c(moy = moy, et = et) )
}
#3
?apply
moy = apply(iris[,c(1,2,3,4)], 2, mean)
et = apply(iris[,c(1,2,3,4)], 2, sd)
moyenneET(1)
moyenneET(2)
moyenneET(3)
moyenneET(4)

#exercice 15
somme = function(x, y){
  resultat = x+y
  return(resultat)
}

#exercice 16
# 1
nombre_mystere = 59
s = 0
nb_tirages = 0
while (s != nombre_mystere){
  s = sample(1:100, 1)
  nb_tirages = nb_tirages + 1
}
nb_tirages
# 2
trouver_nombre = function(nombre_mystere){
  if (nombre_mystere >=1 & nombre_mystere <=100){
    s = 0
    nb_tirages = 0
    while (s != nombre_mystere){
      s = sample(1:100, 1)
      nb_tirages = nb_tirages + 1
    }
    nb_tirages
  }
  else{
    warning("Votre nombre doit être compris entre 1 et 100")
  }
}
#3
nb_essais_rep = c()
for (i in seq(1000)){
  tmp = trouver_nombre(39)
  nb_essais_rep = c(nb_essais_rep, tmp)
}
mean(tmp)

#exercice 17
emails = c( "john.snow@etudiant.centralsupelec.fr",
            "patti.smith@etudiant.centralsupelec.fr",
            "rick.grimes@etudiant.centralsupelec.fr",
            "mere.theresa@etudiant.centralsupelec.fr")
#1
parseMail = function(email){
  nom_prenom = unlist(strsplit(email, "@"))[1]
  nom_prenom = unlist(strsplit(nom_prenom, "\\."))
  prenom = nom_prenom[1]
  nom = nom_prenom[2]
  data.frame(prenom = prenom, nom = nom, email = email)
}
#2
emails.df = c()
for (m in emails){
  df = parseMail(m)
  emails.df = rbind(emails.df,df)
}