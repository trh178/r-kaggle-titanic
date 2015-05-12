table(train$Survived)

# did port of embark affect placement on ship?
table(train$Embarked)
table(train$Cabin)

# cabin improvement?
family.cabin <- combi[,c('Surname', 'Cabin')]
family.cabin[family.cabin$Surname == 'Carter',]
family.cabin[family.cabin$Cabin != '',]
