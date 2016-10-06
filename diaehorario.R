library(gsheet)

jogos = gsheet2tbl("https://docs.google.com/spreadsheets/d/1LobEkmNGqp_WLF_vRWqqDWTp5Fz730HwehVwTzYrbPw/edit#gid=414497342")

index = which(jogos$Gol.C > jogos$Gol.F)
jogos$resultado[index] = 'Vitoria Casa'
index = which(jogos$Gol.C == jogos$Gol.F)
jogos$resultado[index] = 'Empate'
index = which(jogos$Gol.C < jogos$Gol.F)
jogos$resultado[index] = 'Vitoria Fora'

jogos$saldo = jogos$Gol.C - jogos$Gol.F
jogos$saldo = as.factor(jogos$saldo)

jogos$gols = jogos$Gol.C + jogos$Gol.F
jogos$gols = as.factor(jogos$gols)

b = table(jogos$Horario,jogos$resultado)
prop.table(b,1)*100

b = table(jogos$Dia,jogos$resultado)
prop.table(b,1)*100

round(prop.table(table(jogos$Dia,jogos$saldo),1)*100,2)
round(prop.table(table(jogos$Horario,jogos$saldo),1)*100,2)

round(prop.table(table(jogos$Dia,jogos$gols),1)*100,2)
round(prop.table(table(jogos$Horario,jogos$gols),1)*100,2)
