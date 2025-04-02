import ManiobrasTrenes._

val e1= (List('a','b','c','d'), Nil , Nil)
val e2 = aplicarMovimiento(e1,Uno(1))
val e3 = aplicarMovimiento(e2,Dos(3))
val e4 = aplicarMovimiento(e3,Uno(-1))
val e5 = aplicarMovimiento(e4,Dos(-3))
val e6 = aplicarMovimiento(e5,Uno(2))
val e7 = aplicarMovimiento(e6,Dos(1))
val e8 = aplicarMovimiento(e7,Uno(-2))
val e9 = aplicarMovimiento(e8,Dos(-1))





val e=( List ('a','b' ) , List ('c') , List ('d'))
aplicarMovimientos(e,List(Uno(1),Dos(1),Uno(-2)))

definirManiobra (List('a','b','c','d') , List ('d','b','c','a'))

print("Prueba 1: ")
aplicarMovimientos((List('a','b','c','d'),Nil,Nil),definirManiobra(List('a','b','c','d') , List ('d','b','c','a')))