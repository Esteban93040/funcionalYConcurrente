import ManiobrasTrenes._

val e1= (List('a','b','c','d'), Nil, Nil)
val e2 = aplicarMovimiento(e1,Uno(2))
val e3 = aplicarMovimiento(e2,Dos(3))
val e4 = aplicarMovimiento(e3,Dos(-1))
val e5 = aplicarMovimiento(e4,Uno(-2))


val e= (List('a','b'),)