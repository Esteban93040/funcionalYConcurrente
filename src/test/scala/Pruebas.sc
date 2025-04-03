import ManiobrasTrenes._

//Casos de prueba para la función aplicarMovimiento

print("Prueba 1 de aplicarmovimiento: ")
val e1prueba1= (List('b','d','e'), Nil , Nil)
val e2prueba1 = aplicarMovimiento(e1prueba1,Uno(1))
val e3prueba1 = aplicarMovimiento(e2prueba1,Dos(2))
val e4prueba1 = aplicarMovimiento(e3prueba1,Uno(-1))
val e5prueba1 = aplicarMovimiento(e4prueba1,Dos(-2))
val e6prueba1 = aplicarMovimiento(e5prueba1,Uno(1))
val e7prueba1 = aplicarMovimiento(e6prueba1,Dos(1))
val e8prueba1 = aplicarMovimiento(e7prueba1,Uno(-1))
val e9prueba1 = aplicarMovimiento(e8prueba1,Dos(-1))

val e1prueba2 = (List(1, 2, 3, 4), List(5, 6), List(7, 8, 9))
val e2prueba2 = aplicarMovimiento(e1prueba2, Uno(2))   
val e3prueba2 = aplicarMovimiento(e2prueba2, Dos(2))   
val e4prueba2 = aplicarMovimiento(e3prueba2, Uno(-1))  
val e5prueba2 = aplicarMovimiento(e4prueba2, Dos(-1))  


val e1prueba3 = (List(2, 3), List(4, 5, 6, 7), List(8, 9))
val e2prueba3 = aplicarMovimiento(e1prueba3, Uno(-3))
val e3prueba3 = aplicarMovimiento(e2prueba3, Dos(2))


val e1prueba4 = (List(1, 2, 3, 4, 5), List(), List(6, 7))
val e2prueba4 = aplicarMovimiento(e1prueba4, Uno(3))  
val e3prueba4 = aplicarMovimiento(e2prueba4, Dos(2))  
val e4prueba4 = aplicarMovimiento(e3prueba4, Uno(-1)) 
val e5prueba4 = aplicarMovimiento(e4prueba4, Dos(-2)) 


val e1prueba5 = (List(11, 12, 13), List(9), List(3, 4))
val e2prueba5 = aplicarMovimiento(e1prueba5, Uno(10))
val e3prueba5 = aplicarMovimiento(e2prueba5, Uno(-10))

val vdado1=(List('b','d','e'))
val vdado2=(List('a','e','i','o'))
val vdado3=(List('a','b'))
val vdado4=(List('1','2','3','4','5','6'))
val vdado5=(List())

val esperado1=List ('e','d','b')
val esperado2=List ('o','i','e','a')
val esperado3=List ('b','a')
val esperado4=List ('6','4','5','3','1','2')
val esperado5=List ('a')

//Casos de prueba para la función definirManiobra
val dm1 = definirManiobra(vdado1,esperado1)
val dm2 = definirManiobra(vdado2,esperado2)
val dm3 = definirManiobra(vdado3,esperado3)
val dm4 = definirManiobra(vdado4,esperado4)
val dm5 = definirManiobra(vdado5,esperado5)

print("Prueba de que definir maniobra es correcto: ")
aplicarMovimientos((vdado1,Nil,Nil),dm1)
aplicarMovimientos((vdado2,Nil,Nil),dm2)
aplicarMovimientos((vdado3,Nil,Nil),dm3)
aplicarMovimientos((vdado4,Nil,Nil),dm4)
aplicarMovimientos((vdado5,Nil,Nil),dm5)
