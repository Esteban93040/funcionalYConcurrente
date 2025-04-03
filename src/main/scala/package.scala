import scala.language.postfixOps

package object ManiobrasTrenes {
  type Vagon = Any
  type Tren = List[Vagon]
  type Estado = (Tren, Tren, Tren)

  trait Movimiento

  case class Uno(n: Int) extends Movimiento

  case class Dos(n: Int) extends Movimiento

  type Maniobra = List[Movimiento]

  def aplicarMovimiento(e: Estado, m: Movimiento): Estado = {
    val (principal, uno, dos) = e
    m match {
      case Uno(n) => n match {
        case n if n > 0 =>
          if (n >= principal.length) (Nil, principal++uno, dos)
          else {
            val mover = principal.takeRight(n)
            val restarElementos = principal.dropRight(n)
            (restarElementos, mover++uno, dos)
          }
        case n if n < 0 =>
          if (-n >= uno.length) (principal ++ uno, Nil, dos)
          else {
            val mover = uno.take(-n)
            val restarElementos = uno.drop(-n)
            (principal ++ mover, restarElementos, dos)
          }
        case _ => e
      }
      case Dos(n) => n match {
        case n if n > 0 =>
          if (n >= principal.length) (Nil, uno,principal++dos)
          else {
            val mover = principal.takeRight(n)
            val restarElementos = principal.dropRight(n)
            (restarElementos, uno, dos++mover)
          }
        case n if n < 0 =>
          if (-n >= dos.length) {
            (principal ++ dos, uno, Nil)
          }
          else {
            val mover = dos.take(-n)
            val restarElementos = dos.drop(-n)
            (principal ++ mover, uno, restarElementos)
          }
        case _ => e
      }
    }
  }

  def aplicarMovimientos(e: Estado, movs: Maniobra): List[Estado] = {
    movs.foldLeft(List(e))((acc, m) => acc ++ List(aplicarMovimiento(acc.last, m)))
g  }

  def definirManiobra(t1: Tren, t2: Tren): Maniobra = {

    def realizarMovimiento(estado: Estado, movimientos: Maniobra, objetivo: Tren): Maniobra = estado match {
      case (Nil, Nil, Nil) => movimientos

      case (primerVagon :: restoVagones, Nil, Nil) => {
        if (primerVagon == objetivo.head)
          realizarMovimiento((restoVagones, Nil, Nil), movimientos, objetivo.tail)
        else {
          val vagonesParaMover = estado._1.dropWhile(_ != objetivo.head)
          realizarMovimiento(
            aplicarMovimiento(estado, Uno(vagonesParaMover.length)),
            movimientos :+ Uno(vagonesParaMover.length),
            objetivo
          )
        }
      }
      case (pila1Top :: pila1Rest, pila2Top :: pila2Rest, Nil) =>
        realizarMovimiento(
          aplicarMovimiento(estado, Dos(estado._1.length)),
          movimientos :+ Dos(estado._1.length),
          objetivo
        )
      case (Nil, pila2Top :: pila2Rest, pila3Top :: pila3Rest) =>
        realizarMovimiento(
          aplicarMovimiento(estado, Uno(-estado._2.length)),
          movimientos :+ Uno(-estado._2.length),
          objetivo
        )
      case (pila1Top :: pila1Rest, Nil, pila3Top :: pila3Rest) =>
        realizarMovimiento(
          aplicarMovimiento(estado, Dos(-estado._3.length)),
          movimientos :+ Dos(-estado._3.length),
          objetivo
        )
    }
    realizarMovimiento((t1, Nil, Nil), Nil, t2)
  }


}
