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
          if (n >= principal.length) (Nil, uno ++ principal, dos)
          else {
            val (restarElementos, mover) = principal.splitAt(principal.length - n)
            (restarElementos, uno ++ mover, dos)
          }
        case n if n < 0 =>
          if (-n >= uno.length) (principal ++ uno, Nil, dos)
          else {
            val (mover, restarElementos) = uno.splitAt(-n)
            (principal ++ mover, restarElementos, dos)
          }
        case _ => e
      }
      case Dos(n) => n match {
        case n if n > 0 =>
          if (n >= principal.length) (Nil, uno, dos ++ principal)
          else {
            val (restarElementos, mover) = principal.splitAt(principal.length - n)
            (restarElementos, uno, dos ++ mover)
          }
        case n if n < 0 =>
          if (-n >= dos.length) (principal ++ dos, uno, Nil)
          else {
            val (mover, restarElementos) = dos.splitAt(-n)
            (principal ++ mover, uno, restarElementos)
          }
        case _ => e
      }
    }
  }

  def aplicarMovimientos(e: Estado, movs: Maniobra ) : List [Estado] = {
    movs.foldLeft(List(e))((acc, m) => acc :+ aplicarMovimiento(acc.last, m))
  }

  def definirManiobra(t1: Tren, t2: Tren): Maniobra = {
    def moverVagones(tren: Tren, destino: Tren, maniobra: Maniobra): Maniobra = {
      if (tren.isEmpty) maniobra
      else {
        val vagon = tren.head
        val resto = tren.tail
        val (movs, nuevoDestino) = destino.span(_ != vagon)
        val movimientos = if (movs.isEmpty) {
          List(Uno(1), Dos(1), Uno(-1), Dos(-1))
        } else {
          List(Uno(movs.length + 1), Uno(-movs.length))
        }
        moverVagones(resto, nuevoDestino, maniobra ++ movimientos)
      }
    }

    val maniobraInicial = List(Uno(t1.length))
    val maniobraFinal = moverVagones(t1.reverse, t2.reverse, List())
    maniobraInicial ++ maniobraFinal
  }
}