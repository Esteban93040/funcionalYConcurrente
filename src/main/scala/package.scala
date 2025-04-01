package object ManiobrasTrenes {
  type Vagon = Any
  type Tren = List[Vagon]
  type Estado = (Tren, Tren, Tren)

  trait Movimiento

  case class Uno(n: Int) extends Movimiento

  case class Dos(n: Int) extends Movimiento

  type Maniobra = List[Movimiento]

  def aplicarMovimiento(e: Estado, m: Movimiento): Estado = {
    m match {
      case Uno(n) =>
        if (n > 0) {
          val (principal, uno, dos) = e
          if (n>=principal.length) {
            (Nil,uno++principal,dos)
          } else {
            val mover = principal.takeRight(n)
            val restarElementos = principal.dropRight(n)

            (restarElementos, uno++mover, dos)
          }

        } else if (n < 0) {
          val (principal, uno, dos) = e
          if (-n>=uno.length) {
            (principal++uno,Nil,dos)
          } else {
            val mover = uno.take(-n)
            val restarElementos = uno.drop(-n)

            (principal++mover, restarElementos, dos)
          }

        } else {
          e
        }
      case Dos(n) =>
        if (n > 0) {
          val (principal, uno, dos) = e
          if (n>=principal.length) {
            (Nil,uno,dos++principal)
          } else {
            val mover = principal.takeRight(n)
            val restarElementos = principal.dropRight(n)

            (restarElementos, uno, dos++mover)
          }

        } else if (n < 0) {
          val (principal, uno, dos) = e
          if (-n>=dos.length) {
            (principal++dos,uno,Nil)
          } else {
            val mover = dos.take(-n)
            val restarElementos = dos.drop(-n)

            (principal++mover, uno, restarElementos)
          }

        } else {
          e
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