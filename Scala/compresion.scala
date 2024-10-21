//APUNTES DE SCALA

val lista = 1::2::3::4::Nil

def foldAux[T](l: List[T], fx : (T, T) => T, init: U) : T = l match {
    case Nil => T
    case head::tail => foldAux(tail, fx, fx(init, head))
}

//lambda recibe dos parámetros y los suma
foldAux(lista, _+_, 0)
lista.fold(0)(_+_)

//Primero lo hicimos con Int unicamente, y después la amoldamos para que acepte todos los tipos.
//Había un problema que era el Nil, seguía devolviendo un 0, un int.

