object Clase2 {
  //Generico
  //Recursión de Cola

  /**
   * Buscar
   * Dada una lista y una función de comparación, devuelve el valor que cumple la condición.
   */
  def buscar(lista: List[Int], com: (Int, Int) => Boolean): Int = lista match
    case Nil => 0
    case head::Nil => head
    case head::tail => if (com(head, buscar(tail, com))) head else buscar(tail, com)
  

  /*
   * Busca el Maximo
   */
  def max(lista: List[Int]): Int = buscar(lista, _>_)

  /*
   * Busca el minimo
   */
  def min(lista: List[Int]): Int = buscar(lista, _<_)

  // filtra la lista de valores mayores que el valor e pasado por parametro
  def maximos(lista: List[Int], e: Int): List[Int] = {
    if (lista.isEmpty) Nil // aca Nil es como hacemos una lista vacia en Scala :)
    else filtrar(lista, _ > e)
  }

  // filtra la lista de valores menores que el valor e pasado por parametro
  def minimos(lista: List[Int], e: Int): List[Int] = {
    if (lista.isEmpty) Nil // aca Nil es como hacemos una lista vacia en Scala :)
    else filtrar(lista, _ < e)
  }

  // Ordena los valores de una lista utilizando quicksort
  def QuickSort(xs: List[Int]): List[Int] = {
    def auxSort(min: List[Int], max: List[Int], medio: Int, pos: Int): List[Int] = {
      if (pos >= xs.length) QuickSort(min) ++ (medio :: QuickSort(max))
      else if (xs(pos) >= medio) auxSort(min, max :+ xs(pos), medio, pos + 1)
      else auxSort(min :+ xs(pos), max, medio, pos + 1)
    }
    if (xs.isEmpty) Nil
    else if (xs.length == 1) xs
    else auxSort(Nil, Nil, xs(0), 1)
  }

  /**Obtiene un elemento en la posición n */
  def ObtenerElemento(lista: List[Int], posicion: Int): Int = {
    if (posicion > lista.length) 0
    else lista(posicion)
  }

  /**
   * Busca la mediana
   * En el ámbito de la estadística, la mediana representa el
   * valor de la variable de posición central en un conjunto de datos ordenados.
  */
  def mediana(lista: List[Int]): Int = {
    def auxMediana(l: List[Int]): Int = {
      if(l.length % 2 == 0) (((l.length/2) + (l.length/2)+1) / 2).toInt
      else l(l.length/2)
    }
    if (lista.isEmpty) 0
    else auxMediana(QuickSort(lista)) //aca use la función sorted para ordenar, UPDATE: USE MI FUNCIÓN.
  }

  /**
   * Cuenta los elementos
   * Si bien puedo usar length, lo hago recursivamente para practicar
   */
  def contar(lista: List[Int]): Int = {
    def auxContar(a: Int, valor: Int): Int = {
      if (a >= lista.length) valor
      else auxContar(a + 1, valor + 1)
    }
    if(lista.isEmpty) 0
    else auxContar(0, 0)
  }

  // Acumula los elementos
  def acc(lista: List[Int]): Int = {
    def auxAcc(a: Int, valor: Int): Int = {
      if (a >= lista.length) valor
      else auxAcc(a + 1, valor + lista(a))
    }
    if(lista.isEmpty) 0
    else auxAcc(0, 0)
  }

  /**
   * Filtra los elementos de la lista xs segun la funcion p
   */
  def filtrar(xs: List[Int], p: Int => Boolean): List[Int] = {
    def auxFiltrar(pos: Int, nueva: List[Int]): List[Int] = {
      if (pos >= xs.length) nueva
      else if (p(xs(pos))) auxFiltrar(pos + 1, nueva :+ xs(pos))
      else auxFiltrar(pos + 1, nueva)
    }
    if (xs.isEmpty) Nil
    else auxFiltrar(0, Nil)
  }

  /**
   * Filtra los elementos pares usando filtrar utilizando la función Filtrar
   */
  def filtrarPares(xs: List[Int]): List[Int] = {
    filtrar(xs, _ % 2 == 0)
  }

  /**
   * Filtra los elementos multiplos de 3 usando filtrar
   */
  def filtrarMultiplosDeTres(xs: List[Int]): List[Int] = {
    filtrar(xs, _ % 3 == 0)
  }

  /**
   * Acumula los elementos aplicandoles fx
   */
  def acumular(lista: List[Int])(fx: (Int) => Int): Int = {
    def auxAcumular(pos: Int, valor: Int): Int = {
      if (pos >= lista.length) valor
      else auxAcumular(pos + 1, (valor + fx(lista(pos))))
    }
    if (lista.isEmpty) 0
    else auxAcumular(0, 0)
  }


  /**
   * Acumula todos los elementos de una lista usando acumular
   */
  def acumularUnidad(lista: List[Int]): Int = {
    acumular(lista)((x: Int) => x)
  }

  /**
   * Acumula  el dobles de los elementos de una lista usando acumular
   */
  def acumularDoble(lista: List[Int]): Int = {
    acumular(lista)((x: Int) => x*2)
  }

  /**
   * Acumula el cuadrado de los elementos de una lista usando acumular
   */
  def acumularCuadrado(lista: List[Int]): Int = {
    acumular(lista)((x: Int) => x * x)
  }

  /**
   * Une 2 listas pasada por parametros
   */
  def unir(lista: List[Int], otraLista: List[Int]): List[Int] = {
    if(lista.isEmpty) otraLista
    else if (otraLista.length == 0) lista
    else lista ++ otraLista
  }

  /**
   * Transforma la lista a un lista de otro tipo
   */
  def transformar[T](lista: List[Int], fx: (Int) => (T)): List[T] = {
    def auxTransformar(pos: Int, nueva: List[T]): List[T] = {
      if (pos >= lista.length) nueva
      else auxTransformar(pos + 1, (nueva :+ fx(lista(pos))))
    }
    if (lista.isEmpty) Nil
    else auxTransformar(0, Nil)
  }

  /**
   * retorna true si un elemento existe en la lista
   */
  def existe(lista: List[Int], nro: Int): Boolean = {
    def auxExiste(pos: Int): Boolean = {
      if (pos >= lista.length) false
      else if (lista(pos) == nro) true
      else auxExiste(pos + 1)
    }
    if (lista.isEmpty) false
    else auxExiste(0)
  }

  /**
   * Une 2 listas pasada por parametros pero ignora los repetidos
   */
  def unirOfSet(lista: List[Int], otraLista: List[Int]): List[Int] = {
    def auxIgnorarRepetidos(resto: List[Int], nueva: List[Int]): List[Int] = {
      if (resto.isEmpty) nueva
      else if (estaEnLista(nueva, resto.head)) auxIgnorarRepetidos(resto.tail, nueva) //Notas: En Scala, head es el primer elemento, y tail es el resto
      else auxIgnorarRepetidos(resto.tail, nueva :+ resto.head)
    }

    def estaEnLista(lista: List[Int], elemento: Int): Boolean = {
      if (lista.isEmpty) false
      else if (lista.head == elemento) true
      else estaEnLista(lista.tail, elemento)
    }
    auxIgnorarRepetidos(lista ++ otraLista, Nil)
  }

  /*******************************
  * Opcional
   * Dada una lista de enteros retorna una lista con todas las posibles subconjuntos
   * Por ejemplo : (1,2,3) -> ((),(1),(2),(3),(1,2),(1,3), (1,2,3), (2,3))
  ********************************/
  def subSets(lista: List[Int]): List[List[Int]] = {
    def auxSubSets(xs: List[Int], nueva: List[List[Int]]): List[List[Int]] = {
      if (xs.isEmpty) nueva
      else auxSubSets(xs.tail, (nueva ++ nueva.map(e => e :+ xs.head)))
    }
    if (lista.isEmpty) Nil
    else auxSubSets(lista, List(Nil))
  }

  def cortar(lista: List[Int], n: Int): List[List[Int]] = {
    def auxCortar(pos: Int, actual: List[Int], nueva: List[List[Int]]): List[List[Int]] = {
      if (actual.length == n) auxCortar(pos + 1, List(), nueva :+ actual)
      else if (pos >= lista.length) nueva
      else auxCortar(pos + 1, actual :+ lista(pos), nueva)
    }
    auxCortar(0, List(), List())
  }

}
