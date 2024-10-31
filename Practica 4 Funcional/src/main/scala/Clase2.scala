object Clase2 {

  // Usar la función fold, foldRight, reverse, filter, map

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
  def max(lista: List[Int]): Int = lista match
    case Nil => 0
    case head::Nil => head
    case head::tail => lista.fold(Int.MinValue)((acc, n) => if (n > acc) n else acc) //busque, y minValue es el minimo int que se puede declarar, o algo así
    // mi función es que, tengo el acumulador, y cada elmento. Voy guardando, y si n es mayor, lo guardo, si no lo dejo.
  

  /*
   * Busca el minimo
   */
  def min(lista: List[Int]): Int = lista match
    case Nil => 0
    case head::Nil => head
    case head::tail => lista.fold(Int.MaxValue)((acc, n) => if (n < acc) n else acc)

  // filtra la lista de valores mayores que el valor e pasado por parametro
  def maximos(lista: List[Int], e: Int): List[Int] = lista match
    case Nil => Nil
    case head::tail => lista.filter(_ > e)

  // filtra la lista de valores menores que el valor e pasado por parametro
  def minimos(lista: List[Int], e: Int): List[Int] = lista match
    case Nil => Nil
    case head::tail => lista.filter(_ < e)

  // Ordena los valores de una lista utilizando quicksort
  def QuickSort(xs: List[Int]): List[Int] = xs match
    case Nil => Nil
    case head::Nil => xs
    case head::tail => QuickSort(xs.filter(_ < head)) ++ xs.filter(_ == head) ++ QuickSort(xs.filter(_ > head))
  
  /**Obtiene un elemento en la posición n */
  def ObtenerElemento(lista: List[Int], posicion: Int): Int = ???

  /**
   * Busca la mediana
   * En el ámbito de la estadística, la mediana representa el
   * valor de la variable de posición central en un conjunto de datos ordenados.
  */
  def mediana(lista: List[Int]): Int = lista match
    case Nil => 0
    case head :: next => 
      if (lista.length % 2 == 0) {
        QuickSort(lista)(lista.length / 2 - 1)
      } else QuickSort(lista)(lista.length / 2)


  /**
   * Cuenta los elementos
   */
  def contar(lista: List[Int]): Int = lista match
    case Nil => 0
    case head::tail => 1 + contar(tail)
  

  // Acumula los elementos
  def acc(lista: List[Int]): Int = lista match
    case Nil => 0
    case head :: next => lista.fold(0)((acc, n) => acc + n)
  

  /**
   * Filtra los elementos de la lista xs segun la funcion p
   */
  def filtrar(xs: List[Int], p: Int => Boolean): List[Int] = xs match
    case Nil => Nil
    case head::tail => 
      xs.foldLeft(List.empty[Int]) { (acc, x) =>
        if (p(x)) acc :+ x 
        else acc
      }

  /**
   * Filtra los elementos pares usando filtrar utilizando la función Filtrar
   */
  def filtrarPares(xs: List[Int]): List[Int] = xs match
    case Nil => Nil
    case head::tail => filtrar(xs, _ % 2 == 0)
  

  /**
   * Filtra los elementos multiplos de 3 usando filtrar
   */
  def filtrarMultiplosDeTres(xs: List[Int]): List[Int] = xs match
    case Nil => Nil
    case head::tail => filtrar(xs, _ % 3 == 0)

  /**
   * Acumula los elementos aplicandoles fx
   */
  def acumular(lista: List[Int])(fx: (Int) => Int): Int = lista match
    case Nil => 0
    case head :: next => lista.fold(0)((acc, n) => acc + fx(n))


  /**
   * Acumula todos los elementos de una lista usando acumular
   */
  def acumularUnidad(lista: List[Int]): Int = lista match
    case Nil => 0
    case head :: next => acc(lista)

  /**
   * Acumula  el dobles de los elementos de una lista usando acumular
   */
  def acumularDoble(lista: List[Int]): Int = lista match
    case Nil => 0
    case head :: next => acumular(lista)((x: Int) => x*2)

  /**
   * Acumula el cuadrado de los elementos de una lista usando acumular
   */
  def acumularCuadrado(lista: List[Int]): Int = lista match
    case Nil => 0
    case head :: next => acumular(lista)((x: Int) => x*x)

  /**
   * Une 2 listas pasada por parametros
   */
  def unir(lista: List[Int], otraLista: List[Int]): List[Int] = lista match
    case Nil => otraLista
    case head :: tail => lista ++ otraLista

  /**
   * Transforma la lista a un lista de otro tipo
   */
  def transformar[T](lista: List[Int], fx: (Int) => (T)): List[T] = lista match
    case Nil => List()
    case head :: tail => lista.map(fx)
  

  /**
   * retorna true si un elemento existe en la lista
   */
  def existe(lista: List[Int], nro: Int): Boolean = lista match
    case Nil => false
    case head :: tail => head == nro || existe(tail, nro)

  

  /**
   * Une 2 listas pasada por parametros pero ignora los repetidos
   */
  def unirOfSet(lista: List[Int], otraLista: List[Int]): List[Int] = {
    if (lista.isEmpty) otraLista.foldLeft(List[Int]()) { (acc, elem) => 
      if (acc.contains(elem)) acc else acc :+ elem
    }
    else if (otraLista.isEmpty) lista.foldLeft(List[Int]()) { (acc, elem) => 
      if (acc.contains(elem)) acc else acc :+ elem
    }
    else (unir(lista, otraLista)).foldLeft(List[Int]()) { (acc, elem) => 
      if (acc.contains(elem)) acc else acc :+ elem
    }
  }

  /*******************************
  * Opcional
   * Dada una lista de enteros retorna una lista con todas las posibles subconjuntos
   * Por ejemplo : (1,2,3) -> ((),(1),(2),(3),(1,2),(1,3), (1,2,3), (2,3))
  ********************************/
  def subSets(lista: List[Int]): List[List[Int]] = lista match 
    case Nil => List(Nil) 
    case head :: tail =>
      subSets(tail) ++ subSets(tail).map(head :: _) 


  /** *****************************
   * Opcional
   * Dada una lista de enteros y un numoro entero n, retorne subconjuntos con n elementos
   * Ej> (1,2,3,4,5) y 2 -> ((1,2), (3,4), (5))
   * ****************************** */
  def cortar(lista: List[Int], n : Int): List[List[Int]] = {
    def auxCortar(pos: Int, actual: List[Int], nueva: List[List[Int]]): List[List[Int]] = {
      if (actual.length == n) auxCortar(pos + 1, List(), nueva :+ actual)
      else if (pos >= lista.length) nueva
      else auxCortar(pos + 1, actual :+ lista(pos), nueva)
    }
    auxCortar(0, List(), List())
  }


}
