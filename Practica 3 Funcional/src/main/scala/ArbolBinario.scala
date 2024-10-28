  /**
   * Arbol Binario
   *
   */
  object ArbolBinario {

    /**
     * Un árbol binario de búsqueda también llamados BST (acrónimo del inglés Binary Search Tree)
     * es un tipo particular de árbol binario que presenta una estructura de datos en forma de árbol
     * usada en informática.
     *
     * Árbol binario
     *
     * la mayoría de los árboles binarios son de búsqueda
     *
     * Un árbol binario no vacío, de raíz R, es un árbol binario de búsqueda si:
     *
     * En caso de tener subárbol izquierdo, la raíz R debe ser mayor que el valor máximo almacenado en el
     * subárbol izquierdo, y que el subárbol izquierdo sea un árbol binario de búsqueda.
     * En caso de tener subárbol derecho, la raíz R debe ser menor que el valor mínimo almacenado en el
     * subárbol derecho, y que el subárbol derecho sea un árbol binario de búsqueda.
     *
     * En Resumen: Un elemento apunta a otros dos elementos, uno esta a la izquierda y otro a la derecha.
     * El elemento a la izquierda es más pequeño y el segundo es más grande.
     * Cada uno de estos dos elementos puede apuntar a otros dos elementos (o a uno o a ninguno).
     * En efecto, cada elemento tienen sus propios sub-árboles. Lo bueno de los árboles binarios
     * de búsqueda es que sabemos que todos los elementos que están en el sub-árbol de la izquierda
     * de, 5, por ejemplo, son menores que 5. Lo elementos que están en el sub-árbol de la derecha
     * son mayores. Así que si estamos buscando el elemento 8 en nuestro árbol, empezamos comparándolo
     * con 5, como vemos que es menor que 5, nos vamos al sub-árbol de la derecha. Ahora estaríamos en 7,
     * como es menor que 8 continuaríamos hacia la derecha. De esta forma encontraríamos el elemento en
     * tres pasos. Si estuviéramos usando una lista (o un árbol no balanceado), nos hubiera costado unos 7
     * pasos encontrar el 8. 
     */

    sealed abstract class Tree
    case class EmptyTree() extends Tree
    case class Node(nro: Int, left: Tree, right: Tree) extends Tree

    /*
     * Inserta un elemento
     */
    def insert(nro: Int, tree: Tree): Tree = tree match {
      case EmptyTree() =>
        Node(nro, EmptyTree(), EmptyTree()) // Crea un nuevo nodo con el valor en un árbol vacío

      case Node(value, left, right) =>
        if (nro < value) Node(value, insert(nro, left), right) // Inserta en el subárbol izquierdo
        else if (nro > value) Node(value, left, insert(nro, right)) // Inserta en el subárbol derecho
        else tree // Si el valor ya existe, devuelve el árbol sin cambios
    }

    /**
     * Devuelve verdadero si el elemento esta en el arbol.
     */
    def elem(nro: Int, tree: Tree): Boolean = tree match {
      case EmptyTree() => false
      // case Node(nro, left, right) => true // si tenemos un subárbol con el número que le pasamos
      case Node(numero, left, right) => 
        if (numero == nro) true
        else elem(nro, left) || elem(nro, right)
    }

    /**
     * Devuelve la cantidad de nodos del arbol
     */
    def countNodes(tree: Tree): Int = tree match {
      case EmptyTree() => 0 // Si el árbol está vacío, devuelve
      case Node(_, left, right) => 1 + countNodes(left) + countNodes(right)
    }

    /**
     * Devuelve un arbol donde se aplico esta funcion
     */
    def apply(tree: Tree, fx: (Int) => Int): Tree = tree match {
      case EmptyTree() => tree
      case Node(nro, left, right) => 
        Node(fx(nro), apply(left, fx), apply(right, fx)) //aplico la búsqueda en ambos sub árboles
    }

    /**
     * Devuelve un arbol con el doble de los elementos
     */
    def double(tree: Tree): Tree = tree match {
      case EmptyTree() => tree
      case Node(nro, left, right) => 
        // Node(nro * 2, double(left), double(right))
        apply(tree, _*2)
    }

    /**
     * Devuelve una lista ordenada
     */
    def toList(tree: Tree): List[Int] = tree match {
      case EmptyTree() => Nil
      case Node(nro, left, right) => 
        toList(left) ++ List(nro) ++ toList(right) //hago una unión entre la lista hacia la izquierda y hacia la derecha
    }

    /**
     * Retorna el nivel del arbol
     */
    def nivel(ts: Tree): Int = ts match {
      case EmptyTree() => 0
      case Node(nro, left, right) => 
        1 + Math.max(nivel(left), nivel(right))
    }


    /**
     * Balancea el árbol si no está balanceado.
     * Creo un árbol nuevo, ordenándolo siempre.
     */
    def balancear(tree: Tree): Tree = {
      // Convertir el árbol a una lista ordenada
      val lista = toList(tree)
      
      // Si el árbol ya está balanceado, lo devolvemos sin cambios
      if (isBal(tree)) tree
      else listToBalancedTree(lista) // Construimos un nuevo árbol balanceado desde la lista
    }

    /**
     * Retorna true si el árbol está balanceado:
     * la diferencia de niveles entre los subárboles no supera 1.
     */
    def isBal(tree: Tree): Boolean = tree match {
      case EmptyTree() => true
      case Node(_, left, right) =>
        // Math.abs(nivel(left) - nivel(right)) <= 1 && isBal(left) && isBal(right)
        if (nivel(left) == nivel(right)) true
        else false
    }

    /**
     * FUNCION EXTRA PARA QUE PUEDA CONVERTIR DE UNA LISTA A UN ÁRBOL
     */
    def listToBalancedTree(listaOrdenada: List[Int]): Tree = {
      if (listaOrdenada.isEmpty) EmptyTree()
      else {
        val middle = listaOrdenada.length / 2
        Node(listaOrdenada(middle), listToBalancedTree(listaOrdenada.take(middle)), listToBalancedTree(listaOrdenada.drop(middle + 1))
        )
      }
    }

    /**
     * Devuelve una lista ordenada en formato string
     */
    def toString(tree: Tree): String = tree match {
      case EmptyTree() => ""
      case Node(nro, left, right) =>
        toString(left) + "," + nro + "," + toString(right)
    }


  }


