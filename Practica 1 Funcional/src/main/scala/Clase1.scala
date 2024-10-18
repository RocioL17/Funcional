object Clase1 {

  /**
   * Ejercicio -2
   *
   * Defina una función que calcule el cuadrado de un número
   */
  def cuadrado(n: Long): Long = n * n

  /**
   * Ejercicio -1
   *
   * Defina una función que calcule la enésima potencia positiva de un número
   */
  def potencia(n: Long, v: Int): Long = if (v == 0) 1
  else potencia(n, v-1) * n

  /**
   * Ejercicio 0
   *
   * Realice una función que calcule el enesimo numero de fibonacci
   */
  def factorial(n: Long): Long = {
    if (n == 0) 1
    else
      factorial(n - 1) * n
  }


  def fibonacci(n: Int): Long = {

    def fibonacciAux(faa: Long, fa: Long, n: Int): Long =
      if (n == 0) fa
      else fibonacciAux(fa, faa + fa, n -1)


    if (n == 0) 0
    else if (n == 1) 1
    else fibonacciAux(0, 1, n - 1)

  }


  /**
   * Ejercicio 2
   * Realice una funcion que dado el numero de fila y columna,
   * calcule el valor del numero que se encuentre el la piramide de Pascal
   */
  def pascal(c: Int, r: Int): Int = {
    def auxPascal(c: Int, r: Int, valor: Int): Int = {
      if (c == 0 || c == r) valor 
      else auxPascal(c - 1, r - 1, valor * (r - c + 1) / c) //el tercer argumento la fórmula para calcular el valorr
    }
    auxPascal(c, r, 1) 
  }

  /**
   * Ejercicio 3
   * Realice una función que permita saber si un texto tiene los parentesis balanceados, por ejemplo:
   * () => OK
   * ((()))() => OK
   * (()()) => OK
   * (()())) => no OK
   * (()(()) => no OK
   * )( => no OK
   * Lo pense así: Voy sumando uno cuando se abre un paréntesis, y voy restando en el caso que se cierren. Después, contemplo el caso si empieza con ) 
   * o termina con un (, dos casos en los que directamente esta mal.
   */
  def balance(chars: List[Char]): Boolean = {
    def auxBalance(c: Int, pos: Int): Boolean = {
      if(pos >= chars.length && c == 0) true
      else if(pos >= chars.length && c != 0) false
      else if (chars(pos) == '(')  auxBalance(c + 1, pos + 1)
      else if (chars(pos) == ')') auxBalance(c - 1, pos + 1)
      else auxBalance(c, pos + 1)
    }
    if(chars(0) == ')') false
    else if(chars(chars.length - 1) == '(') false
    else auxBalance(0, 0)
  }

  /**
   * Ejercicio 4
   * Realice una función que cuente cuantas combinaciones pueden existir con monedas para un valor determinado, por ejemplo:
   * monedas (1,2) y valor es 4 , podemos llegar con las siguientes convinaciones (1,1,1,1) (1,1,2) (2,2)
   * por lo que la función debería retornar 3.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def auxCountChange(remaining: Int, currentIndex: Int): Int = {
      if (remaining == 0) 1
      else if (remaining < 0 || currentIndex >= coins.length) 0
      else {
        auxCountChange(remaining - coins(currentIndex), currentIndex) +
        auxCountChange(remaining, currentIndex + 1)                    
      }
    }
    if(money < 0) 0
    else auxCountChange(money, 0)
  }

  /**
   * Ejercicio 5
   * Realice una función que indique si una lista de letras es palindromo
   * () -> true
   * ('a','l','a') -> true
   * ('a','l','l','a') -> true
   * ('h','a','l','l','a') -> false
   */
  def isPalindrome(word: List[Char]): Boolean = {
    val reverseWord: List[Char] = word.reverse //val, para asegurar inmutabilidadd
    if (word == reverseWord) true
    else false
  }
  
}
