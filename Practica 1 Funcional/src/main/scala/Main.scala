import Clase1.countChange
@main def hello: Unit = 
  println("Hello world!")
  println(msg)
  val monedas = List(1, 2, 3, 4) // Lista de monedas
  val combinaciones = countChange(4, monedas)
  println(combinaciones)

def msg = "I was compiled by Scala 3. :)"


