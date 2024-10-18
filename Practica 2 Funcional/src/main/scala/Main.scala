import Clase2._

@main def hello: Unit = 
  println("Hello world!")
  println(msg)

  val resultado = subSets(List(1, 2))
  println("Resultado de subSets(List(1, 2, 3)): " + resultado)

def msg = "I was compiled by Scala 3. :)"
