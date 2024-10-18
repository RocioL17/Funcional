class Clase2Suite extends munit.FunSuite  {

  import Clase2._

  test("test de test") {
    assert(1 + 2 == 3)
  }

  test("max 1,2,3,4 es 4") {
    assert(max(List(1, 2, 3, 4)) == 4)
  }

  test("max 1,2,3,4,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3 es 4") {
    assert(max(List(1, 2, 3, 4, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)) == 4)
  }

  test("max 1,2,3,4,1,2,3,4 es 4") {
    assert(max(List(1, 2, 3, 4, 1, 2, 3, 4)) == 4)
  }

  test("min 1,2,3,4 es 1") {
    assert(min(List(1, 2, 3, 4)) == 1)
  }

  test("min 1,2,3,4,1,2,3,4 es 1") {
    assert(min(List(1, 2, 3, 4, 1, 2, 3, 4)) == 1)
  }

  test("mediana 1,2,3,4,5 es 3") {
    assert(mediana(List(1, 2, 3, 4, 5)) == 3)
  }

  test("mediana 1,2,3,4,5,6 es 3") {
    assert(mediana(List(1, 2, 3, 4, 5, 6)) == 3)
  }

  test("contar 1,2,3,4,5 es 5") {
    assert(contar(List(1, 2, 3, 4, 5)) == 5)
  }

  test("filtrarPares 1,2,3,4,5 es 2,4") {
    assert(filtrarPares(List(1, 2, 3, 4, 5)) == List(2, 4))
  }

  test("filtrarMultiplosDeTres 1,2,3,4,5 es 3") {
    assert(filtrarMultiplosDeTres(List(1, 2, 3, 4, 5)) == List(3))
  }

  test("filtrarMultiplosDeTres 1,2,3,4,5,6 es 3,6") {
    assert(filtrarMultiplosDeTres(List(1, 2, 3, 4, 5, 6)) == List(3, 6))
  }

  test("acumularUnidad 1,2,3,4,5 es 15") {
    assert(acumularUnidad(List(1, 2, 3, 4, 5)) == 15)
  }

  test("acumular Doble 1,2,3,4 es 20") {
    assert(acumularDoble(List(1, 2, 3, 4)) == 20)
  }

  test("acumular Cuadrado 1,2,3,4 es 30") {
    assert(acumularCuadrado(List(1, 2, 3, 4)) == 30)
  }

  test("QuickSort 4,2,3,1 es 1,2,3,4") {
    assert(QuickSort(List(4, 2, 3, 1)) == List(1, 2, 3, 4))
  }

  test("QuickSort 4,4,10,1 es 1,4,4,10") {
    assert(QuickSort(List(4, 4, 10, 1)) == List(1, 4, 4, 10))
  }

  test("unir 1,2,3 y 4,5 es 1,2,3,4,5") {
    assert(unir(List(1, 2, 3), List(4, 5)) == List(1, 2, 3, 4, 5))
  }

  test("unir 1,2,3 y {} es 1,2,3") {
    assert(unir(List(1, 2, 3), List()) == List(1, 2, 3))
  }

  test("unir {} y 4,5 es 4,5") {
    assert(unir(List(), List(4, 5)) == List(4, 5))
  }

  test("transformar 1,2,3,4 con toString es  1,2,3,4 ") {
    assert(transformar(List(1, 2, 3, 4), (a: Int) => a.toString) == List("1", "2", "3", "4"))
  }

  test("transformar 1,2,3,4 con toDouble es  1.0,2.0,3.0,4.0 ") {
    assert(transformar(List(1, 2, 3, 4), (a: Int) => a.toDouble) == List(1.0, 2.0, 3.0, 4.0))
  }

  test("unirOfSet 1,2,3 y 4,5 es 1,2,3,4,5") {
    assert(unirOfSet(List(1, 2, 3), List(4, 5)) == List(1, 2, 3, 4, 5))
  }

  test("unirOfSet 1,2,3 y {} es 1,2,3") {
    assert(unirOfSet(List(1, 2, 3), List()) == List(1, 2, 3))
  }

  test("unirOfSet {} y 4,5 es 4,5") {
    assert(unirOfSet(List(), List(4, 5)) == List(4, 5))
  }

  test("unirOfSet 1,1,2,3 y 4,5 es 1,2,3,4,5") {
    assert(unirOfSet(List(1, 1, 2, 3), List(4, 5)) == List(1, 2, 3, 4, 5))
  }

  test("unirOfSet 1,1,2,3 y {} es 1,2,3") {
    assert(unirOfSet(List(1, 1, 2, 3), List()) == List(1, 2, 3))
  }

  test("unirOfSet {} y 4,4,5 es 4,5") {
    assert(unirOfSet(List(), List(4, 4, 5)) == List(4, 5))
  }

  test("unirOfSet 1,1,2,3 y 4,4,5 es 1,2,3,4,5") {
    assert(unirOfSet(List(1, 1, 2, 3), List(4, 4, 5)) == List(1, 2, 3, 4, 5))
  }

  test("unirOfSet 1,2,3,4,5 y 4,5 es 1,2,3,4,5") {
    assert(unirOfSet(List(1, 2, 3, 4, 5), List(4, 5)) == List(1, 2, 3, 4, 5))
  }

  test("existe 1 en 1,2,3,4,5") {
    assert(existe(List(1, 2, 3, 4, 5), 1))
  }

  test("subSets (1,2) debe ser ((),(1,2),(1),(2))") {
    assert(subSets(List(1, 2)).toSet == Set(List(), List(1, 2), List(1), List(2))) //HICE QUE EL ORDEN NO IMPORTARA
  }

  test("cortar () de n retorna Nil") {
    assert(Set(cortar(Nil,2)) == Set(Nil))
  }

  //Pruebas que agregue yo, para las funciones que no tenian prueba.
  //acc
  test("acc con lista vacía retorna 0") {
    assert(acc(Nil) == 0)
  }

  test("acc con lista de un elemento retorna ese elemento") {
    assert(acc(List(5)) == 5)
  }

  test("acc con lista de varios elementos retorna la suma de los elementos") {
    assert(acc(List(1, 2, 3, 4)) == 10)
  }

  //buscar
  // test("buscar devuelve el primer elemento que cumple la condición") {
  //   assert(buscar(List(1, 2, 3, 4), _ > 2) == 3)
  // }

  // test("buscar devuelve el primer elemento que cumple la condición (menor que 0)") {
  //   assert(buscar(List(-1, 0, 1, 2), _ < 0) == -1)
  // }

  //obtener elemento
  test("ObtenerElemento retorna el elemento en la posición indicada") {
    assert(ObtenerElemento(List(1, 2, 3, 4), 2) == 3)
  }

  //filtrar
  test("filtrar devuelve una lista con elementos que cumplen la condición") {
    assert(filtrar(List(1, 2, 3, 4), _ > 2) == List(3, 4))
  }

  test("filtrar devuelve una lista vacía si ningún elemento cumple la condición") {
    assert(filtrar(List(1, 2, 3), _ > 5) == Nil)
  }

  test("filtrar en lista vacía retorna lista vacía") {
    assert(filtrar(Nil, _ > 2) == Nil)
  }

  //acumular
  test("acumular con lista vacía retorna 0") {
    assert(acumular(Nil)(_ + 1) == 0)
  }

  test("acumular con lista de un elemento aplica fx al elemento") {
    assert(acumular(List(5))(_ * 2) == 10)
  }

  test("acumular aplica fx a todos los elementos y suma el resultado") {
    assert(acumular(List(1, 2, 3))(_ * 2) == 12)  // (1*2 + 2*2 + 3*2) = 12
  }

}
