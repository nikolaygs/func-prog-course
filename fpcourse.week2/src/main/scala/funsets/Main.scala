package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(2), 1))
  
//  val s1 = singletonSet(1)
//  val s2 = singletonSet(2)
//  val s3 = singletonSet(3)
//  val s4 = singletonSet(100)
//  
//  val u1 = union(s1, s2)
//  val u2 = union(s3, s4)
//  
//  val set = union(u1, u2)
//
//  val predicate = (x: Int) => x % 2 == 0
//  val filteredSet = filter(set, predicate)
//  for (i <- 1 to 4) {
//    Console println s"contains: ${i} == ${contains(filteredSet, i)}"
//  }
//  
//  val predicate1 = (x: Int) => x > 0
//  Console println forall(set, predicate1)
//  
//  val predicate2 = (x: Int) => x < 1
//  Console println exists(set, predicate2)
//
//  val mapper1 = (x: Int) => x*10
//  Console println "#1321"
//  val mappedSet = map(set, mapper1)
//  Console println contains(mappedSet, 1000)
//  printSet(mappedSet)
//  Console println "#1321"
  
    val s1 = singletonSet(1)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s7 = singletonSet(7)
    val s1000 = singletonSet(1000)
    
    val mySet = union(union(union(s1, s3), union(s4, s5)), union(s7, s1000))
    val result = map(mySet, (x: Int) => x - 1)
    printSet(result)
}
