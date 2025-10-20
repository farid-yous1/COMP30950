import org.scalatest.funsuite.AnyFunSuite

class Assignment1Spec extends AnyFunSuite {

  test("averageDifficultyInTrimester handles empty trimester safely") {
    val c = new College
    assert(c.averageDifficultyInTrimester(Autumn) == 0.0)
  }

  test("mostChillStudent returns student with lowest total difficulty") {
    val c = new College
    c.addCourse(Course("A1", Autumn, 3))
    c.addCourse(Course("S1", Spring, 2))
    c.addStudent("Alice", List("A1"))
    c.addStudent("Bob", List("S1")) // difficulty 2 < 3
    assert(c.mostChillStudent == "Bob")
  }

  test("checkConsistency counts invalid + duplicate courses correctly") {
    val c = new College
    c.addCourse(Course("A1", Autumn, 3))
    c.addCourse(Course("S1", Spring, 2))
    c.addStudent("Ali", List("A1", "BOGUS", "A1")) // 1 invalid, duplicate valid
    c.addStudent("Zoe", List("S1", "S1", "BOGUS", "X")) // 2 invalid, duplicate valid

    val (invalids, dupStudents, balanced) = c.checkConsistency
    assert(invalids == 3)      // Ali:1 + Zoe:2
    assert(dupStudents == 2)   // both have duplicates
    assert(balanced)           // both have one valid unique (A1 & S1) → balanced
  }
}
