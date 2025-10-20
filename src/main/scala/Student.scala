final case class Student(name: String, courseNames: List[String], college: College){
  override def toString: String =
    "name: " + name + ", courses: " + courseNames + ", college: " + college
}
