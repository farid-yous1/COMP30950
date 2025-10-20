class College {

  private var students: List[Student] = List.empty
  private var courses:  List[Course]  = List.empty


  def addCourse(course: Course): Unit =
    courses = courses :+ course

  def sortCoursesByName(): Unit =
    courses = courses.sortBy(_.name)

  def averageDifficultyInTrimester(trimester: Semester): Double =
    val inTrimester = courses.filter(_.semester == trimester)
    if inTrimester.isEmpty then 0.0   //edge cases
    else inTrimester.map(_.difficulty).sum.toDouble / inTrimester.length.toDouble //calculate average


  def addStudent(name: String, coursesChosen: List[String]): Unit =
    students = students :+ Student(name, coursesChosen, this)
    
  def mostChillStudent: String =
    val courseByName: Map[String, Course] = courses.map(c => c.name -> c).toMap

    def totalDifficulty(courseNames: List[String]): Int =
      courseNames.flatMap(courseByName.get).map(_.difficulty).sum

    students.minBy(s => totalDifficulty(s.courseNames)).name


  def checkConsistency: Tuple3[Int, Int, Boolean] = {
    val courseByName: Map[String, Course] = courses.map(c => c.name -> c).toMap  //map of names -> course object
    val validCourseNames: Set[String]     = courseByName.keySet

    val invalidSelectionsCount: Int =
      students.map(_.courseNames.count(n => !validCourseNames.contains(n))).sum

    val duplicateStudentsCount: Int =
      students.count { s =>
        val validOnly = s.courseNames.filter(validCourseNames.contains)
        validOnly.distinct.length != validOnly.length
      }

    val allBalanced: Boolean =
      students.forall { s =>
        val dedupValid = s.courseNames.filter(validCourseNames.contains).distinct
        val semesters: List[Semester] =
          dedupValid.flatMap(courseByName.get).map(_.semester)

        val counts: Map[Semester, Int] = semesters.groupBy(identity).view.mapValues(_.size).toMap
        val autumnCount = counts.getOrElse(Autumn, 0)
        val springCount = counts.getOrElse(Spring, 0)

        math.abs(autumnCount - springCount) <= 1
      }

    (invalidSelectionsCount, duplicateStudentsCount, allBalanced)
  }

  override def toString: String =
    courses.map(c => s"[${c.toString}]").mkString(", ")
}
