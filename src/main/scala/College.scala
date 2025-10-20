class College {

  // Internal state points to immutable Lists; we only ever rebind these fields.
  private var students: List[Student] = List.empty
  private var courses:  List[Course]  = List.empty

  // ---- Part 1 ----

  def addCourse(course: Course): Unit =
    // append is fine; List is immutable, so this produces a new List
    courses = courses :+ course

  def sortCoursesByName(): Unit =
    courses = courses.sortBy(_.name)

  def averageDifficultyInTrimester(trimester: Semester): Double =
    val inTrimester = courses.filter(_.semester == trimester)
    if inTrimester.isEmpty then 0.0
    else inTrimester.map(_.difficulty).sum.toDouble / inTrimester.length.toDouble

  // ---- Part 2 ----

  def addStudent(name: String, coursesChosen: List[String]): Unit =
    students = students :+ Student(name, coursesChosen, this)
    
  def mostChillStudent: String =
    val courseByName: Map[String, Course] = courses.map(c => c.name -> c).toMap

    def totalDifficulty(courseNames: List[String]): Int =
      courseNames.flatMap(courseByName.get).map(_.difficulty).sum

    // assumes at least one student exists per assignment tests
    students.minBy(s => totalDifficulty(s.courseNames)).name

  // ---- Part 3 ----

  // Keep the exact return type as a Tuple3 to match your original signature.
  def checkConsistency: Tuple3[Int, Int, Boolean] = {
    val courseByName: Map[String, Course] = courses.map(c => c.name -> c).toMap
    val validCourseNames: Set[String]     = courseByName.keySet

    // 1) Count of invalid course selections (sum over all students, all entries not in college)
    val invalidSelectionsCount: Int =
      students.map(_.courseNames.count(n => !validCourseNames.contains(n))).sum

    // 2) Number of students who have selected duplicate *valid* courses
    //    (we count the student once if they have any valid duplicate)
    val duplicateStudentsCount: Int =
      students.count { s =>
        val validOnly = s.courseNames.filter(validCourseNames.contains)
        validOnly.distinct.length != validOnly.length
      }

    // 3) Do ALL students have balanced valid, non-duplicated courses by trimester?
    //    (after filtering invalids and de-duplicating)
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
