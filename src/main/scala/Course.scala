// ----- Domain -----

sealed trait Semester
case object Autumn extends Semester
case object Spring extends Semester

final case class Course(name: String, semester: Semester, difficulty: Int) {
  override def toString: String = s"$name, $semester, $difficulty"
}



