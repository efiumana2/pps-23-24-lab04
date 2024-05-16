package tasks.adts

import u03.Sequences.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */



object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object SchoolModuleImpl extends SchoolModule:
    case class CourseImpl(name: String)
    case class TeacherImpl(name: String,courses: Sequence[Course])
    case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])

    extension [A](seq: Sequence[A])
      def find(query: A => Boolean): Optional[A] = seq match
        case Sequence.Cons(h,t) if query(h) => Optional.Just(h)
        case Sequence.Cons(_,t) => t.find(query)
        case Sequence.Nil() => Optional.Empty()
    type Course = CourseImpl
    type Teacher = TeacherImpl
    type School = SchoolImpl

    def school: School = SchoolImpl(Sequence.Nil(),Sequence.Nil())
    def teacher(name: String): Teacher = TeacherImpl(name, Sequence.Nil())
    def course(name: String):Course = CourseImpl(name)
    def addCourseToTeacher(t: Teacher)(c: Course): Teacher =
      TeacherImpl(t.name,Sequence.Cons(c,t.courses))
    extension (school: School)
      def addTeacher(name: String): School =
         SchoolImpl(Sequence.Cons(teacher(name), school.teachers),school.courses)
      def addCourse(name: String): School =
        SchoolImpl(school.teachers, Sequence.Cons(CourseImpl(name),school.courses))
      def teacherByName(name: String): Optional[Teacher] = school.teachers.find(_.name == name)
      def courseByName(name: String): Optional[Course] = school.courses.find(_.name == name)
      def nameOfTeacher(teacher: Teacher): String = teacher.name //???
      def nameOfCourse(course: Course): String = course.name //???//teacher. //???
      def setTeacherToCourse(teacher: Teacher, course: Course): School = //??? // Deve essere nella scuola e lo aggiungo ai corsi del teacher
        def f1(s: Sequence[Teacher],v: Teacher,c: Course):Sequence[Teacher] = s match
          case Sequence.Cons(h,t) if (h == v) => Sequence.Cons(addCourseToTeacher(h)(c),f1(t,v,c))
          case Sequence.Cons(h, t) => Sequence.Cons(h,f1(t,v,c))
          case Sequence.Nil() => Sequence.Nil()
        SchoolImpl(f1(school.teachers,teacher,course),school.courses)
      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = teacher.courses //???

@main def Scuola =
  import SchoolModel.*

  val schoolADT = SchoolModuleImpl
  import schoolADT.*

  var sc = school.addCourse("mat")
  println(sc)
  var sc2 = sc.addTeacher("tizio")
  println(sc2)
  val sc3 = sc2.setTeacherToCourse(teacher("tizio"),course("mat"))
  println(sc3)
