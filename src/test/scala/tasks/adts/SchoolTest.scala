package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import tasks.adts.SchoolModel.BasicSchoolModule.{course, emptySchool, teacher}
import u03.extensionmethods.Sequences.Sequence.{Cons, cons, nil}

class SchoolTest:

  val schoolModel: SchoolModule = BasicSchoolModule

  @Test def testEmptySchool() =
    val school = emptySchool
    assertEquals(nil(), school.courses)
    assertEquals(nil(), school.teachers)
    assertEquals(nil(), school.teacherToCourses)

  @Test def testSetTeacher() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val school2 = school.setTeacherToCourse(john, math)
    assertEquals(cons("John", nil()), school2.teachers.map(teacher => teacher.teacherName))
    assertEquals(cons("Math", nil()), school2.courses.map(course => course.courseName))

  @Test def testSetTeacher2() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val italian = course("Italian")
    val school2 = school.setTeacherToCourse(john, math)
    assertEquals(cons("John", nil()), school2.teachers.map(teacher => teacher.teacherName))
    assertEquals(cons("Math", nil()), school2.courses.map(course => course.courseName))
    val school3 = school2.setTeacherToCourse(john, italian)
    assertEquals(cons("John", nil()), school2.teachers.map(teacher => teacher.teacherName))
    assertEquals(cons("Math",cons("Italian", nil())), school3.courses.map(course => course.courseName))

  @Test def testMultipleTeachersAndCourses(): Unit = {
    val school = emptySchool
    val john = teacher("John")
    val alice = teacher("Alice")
    val math = course("Math")
    val italian = course("Italian")
    val history = course("History")

    val school1 = school.setTeacherToCourse(john, math)
    val school2 = school1.setTeacherToCourse(john, italian)
    val school3 = school2.setTeacherToCourse(alice, history)

    assertEquals(cons("John", cons("Alice", nil())), school3.teachers.map(t => t.teacherName))
    assertEquals(cons("Math", cons("Italian", cons("History", nil()))), school3.courses.map(c => c.courseName))

    val johnCourses =
      school3.teacherToCourses
        .filter { case (t, _) => t == john }
        .flatMap { case (_, courses) => courses }
        .map(c => c.courseName)
    assertEquals(cons("Math", cons("Italian", nil())), johnCourses)

    val aliceCourses =
      school3.teacherToCourses
        .filter { case (t, _) => t == alice }
        .flatMap { case (_, courses) => courses }
        .map(c => c.courseName)
    assertEquals(cons("History", nil()), aliceCourses)
  }
