package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import tasks.adts.SchoolModel.BasicSchoolModule.*
import u03.extensionmethods.Sequences.Sequence.*

class SchoolTest:

  val schoolModel: SchoolModule = BasicSchoolModule

  @Test def testEmptySchool() =
    val school = emptySchool
    assertEquals(nil(), school.sequenceCourses)
    assertEquals(nil(), school.sequenceTeachers)
    assertEquals(nil(), school.teacherToCourses)

  @Test def testHasTeacher() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val school2 = school.setTeacherToCourse(john, math)
    assertTrue(school2.hasTeacher("John"))
    assertFalse(school.hasTeacher("John"))

  @Test def testHasCourse() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val school2 = school.setTeacherToCourse(john, math)
    assertTrue(school2.hasCourse("Math"))
    assertFalse(school.hasCourse("Math"))

  @Test def testSetTeacher() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val school2 = school.setTeacherToCourse(john, math)
    assertEquals(cons("John", nil()), school2.teachers)
    assertEquals(cons("Math", nil()), school2.courses)


  @Test def testSetTeacher2() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val italian = course("Italian")
    val school2 = school.setTeacherToCourse(john, math)
    assertEquals(cons("John", nil()), school2.teachers)
    assertEquals(cons("Math", nil()), school2.courses)
    val school3 = school2.setTeacherToCourse(john, italian)
    assertEquals(cons("John", nil()), school2.teachers)
    assertEquals(cons("Math",cons("Italian", nil())), school3.courses)

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

    assertEquals(cons("John", cons("Alice", nil())), school3.teachers)
    assertEquals(cons("Math", cons("Italian", cons("History", nil()))), school3.courses)
    assertEquals(cons("Math", cons("Italian", nil())), school3.coursesOfATeacher(john).map(c => c.courseName))
    assertEquals(cons("History", nil()), school3.coursesOfATeacher(alice).map(c => c.courseName))
  }
