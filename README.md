# Clash-free-timetable-in-Prolog
This code is in SWI Prolog which is about the time table of university. It actually extract data from Exel files and store in tables. 
There are queries in this code that will be used to find clash in time table of classes and exams.

--TABLES--
1. A set of students: student(RollNo,Name).
2. A set of lecturers: instructor(emailID,Name)
3. A set of courses: course(CID,Title)
4. A set of section for each course: coursesection(CID,SecID)
5. A list of enrollment in CS211 sections: section(RollNo,SecID)
6. Which instructor teaches which courses: teaches(emailID,SecID)
7. The capacity of rooms: capacity(RID,Capacity)
8. The timing of the exam as per given datesheet: examTime(Date,StartTime,EndTime,CID,RID)

--PREDICATES--
1. Give a “Yes” or “No” answer on whether the given student name has two exams in one day.
2. Check if two students given in the query have the same section for a given course name.
3. Check if the two course given in the query have exams at the same time.
4. Check if a student has more than one roll number assigned.
5. Check if the two courses given in the query, have the exam in the same room at the same time.
6. Check if the given instructor is teaching any of the sections of the given course name
7. Check if the given instructor teaches two different courses
8. Check if the exams in the given room can be switched with another given room having same capacity.

