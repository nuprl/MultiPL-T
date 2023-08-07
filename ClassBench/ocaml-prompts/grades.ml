(* A Student type consists of a name and a id number representing a student. *)
(* name, id*)
type student = 
  | Student of string * int

(* A Grade type consists of a student id, a class, and a numeric grade. Represents 
the grade that a student received in a course. *)
(* nuid, class, numeric-grade *)
type grade = 
  | Grade of int * string * int

(* A StudentGrades type consists of a student name and list of numeric grades 
receieved by the student. Represents the grades that a student has received in 
all courses. *)
(* name, list of numeric grades the student received *)
type student_grades = 
| SG of string * int list

(* Given a list of students and a list of grades, pair the name of the student 
   with a list of their numeric grades using the student_grades type 
*)
let students_to_sg (los: student list) (log: grade list) : student_grades list = 
  let grades_for_student (s: student) = 
    (* grades-for-student : looks up all the grades for the supplied student *)
    let Student(name, sid) = s in
    let grades = List.map (fun (Grade(_, _, g)) -> g) 
                     (List.filter (fun (Grade(gid, _, _)) -> gid = sid) log)
    in SG(name, grades)
  in List.map grades_for_student los
;;

(* <tests> *)

let ex_grade1 = Grade(1, "Fundies", 95);;
let ex_grade2 = Grade(1, "Pyschoceramics", 65);;
let ex_grade3 = Grade(2, "PL", 85);;
let ex_grade4 = Grade(2, "Fundies", 75);;
let ex_grade5 = Grade(3, "Fundies", 68);;
let ex_grade6 = Grade(3, "Cybernetics", 82);;
let ex_grade7 = Grade(3, "Phonology", 89);;
let ex_grade8 = Grade(4, "Fundies", 89);;

let ex_student1 = Student("Alice", 1);; 
let ex_student2 = Student("Bob", 2);; 
let ex_student3 = Student("Carol", 3);; 

let assertions () = 
  assert (students_to_sg [] [] = []);
  assert (students_to_sg [] [ex_grade1; ex_grade2; ex_grade3; ex_grade4] = []);
  assert (students_to_sg [] [ex_grade1; ex_grade2; ex_grade3; ex_grade4] = []);
  assert (students_to_sg 
            [ex_student1] 
            [ex_grade1; ex_grade2; ex_grade3; ex_grade4] = 
          [SG("Alice", [95; 65])]);
  assert (students_to_sg 
            [ex_student1; ex_student2; ex_student3] 
            [ex_grade1; 
             ex_grade2; 
             ex_grade3; 
             ex_grade4; 
             ex_grade5; 
             ex_grade6; 
             ex_grade7;
             ex_grade8;] = 
          [SG("Alice", [95; 65]); 
           SG("Bob", [85; 75]);
           SG("Carol", [68; 82; 89])]);;

assertions()