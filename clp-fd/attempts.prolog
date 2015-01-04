
:- use_module(library(clpfd)).

% problem definition
room(toja, 35).
room(auditorium, 200).

/*
Predicate `teaching(T, H)` is true iff teaching T is taught H hours a week.
*/
teaching(computer_science, 9).
teaching(algebra, 6).
teaching(medicine_one, 9).
teaching(chemistry, 4).

teaches(crescenzi, algebra).
teaches(crescenzi, computer_science).
teaches(crescenzi, algorithms).

teaches(casolo, algebra).
teaches(casolo, algebra_two).

teaches(rossi, medicine_one).

course(engineering, computer_science).
course(engineering, algebra).
course(engineering, chemistry).

course(medicine, algebra).
course(medicine, medicine_one).
course(medicine, chemistry).

% HINT: consider adding explicit knowledge for sharing a teaching among two courses
% sharing(engineering, medicine, algebra).

%   working days
day(monday).
day(tuesday).
day(wednesday).
day(thursday).
day(friday).

% the sharing knowledge can be deduced according the following `sharing` predicate:
sharing(Course, AnotherCourse, Teaching) :-
        Course \= AnotherCourse
    ,   course(Course, Teaching)
    ,   course(AnotherCourse, Teaching)
    .

valid([]).
valid([schedule(Day, time(Start, End), Room, Teaching, Professor, Courses), Rest]) :-  
        day(Day)
    ,   [Start, End] ins 8..17
    ,   End #> Start
    ,   room(Room, _)
    ,   teaching(Teaching, _)
    ,   teaches(Professor, Teaching)
    ,   setof(C, course(C, Teaching), Courses)  % maybe it is not the best idea to unify a list
    ,   valid(Rest) 
    .
     
acceptable(Schedule) :-
        valid(Schedule)
    ,   every_teacher_teaches_two_teachings(Schedule)
    ,   every_teacher_hasnt_time_overlapping(Schedule)
    ,   every_teacher_teaches_at_most_four_hours_a_day(Schedule)
    ,   no_less_than_two_and_no_more_than_four_hours_per_teaching_in_a_day(Schedule)
    .
          

% The following predicate `schedule` is true if on day D,
% at hour H, in room R, teaching T is taught by professor P,
% for course C
%schedule(D, H, R, T, P, C).

% the following is a simple instantiation
%schedule(monday, 10, toja, algebra, crescenzi, engineering).
