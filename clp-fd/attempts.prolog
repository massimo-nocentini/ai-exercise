
:- use_module(library(clpfd)).

% problem definition
room(toja, 35).
room(auditorium, 200).

/*
Predicate `teaching(T, H)` is true iff teaching T is taught H hours a week.
*/
teaching(computer_science, 9).
%teaching(algebra, 6).
%teaching(medicine_one, 9).
%teaching(chemistry, 4).

%teaches(bianchi, algebra).
teaches(bianchi, computer_science).
%teaches(bianchi, algorithms).

teaches(verdi, algebra).
teaches(verdi, algebra_two).

teaches(rossi, medicine_one).

course(engineering, computer_science).
%course(engineering, algebra).
%course(engineering, chemistry).

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

teachings_list(Teachings)
    :-  findall(Teaching, teaching(Teaching, _), Teachings).

satisfy_teaching_hours(Hours_per_teaching_assoc_list, Teaching) 
    :-  teaching(Teaching, Hours)
    ,   get_assoc(Teaching, Hours_per_teaching_assoc_list, Hours)
    .  

valid(Schedule)
    :-  Room_occupacy_assoc_list = []

    ,   findall(Teaching-0, teaching(Teaching, _), TeachingsAssociations)
    ,   list_to_assoc(TeachingsAssociations, Hours_per_teaching_assoc_list)

    ,   valid(Schedule, Room_occupacy_assoc_list, Hours_per_teaching_assoc_list)
    .

valid([], Room_occupacy_list, Hours_per_teaching_assoc_list)
    :-  teachings_list(Teachings)
    ,   maplist(satisfy_teaching_hours(Hours_per_teaching_assoc_list), Teachings)
    .

valid(  [schedule(Day, time(Start, End), Room, Teaching, Professor, Course)|Rest]
        ,   AssignedTeachingSlots
        ,   Hours_per_teaching_assoc_list    ) 
    :-  day(Day)
    ,   room(Room, _)
    ,   teaching(Teaching, MaxTeachingHoursPerWeek)
    
    ,   get_assoc(Teaching, Hours_per_teaching_assoc_list, TaughtHours)

    ,   between(8, 16, Start)
    ,   MaxTeachingHours is min(4, 17 - Start)
    ,   between(1, MaxTeachingHours, TeachingTime)
    ,   AugmentedTaughtHours is TeachingTime + TaughtHours
    ,   AugmentedTaughtHours =< MaxTeachingHoursPerWeek
    ,   End is Start + TeachingTime

    ,   forall(member(teaching_slot(Day, Room, _, _, OccStart, OccEnd), AssignedTeachingSlots), 
            ((End =< OccStart) ; (Start >= OccEnd)))

    ,   put_assoc(Teaching, Hours_per_teaching_assoc_list, 
            AugmentedTaughtHours, Updated_hours_per_teaching_assoc_list)

    ,   teaches(Professor, Teaching)
    ,   TeachingSlot = teaching_slot(Day, Room, Teaching, Professor, Start, End)

    ,   course(Course, Teaching)

    ,   valid(Rest, 
            [TeachingSlot|AssignedTeachingSlots], 
            Updated_hours_per_teaching_assoc_list) 
    .
     
acceptable(Schedule) :-
        valid(Schedule)
    ,   every_teacher_teaches_two_teachings(Schedule)
%    ,   every_teacher_hasnt_time_overlapping(Schedule)
%    ,   every_teacher_teaches_at_most_four_hours_a_day(Schedule)
%    ,   no_less_than_two_and_no_more_than_four_hours_per_teaching_in_a_day(Schedule)
    .
          
every_teacher_teaches_two_teachings(Schedule) :-
        setof(P, T^teaches(P, T), Professors) 
    ,   maplist(teachings_taught_by_prof(2, Schedule), Professors)
    .

teachings_taught_by_prof(NumOfTeachings, Schedule, Professor) :-
        include(schedule_for_prof(Professor), Schedule, SchedulesForProfessor)
    ,   maplist(proj_teaching, SchedulesForProfessor, TeachingForProfessor)
    ,   write(TeachingForProfessor)
    ,   length(TeachingForProfessor, NumOfTeachings)
    .
schedule_for_prof(Professor, schedule(_, _, _, _, Professor, _)).
proj_teaching(schedule(_, _, _, Teaching, _, _), Teaching).

        
        
        %foldl((schedule(_, time(Start, End), _, _, Professor, _) 

% The following predicate `schedule` is true if on day D,
% at hour H, in room R, teaching T is taught by professor P,
% for course C
%schedule(D, H, R, T, P, C).

% the following is a simple instantiation
%schedule(monday, 10, toja, algebra, crescenzi, engineering).
