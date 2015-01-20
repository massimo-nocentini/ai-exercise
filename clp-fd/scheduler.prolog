
%:- use_module(library(clpfd)).

% problem definition
room(toja, 35).
room(auditorium, 200).

/*
Predicate `teaching(T, H)` is true iff teaching T is taught H hours a week.
*/
teaching(computer_science, 9).
teaching(algebra, 6).
%teaching(medicine_one, 9).
%teaching(chemistry, 4).

%teaches(bianchi, algebra).
teaches(bianchi, computer_science).
%teaches(bianchi, algorithms).

teaches(verdi, algebra).
teaches(verdi, computer_science).

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

professors_list(Professors)
    :-  setof(Prof, T^teaches(Prof, T), Professors).

satisfy_teaching_hours(Hours_per_teaching_assoc_list, Teaching)
    :-  teaching(Teaching, Hours)
    ,   get_assoc(Teaching, Hours_per_teaching_assoc_list, Hours)
    .

valid(Schedule)
    :-
        Room_occupacy_assoc_list = []

    ,   findall(Teaching-0, teaching(Teaching, _), TeachingsAssociations)
    ,   list_to_assoc(TeachingsAssociations, Hours_per_teaching_assoc_list)

    ,   setof((Prof,Day)-0, T^(day(Day), teaches(Prof, T)), ProfDayPairs)
    ,   list_to_assoc(ProfDayPairs, DailyHoursPerProf)

    ,   setof((T,Day)-0, Prof^(day(Day), teaches(Prof, T)), TeachingDayPairs)
    ,   list_to_assoc(TeachingDayPairs, DailyHoursPerTeaching)

    ,   setof(Prof-[], T^teaches(Prof, T), ProfessorTeachingsPair)
    ,   list_to_assoc(ProfessorTeachingsPair, TaughtTeachingsPerProfessor )

    ,   valid(Schedule,
            Room_occupacy_assoc_list,
            Hours_per_teaching_assoc_list,
            DailyHoursPerProf,
            DailyHoursPerTeaching,
            TaughtTeachingsPerProfessor)
    .

valid([], AssignedTeachingSlots,
        Hours_per_teaching_assoc_list,
        DailyHoursPerProf,
        DailyHoursPerTeaching,
        TaughtTeachingsPerProfessor)
    :-
        teachings_list(Teachings)
    ,   maplist(satisfy_teaching_hours(Hours_per_teaching_assoc_list), Teachings)
    .

valid(  [schedule(Day, time(Start, End), Room, Teaching, Professor, Course)|Rest]
        ,   AssignedTeachingSlots
        ,   Hours_per_teaching_assoc_list
        ,   DailyHoursPerProf
        ,   DailyHoursPerTeaching
        ,   TaughtTeachingsPerProfessor)
    :-
        day(Day)
    ,   room(Room, _)
    ,   teaching(Teaching, MaxTeachingHoursPerWeek)

    ,   get_assoc(Teaching, Hours_per_teaching_assoc_list, TaughtHours)

    ,   between(8, 16, Start)
    ,   MaxTeachingHours is min(4, 17 - Start)
    ,   between(1, MaxTeachingHours, TeachingTime)
    ,   AugmentedTaughtHours is TeachingTime + TaughtHours
    ,   AugmentedTaughtHours =< MaxTeachingHoursPerWeek
    ,   End is Start + TeachingTime

    ,   forall(member(teaching_slot(Day, Room, _, _, OccStart, OccEnd),
                AssignedTeachingSlots),
            ((End =< OccStart) ; (Start >= OccEnd)))

    ,   put_assoc(Teaching, Hours_per_teaching_assoc_list,
            AugmentedTaughtHours, Updated_hours_per_teaching_assoc_list)

    ,   teaches(Professor, Teaching)

    ,   ensure_professor_teaches_at_most_two_teachings(
            Professor, Teaching, TaughtTeachingsPerProfessor,
                AugmentedTaughtTeachingsPerProfessor)

    ,   ensure_professor_teaches_at_most_four_hours_in_a_day(
            Professor, Day, TeachingTime, DailyHoursPerProf, AugmentedDailyHoursPerProf)

    ,   ensure_teaching_is_taught_at_most_four_hours_in_a_day(
            Teaching, Day, TeachingTime, DailyHoursPerTeaching, AugmentedDailyHoursPerTeaching)

    ,   forall(member(teaching_slot(Day, _, _, _, Professor, OccStart, OccEnd),
            AssignedTeachingSlots), ((End =< OccStart) ; (Start >= OccEnd)))

    ,   TeachingSlot = teaching_slot(Day, Room, Teaching, Professor, Start, End)

    ,   course(Course, Teaching)

    ,   AugmentedAssignedTeachingSlots = [TeachingSlot|AssignedTeachingSlots]

    ,   valid(Rest,
            AugmentedAssignedTeachingSlots,
            Updated_hours_per_teaching_assoc_list,
            AugmentedDailyHoursPerProf,
            AugmentedDailyHoursPerTeaching,
            AugmentedTaughtTeachingsPerProfessor)
    .

ensure_professor_teaches_at_most_two_teachings(
        Professor, Teaching, TaughtTeachingsPerProfessor,
        AugmentedTaughtTeachingsPerProfessor)
    :-  get_assoc(Professor, TaughtTeachingsPerProfessor, TaughtTeachings)
    %,   (member(Teaching, TaughtTeachings)
    ,   ((TaughtTeachings = [Teaching] ;
            TaughtTeachings = [Teaching, _] ;
            TaughtTeachings = [_, Teaching])
            ->  AugmentedTaughtTeachingsPerProfessor = TaughtTeachingsPerProfessor
            ;   (
                % length(TaughtTeachings, NumberOfTaughtTeachings),
                % between(0, 1, NumberOfTaughtTeachings),
                    (TaughtTeachings = [] ; TaughtTeachings = [_]),
                    put_assoc(Professor,
                                TaughtTeachingsPerProfessor,
                                [Teaching|TaughtTeachings],
                                AugmentedTaughtTeachingsPerProfessor)
                )
            )
    .

ensure_professor_teaches_at_most_four_hours_in_a_day(
        Professor, Day, TeachingTime, DailyHoursPerProf, AugmentedDailyHoursPerProf)
    :-  get_assoc((Professor, Day), DailyHoursPerProf, ProfDailyTaughtHours)
    ,   AugmentedProfDailyTaughtHours is ProfDailyTaughtHours + TeachingTime
    ,   between(0, 4, AugmentedProfDailyTaughtHours)
    ,   put_assoc((Professor, Day), DailyHoursPerProf, AugmentedProfDailyTaughtHours,
            AugmentedDailyHoursPerProf)
    .

ensure_teaching_is_taught_at_most_four_hours_in_a_day(
        Teaching, Day, TeachingTime, DailyHoursPerProf, AugmentedDailyHoursPerProf)
    :-  get_assoc((Teaching, Day), DailyHoursPerProf, ProfDailyTaughtHours)
    ,   AugmentedProfDailyTaughtHours  is ProfDailyTaughtHours + TeachingTime
    ,   between(2, 4, AugmentedProfDailyTaughtHours)
    ,   put_assoc((Teaching, Day), DailyHoursPerProf, AugmentedProfDailyTaughtHours,
            AugmentedDailyHoursPerProf)
    .

go(Schedules)
    :-
        set_prolog_flag(toplevel_print_options,
            [quoted(true), portray(true), max_depth(100)])
    ,   statistics(runtime,_)
    ,   valid(Schedules)
    ,   statistics(runtime,[_,TimeTaken])
    ,   TimeTakenInSeconds is TimeTaken * 10^(-3)
    ,   number_codes(TimeTakenInSeconds, TimeTakenInSecondsAsString)
    ,   append("Execution time in seconds: ", TimeTakenInSecondsAsString,
                        ExecutionTimeMessage)
    ,   string_codes(Message, ExecutionTimeMessage)
    ,   writeln(Message)
    .
