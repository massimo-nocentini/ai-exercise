%:- use_module(library(clpfd)).

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

courses_on_teaching(Teaching, Courses)
    :-  findall(Course, course(Course, Teaching, _), Courses).

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

valid([], _, Hours_per_teaching_assoc_list, _, _, _)
    :-
        teachings_list(Teachings)
    ,   maplist(satisfy_teaching_hours(Hours_per_teaching_assoc_list), Teachings)
    .

valid(  [schedule(Day, time(Start, End), Room, Teaching, Professor, Courses)|Rest]
        ,   AssignedTeachingSlots
        ,   Hours_per_teaching_assoc_list
        ,   DailyHoursPerProf
        ,   DailyHoursPerTeaching
        ,   TaughtTeachingsPerProfessor)
    :-

        choose_teaching_with_time_slot(
            Teaching, Start, End, TeachingTime,
            Hours_per_teaching_assoc_list,
            Updated_hours_per_teaching_assoc_list)

    ,   teaches(Professor, Teaching)

    ,   ensure_professor_teaches_at_most_two_teachings(
            Professor, Teaching, TaughtTeachingsPerProfessor,
                AugmentedTaughtTeachingsPerProfessor)

    ,   day(Day)

    ,   ensure_professor_teaches_at_most_four_hours_in_a_day(
            Professor, Day, TeachingTime, DailyHoursPerProf, AugmentedDailyHoursPerProf)

    ,   ensure_teaching_is_taught_at_most_four_hours_in_a_day(
            Teaching, Day, TeachingTime, DailyHoursPerTeaching, AugmentedDailyHoursPerTeaching)

    ,   ensure_no_overlapping_for_professor_in_the_same_day(
            Professor, Day, Start, End, AssignedTeachingSlots)

    ,   ensure_no_overlapping_for_teaching_in_the_same_day(
            Teaching, Day, Start, End, AssignedTeachingSlots)

    ,   choose_room_with_enough_sittings_for_teaching(Room, Teaching)

    ,   ensure_no_overlapping_for_room_in_the_same_day(
            Room, Day, Start, End, AssignedTeachingSlots)

    ,   courses_on_teaching(Teaching, Courses)

    ,   AugmentedAssignedTeachingSlots = [
            teaching_slot(Day, Room, Teaching, Professor, Start, End) |
            AssignedTeachingSlots]

    ,   valid(Rest,
            AugmentedAssignedTeachingSlots,
            Updated_hours_per_teaching_assoc_list,
            AugmentedDailyHoursPerProf,
            AugmentedDailyHoursPerTeaching,
            AugmentedTaughtTeachingsPerProfessor)
    .

choose_room_with_enough_sittings_for_teaching(Room, Teaching)
    :-
        room(Room, AvailableSittingsInRoom)

    ,   findall(StudentsNumber, course(_, Teaching, StudentsNumber), StudentsNumbers)
    ,   foldl(plus, StudentsNumbers, 0, ComprehensiveStudentsFollowingTeaching)
    ,   ComprehensiveStudentsFollowingTeaching =< AvailableSittingsInRoom
    .

ensure_no_overlapping_for_room_in_the_same_day(
        Room, Day, Start, End, AssignedTeachingSlots)
    :-
        forall(member(teaching_slot(Day, Room, _, _, OccStart, OccEnd),
            AssignedTeachingSlots), ((End =< OccStart) ; (Start >= OccEnd)))
    .

ensure_no_overlapping_for_professor_in_the_same_day(
        Professor, Day, Start, End, AssignedTeachingSlots)
    :-
        forall(member(teaching_slot(Day, _, _, Professor, OccStart, OccEnd),
            AssignedTeachingSlots), ((End =< OccStart) ; (Start >= OccEnd)))
    .

ensure_no_overlapping_for_teaching_in_the_same_day(
        Teaching, Day, Start, End, AssignedTeachingSlots)
    :-
        forall(member(teaching_slot(Day, _, Teaching, _, OccStart, OccEnd),
            AssignedTeachingSlots), ((End =< OccStart) ; (Start >= OccEnd)))
    .

choose_teaching_with_time_slot(
        Teaching, Start, End, TeachingTime,
        Hours_per_teaching_assoc_list,
        Updated_hours_per_teaching_assoc_list)
    :-
        teaching(Teaching, MaxTeachingHoursPerWeek)

    ,   get_assoc(Teaching, Hours_per_teaching_assoc_list, TaughtHours)

    ,   between(8, 16, Start)
    ,   MaxTeachingHours is min(4, 17 - Start)
    ,   between(1, MaxTeachingHours, TeachingTime)
    ,   AugmentedTaughtHours is TeachingTime + TaughtHours
    ,   AugmentedTaughtHours =< MaxTeachingHoursPerWeek
    ,   End is Start + TeachingTime

    ,   put_assoc(Teaching, Hours_per_teaching_assoc_list,
            AugmentedTaughtHours, Updated_hours_per_teaching_assoc_list)
    .

ensure_professor_teaches_at_most_two_teachings(
        Professor, Teaching, TaughtTeachingsPerProfessor,
        AugmentedTaughtTeachingsPerProfessor)
    :-
        get_assoc(Professor, TaughtTeachingsPerProfessor, TaughtTeachings)
    ,   length(TaughtTeachings, L)
    ,   (L > 2 -> fail ; true)
    ,   ((TaughtTeachings = [Teaching] ;
            TaughtTeachings = [Teaching, _] ;
            TaughtTeachings = [_, Teaching])
            ->  AugmentedTaughtTeachingsPerProfessor = TaughtTeachingsPerProfessor
            ;   (
                    (TaughtTeachings = [] ; TaughtTeachings = [_]) ->
                        put_assoc(Professor,
                                    TaughtTeachingsPerProfessor,
                                    [Teaching|TaughtTeachings],
                                    AugmentedTaughtTeachingsPerProfessor)
                    ; fail
                )
            )
    .

ensure_professor_teaches_at_most_four_hours_in_a_day(
        Professor, Day, TeachingTime, DailyHoursPerProf, AugmentedDailyHoursPerProf)
    :-
        get_assoc((Professor, Day), DailyHoursPerProf, ProfDailyTaughtHours)
    ,   AugmentedProfDailyTaughtHours is ProfDailyTaughtHours + TeachingTime
    ,   between(0, 4, AugmentedProfDailyTaughtHours)
    ,   put_assoc((Professor, Day), DailyHoursPerProf, AugmentedProfDailyTaughtHours,
            AugmentedDailyHoursPerProf)
    .

ensure_teaching_is_taught_at_most_four_hours_in_a_day(
        Teaching, Day, TeachingTime, DailyHoursPerProf, AugmentedDailyHoursPerProf)
    :-
        get_assoc((Teaching, Day), DailyHoursPerProf, ProfDailyTaughtHours)
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
    ,   writeln(Schedules)
    .
