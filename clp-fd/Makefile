
# This makefile contains a rule for each test case we would
# like to report in the TeX document and that shows that requirements
# are met.
#

easy:
	swipl --quiet --nodebug -t 'go(Schedule).' \
		scheduler.prolog datasets/very-first-dataset.prolog

# The following rules works toward not so difficult problems
# trying to show the requested constraints are met.

no-overlapping-for-professor:
	swipl --quiet --nodebug -t 'go(Schedule).' \
		scheduler.prolog datasets/no-overlapping-for-professor.prolog

no-overlapping-for-room:
	swipl --quiet --nodebug -t 'go(Schedule).' \
		scheduler.prolog datasets/no-overlapping-for-room.prolog

more-than-two-teaching-single-professor:
	swipl --quiet --nodebug -t 'go(Schedule).' \
		scheduler.prolog datasets/more-than-two-teaching-single-professor.prolog

more-than-two-teaching-multi-professors:
	swipl --quiet --nodebug -t 'go(Schedule).' \
		scheduler.prolog datasets/more-than-two-teaching-multi-professors.prolog

at-most-four-day-hours-for-prof:
	swipl --quiet --nodebug -t 'go(Schedule).' \
		scheduler.prolog datasets/at-most-four-day-hours-for-prof.prolog

teaching-with-one-hour-a-week:
	swipl --quiet --nodebug -t 'go(Schedule).' \
		scheduler.prolog datasets/teaching-with-one-hour-a-week.prolog

teaching-shared-by-courses-too-many-students:
	swipl --quiet --nodebug -t 'go(Schedule).' \
		scheduler.prolog datasets/teaching-shared-by-courses-too-many-students.prolog

teaching-shared-by-courses-no-overlapping:
	swipl --quiet --nodebug -t 'go(Schedule).' \
		scheduler.prolog datasets/teaching-shared-by-courses-no-overlapping.prolog

# The following rules is for a more difficult instances.

difficult:
	swipl --quiet --nodebug -t 'go(Schedule).' \
		scheduler.prolog datasets/difficult.prolog
