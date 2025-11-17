% Kaia Kanj (kmk233) and Asya Akkus (aya29)

%this portion of the script lists out all the courses 

corecourses([csds132, csds233, csds281, csds302, csds310, csds395]).

breadth(1, [csds341, csds356, csds390, csds393]).
breadth(2, [csds312, csds314, csds325, csds338]).
breadth(3, [csds337, csds343, csds344, csds345]). 
breadth(4, [csds313, csds335, csds340, csds391]). 

security([csds344, csds356, csds427, csds444, csds448]). 

% Computer Science technical electives
elective([csds301, csds303, csds315, csds317, csds419, csds484, csds431, csds490, csds491, csds492]).

% this portion of the code lists out prereqs for the courses
prereq(csds132, []).
prereq(csds233, [csds132]).
prereq(csds281, [csds132]).
prereq(csds302, []).
prereq(csds310, [csds302, csds233]). 
prereq(csds395, []).

% these are courses that require a language 
course_language(csds132, 'Java').
course_language(csds233, 'Java').
course_language(csds345, 'C').
course_language(csds337, 'C').

% print major requirements
print_list([]).
print_list([H|T]) :-
    format('~w~n', [H]),
    print_list(T).

% determine whether a course is an elective 
is_elective(Course) :-
    elective(List), 
    (member(Course, List)
    ->  format('~w is an elective ~n', [Course])
    ;   format('~w is not an elective ~n', [Course])).

% determine what is a prereq
is_prereq(Course) :-
    prereq(Course, Prereqs), 
    print_prereq(Prereqs).
% print out the prereqs on the screen 
print_prereq([]).
print_prereq([H|T]) :-
    format('    Prereq: ~w~n', [H]),
    (prereq(H, P2) -> print_prereq(P2) ; true),
    print_prereq(T).

% make menu

menu :-
    write('Welcome to the Logical Advisor'), nl,
    write('What would you like to do?'), nl,
    write('    1. Show Major Requirements'), nl,
    write('    2. Check Elective Course'), nl,
    write('    3. Check Prerequisites'), nl,
    write('    4. Check Courses for Language'), nl,
    write('    Anything Else. Quit'), nl,
    write('Enter Choice: '),
    read(Choice),
    handler(Choice). 
	
	handler(1) :- 
    	write('Core Courses'), nl,
    	corecourses(C), print_list(C), nl,
        forall(breadth(N, List),
           (format('Breadth Area ~w Courses (Choose 2)~n', [N]),
            print_list(List), nl)),
    	write('Security Course (Choose 1)'), nl,
		security(S), print_list(S), nl, nl,
        menu. % loop back to menu 
	handler(2) :-
    	write('Enter Elective Course'), 
    	read(C), 
		is_elective(C), nl, nl, 
        menu.
	handler(3) :-
    	write('Enter Course to get prereqs'),
    	read(C),
    	is_prereq(C), nl, nl,
        menu.
	handler(4) :-
        write('Enter language: '), nl,
        read(Lang),
        findall(Course, course_language(Course, Lang), Courses),
        (   Courses = [] ->
            format('  Sorry, no courses include ~w~n', [Lang])
        ;   format('  The below courses include ~w~n', [Lang]),
            print_list(Courses)
        ),
        nl, menu.

    handler(_) :- % any other input quits
        write('Have a good day!'), nl.
    





