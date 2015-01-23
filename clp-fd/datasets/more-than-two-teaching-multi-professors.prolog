
room(toja, 35).

teaching(computer_science, 6).
teaching(algebra, 6).
teaching(chemistry, 6).


teaches(verdi, algebra).
teaches(verdi, computer_science).
teaches(verdi, chemistry).

% observe that putting this clause here makes Prolog
% to attempt more paths respect putting it before
% clauses relative to professor `verdi`
teaches(bianchi, algebra).

course(engineering, computer_science, 30).
course(engineering, algebra, 20).
course(engineering, chemistry, 10).

