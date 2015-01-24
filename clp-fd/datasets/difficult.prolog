
room(attic, 20).
room(lab, 30).
room(basement, 40).
room(toja, 50).
room(auditorium, 100).

teaching(computer_science, 9).
teaching(computer_science_medicine, 3).
teaching(algorithms, 6).
teaching(algebra, 6).
teaching(optimization, 6).
teaching(networks, 6).
teaching(crypto, 9).

teaching(curves, 9).
teaching(surfaces, 6).
teaching(geometry, 9).

teaching(chemistry, 9).
teaching(biology, 9).
teaching(health, 6).

teaches(verdi, computer_science).
teaches(rossi, computer_science).

teaches(verdi, computer_science_medicine).
teaches(bianchi, computer_science_medicine).

teaches(bianchi, algorithms).
teaches(rossi, algorithms).

teaches(zueta, algebra).
teaches(ferraio, algebra).

teaches(olevano, optimization).

teaches(olevano, networks).
teaches(verdi, networks).

teaches(zueta, crypto).

teaches(ferraio, curves).
teaches(sabaudo, curves).
teaches(ferrero, curves).

teaches(ferrero, surfaces).
teaches(ferrero, surfaces).

teaches(sabaudo, geometry).
teaches(zueta, geometry).

teaches(fabbri, chemistry).
teaches(tusciano, chemistry).

teaches(parodo, biology).
teaches(fabbri, biology).

teaches(tusciano, health).

course(engineering, computer_science, 60).
course(engineering, optimization, 20).
course(engineering, algebra, 15).
course(engineering, chemistry, 12).
course(engineering, networks, 18).

course(math, algorithms, 10).
course(math, curves, 40).
course(math, surfaces, 25).
course(math, geometry, 15).
course(math, crypto, 25).

course(medicine, computer_science_medicine, 40).
course(medicine, chemistry, 60).
course(medicine, biology, 30).
course(medicine, health, 20).
course(medicine, algebra, 23).

course(science, computer_science, 20).
course(science, optimization, 10).
course(science, algebra, 10).
course(science, algorithms, 30).
course(science, crypto, 10).

