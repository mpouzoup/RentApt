/*
  ΟΜΑΔΑ:
  Φοιτητής 1: Μπουζούκη Πολυξένη - 4535
  Φοιτητής 2: Παπαδοπούλου Ουρανία - 4499

  Περιγραφή: Πρόγραμμα για εύρεση διαμερισμάτων σύμφωνα με τις απαιτήσεις πελατών.
  Περιέχει υλοποίηση των λειτουργιών: διαδραστική, μαζική και δημοπρασία.
*/

:- consult('houses.pl').
:- consult('requests.pl').

:- dynamic house/9.
:- dynamic request/11.

% ---------------------- Αρχικό Κατηγόρημα ----------------------
run :-
    menu.

menu :-
    repeat,
    nl, write('Μενού:'), nl,
    write('1 - Προτιμήσεις ενός πελάτη'), nl,
    write('2 - Μαζικές προτιμήσεις πελατών'), nl,
    write('3 - Επιλογή πελατών μέσω δημοπρασίας'), nl,
    write('0 - Έξοδος'), nl,
    write('Επιλογή: '),
    read(Choice),
    handle_choice(Choice),
    Choice == 0.

handle_choice(1) :-
    interactive_mode,
    fail.
handle_choice(2) :-
    batch_mode,
    fail.
handle_choice(3) :-
    auction_mode,
    fail.
handle_choice(0) :-
    write('Έξοδος...'), nl.
handle_choice(_) :-
    write('Επίλεξε έναν αριθμό μεταξύ 0 έως 3!'), nl,
    fail.

% ---------------------- Διαδραστική Λειτουργία ----------------------
interactive_mode :-
    write('=============================='), nl,
    write('Ελάχιστο Εμβαδόν: '), read(MinArea),
    write('Ελάχιστος αριθμός υπνοδωματίων: '), read(MinBedrooms),
    write('Να επιτρέπονται κατοικίδια; (yes/no) '), read(Pets),
    write('Από ποιον όροφο και πάνω να υπάρχει ανελκυστήρας; '), read(ElevatorFloorLimit),
    write('Μέγιστο ενοίκιο: '), read(MaxTotal),
    write('Μέγιστο στο κέντρο (για ελάχιστα τ.μ.): '), read(MaxCenter),
    write('Μέγιστο σε προάστια (για ελάχιστα τ.μ.): '), read(MaxSuburb),
    write('Επιπλέον €/m2 διαμερίσματος: '), read(ExtraPerM2),
    write('Επιπλέον €/m2 κήπου: '), read(ExtraPerGarden),
    
    findall(H, house_tuple(H), Houses),
    Requirements = request(temp, MinArea, MinBedrooms, Pets, ElevatorFloorLimit, MaxTotal, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden, MaxTotal),
    compatible_houses(Requirements, Houses, Suitable),
    print_houses(Suitable),
    find_best_house(Suitable, Best),
    ( Best = house(Address, _, _, _, _, _, _, _, _) ->
        write('Προτείνεται η ενοικίαση του διαμερίσματος στην διεύθυνση: '), write(Address), nl
    ;
        write('Δεν υπάρχει κατάλληλο σπίτι!'), nl
    ).

% ---------------------- Ορισμοί για εύρεση συμβατότητας ----------------------
house_tuple(house(A,B,C,D,E,F,G,H,I)) :- house(A,B,C,D,E,F,G,H,I).

compatible_houses(_, [], []).
compatible_houses(Req, [H|T], [H|Res]) :-
    compatible_house(Req, H), !,
    compatible_houses(Req, T, Res).
compatible_houses(Req, [_|T], Res) :-
    compatible_houses(Req, T, Res).

% Dummy για παράδειγμα (να συμπληρωθεί με όλους τους ελέγχους συμβατότητας)
compatible_house(request(_, MinArea, MinBed, Pets, ElevatorLimit, _, _, _, _, _, _),
                 house(_, Bed, Area, PetsAllowed, Floor, _, _, _, _)) :-
    Area >= MinArea,
    Bed >= MinBed,
    (Floor >= ElevatorLimit),  % αν θέλεις να είναι στον όροφο ή πάνω από το όριο με ασανσέρ
    ( Pets == PetsAllowed ; Pets == yes, PetsAllowed == yes ).



% ---------------------- Εκτύπωση Σπιτιών ----------------------
print_houses([]).
print_houses([house(Address, B, A, F, G, E, C, P, R)|T]) :-
    write('Κατάλληλο σπίτι στην διεύθυνση: '), write(Address), nl,
    write('Υπνοδωμάτια: '), write(B), nl,
    write('Εμβαδόν: '), write(A), nl,
    write('Εμβαδόν κήπου: '), write(G), nl,
    write('Είναι στο κέντρο της πόλης: '), write(C), nl,
    write('Επιτρέπονται κατοικίδια: '), write(P), nl,
    write('Όροφος: '), write(F), nl,
    write('Ανελκυστήρας: '), write(E), nl,
    write('Ενοίκιο: '), write(R), nl, nl,
    print_houses(T).

% ---------------------- Προτεινόμενο σπίτι ----------------------
find_best_house(Houses, Best) :-
    find_cheaper(Houses, Cheapest),
    find_biggest_garden(Cheapest, GardenMax),
    find_biggest_house(GardenMax, [Best|_]).

find_cheaper(Houses, Cheapest) :-
    findall(R, member(house(_,_,_,_,_,_,_,_,R), Houses), Prices),
    min_list(Prices, Min),
    include({Min}/[house(_,_,_,_,_,_,_,_,R)]>>(R=:=Min), Houses, Cheapest).

find_biggest_garden(Houses, MaxGardens) :-
    findall(G, member(house(_,_,_,_,G,_,_,_,_), Houses), Gardens),
    max_list(Gardens, Max),
    include({Max}/[house(_,_,_,_,G,_,_,_,_)]>>(G=:=Max), Houses, MaxGardens).

find_biggest_house(Houses, MaxHouses) :-
    findall(A, member(house(_,_,A,_,_,_,_,_,_), Houses), Areas),
    max_list(Areas, Max),
    include({Max}/[house(_,_,A,_,_,_,_,_,_)]>>(A=:=Max), Houses, MaxHouses).

% ---------------------- Batch Mode (Placeholder) ----------------------
batch_mode :-
    findall(request(N, A, B, P, E, M, MC, MS, EP, EG, MT), request(N, A, B, P, E, M, MC, MS, EP, EG, MT), Requests),
    forall(member(R, Requests), (
        R = request(Name, _, _, _, _, _, _, _, _, _, _),
        write('Κατάλληλα διαμερίσματα για τον πελάτη: '), write(Name), nl,
        findall(H, house_tuple(H), Houses),
        compatible_houses(R, Houses, Suitable),
        print_houses(Suitable),
        find_best_house(Suitable, Best),
        ( Best = house(Address, _, _, _, _, _, _, _, _) ->
            write('Προτείνεται η ενοικίαση του διαμερίσματος στην διεύθυνση: '), write(Address), nl
        ;
            write('Δεν υπάρχει κατάλληλο σπίτι!'), nl
        ), nl
    )).

% ---------------------- Δημοπρασία (Placeholder) ----------------------
auction_mode :-
    write('Η λειτουργία δημοπρασίας δεν έχει ακόμη υλοποιηθεί.'), nl.
