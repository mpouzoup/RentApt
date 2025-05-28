% main.pl
% Μπουζούκη Πολυξένη
% ΑΕΜ:4535
 
:- dynamic house/9.
:- dynamic request/10.
 
:- ['houses'].
:- ['requests'].
 
% ========== Κύριο πρόγραμμα ==========
run :-
    repeat,
    format('\nΜενού:\n======\n'),
    format('1 - Προτιμήσεις ενός πελάτη\n'),
    format('2 - Μαζικές προτιμήσεις πελατών\n'),
    format('3 - Επιλογή πελατών μέσω δημοπρασίας\n'),
    format('0 - Έξοδος\n\n'),
    write('Επιλογή: '),
    read(Choice),
    handle_choice(Choice),
    Choice = 0.
 
handle_choice(1) :- interactive_mode, !.
handle_choice(2) :- batch_mode, !.
handle_choice(3) :- auction_mode, !.
handle_choice(0) :- writeln('Έξοδος από το πρόγραμμα.'), !.
handle_choice(_) :- writeln('Μη έγκυρη επιλογή, προσπαθήστε ξανά.'), fail.
 
% ========== Διαδραστική λειτουργία ==========
interactive_mode :-
    format('\nΔώσε τις παρακάτω πληροφορίες:\n'),
    write('Ελάχιστο Εμβαδόν: '), read(MinSize),
    write('Ελάχιστος αριθμός υπνοδωματίων: '), read(MinRooms),
    write('Να επιτρέπονται κατοικίδια; (yes/no): '), read(Pets),
    write('Από ποιον όροφο και πάνω να υπάρχει ανελκυστήρας; '), read(FloorLimit),
    write('Μέγιστο συνολικό ενοίκιο: '), read(MaxRent),
    write('Μέγιστο ενοίκιο για σπίτι στο κέντρο: '), read(MaxCenter),
    write('Μέγιστο ενοίκιο για σπίτι στα προάστια: '), read(MaxSuburb),
    write('Επιπλέον €/m2 για παραπάνω εμβαδόν: '), read(ExtraPerM2),
    write('Επιπλέον €/m2 για κήπο: '), read(ExtraPerGarden),
 
    Request = request(dummy, MinSize, MinRooms, Pets, FloorLimit, MaxRent, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden),
 
    findall(House, (house_info(House), compatible_house(Request, House)), AllHouses),
    include(acceptable_offer(Request), AllHouses, SuitableHouses),
 
    (
        SuitableHouses = [] ->
            writeln('\nΔεν υπάρχει κατάλληλο σπίτι!')
        ;
            print_houses(SuitableHouses),
            find_best_house(SuitableHouses, BestHouse),
            BestHouse = house(Address, _, _, _, _, _, _, _, _),
            format('\nΠροτείνεται η ενοικίαση του διαμερίσματος στην διεύθυνση: ~w\n', [Address])
    ).
 
% ========== Έλεγχος συμβατότητας ==========
compatible_house(
    request(_, MinSize, MinRooms, Pets, FloorLimit, _, _, _, _, _),
    house(_, Rooms, Size, Floor, _, _, Lift, _, Pets)
) :-
    Size >= MinSize,
    Rooms >= MinRooms,
    (Floor < FloorLimit ; Lift == yes),
    true.
 
% ========== Υπολογισμός Προσφοράς ==========
acceptable_offer(Request, House) :-
    offer(Request, House, Price),
    Request = request(_, _, _, _, _, MaxRent, _, _, _, _),
    Price =< MaxRent.
 
offer(
    request(_, MinSize, _, _, _, _, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden),
    house(_, _, Size, _, _, Garden, _, Center, _),
    FinalRent
) :-
    (Center == yes -> Base is MaxCenter ; Base is MaxSuburb),
    ExtraSize is max(0, Size - MinSize),
    Extra1 is ExtraSize * ExtraPerM2,
    Extra2 is Garden * ExtraPerGarden,
    FinalRent is Base + Extra1 + Extra2.
 
% ========== Επιλογή Καλύτερου ==========
find_best_house(Houses, Best) :-
    find_cheapest(Houses, CheapestList),
    find_biggest_garden(CheapestList, GardenList),
    find_biggest_house(GardenList, [Best|_]).
 
find_cheapest(Houses, CheapestList) :-
    map_list_to_pairs(get_rent, Houses, Pairs),
    keysort(Pairs, Sorted),
    Sorted = [Min-_|_],
    include(=(Min-_), Sorted, Filtered),
    pairs_values(Filtered, CheapestList).
 
find_biggest_garden(Houses, Result) :-
    map_list_to_pairs(get_garden, Houses, Pairs),
    keysort(Pairs, Sorted),
    reverse(Sorted, Rev),
    Rev = [Max-_|_],
    include(=(Max-_), Rev, Filtered),
    pairs_values(Filtered, Result).
 
find_biggest_house(Houses, Result) :-
    map_list_to_pairs(get_size, Houses, Pairs),
    keysort(Pairs, Sorted),
    reverse(Sorted, Rev),
    Rev = [Max-_|_],
    include(=(Max-_), Rev, Filtered),
    pairs_values(Filtered, Result).
 
get_rent(house(_, _, _, _, Rent, _, _, _, _), Rent).
get_garden(house(_, _, _, _, _, Garden, _, _, _), Garden).
get_size(house(_, _, Size, _, _, _, _, _, _), Size).
 
% ========== Χρήσιμα ==========
house_info(H) :- house(A, B, C, D, E, F, G, H1, I), H = house(A, B, C, D, E, F, G, H1, I).
 
print_houses([]).
print_houses([house(Address, R, S, F, Rent, Garden, Lift, Center, Pets)|T]) :-
    format('\nΚατάλληλο σπίτι στην διεύθυνση: ~w\nΥπνοδωμάτια: ~w\nΕμβαδόν: ~w\nΌροφος: ~w\nΕνοίκιο: ~w\nΚήπος: ~w\nΑνελκυστήρας: ~w\nΚέντρο: ~w\nΚατοικίδια: ~w\n',
           [Address, R, S, F, Rent, Garden, Lift, Center, Pets]),
    print_houses(T).
 
batch_mode :-
    findall(
        request(Name, MinSize, MinRooms, Pets, FloorLimit, MaxRent, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden),
        request(Name, MinSize, MinRooms, Pets, FloorLimit, MaxRent, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden),
        Requests
    ),
    process_all_requests(Requests).
 
process_all_requests([]).
process_all_requests([request(Name, MinSize, MinRooms, Pets, FloorLimit, MaxRent, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden)|T]) :-
    format('\nΚατάλληλα διαμερίσματα για τον πελάτη: ~w:\n', [Name]),
    Request = request(Name, MinSize, MinRooms, Pets, FloorLimit, MaxRent, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden),
    findall(House, (house_info(House), compatible_house(Request, House)), AllHouses),
    include(acceptable_offer(Request), AllHouses, SuitableHouses),
 
    (
        SuitableHouses = [] ->
            writeln('Δεν υπάρχει κατάλληλο σπίτι!')
        ;
            print_houses(SuitableHouses),
            find_best_house(SuitableHouses, house(Address, _, _, _, _, _, _, _, _)),
            format('\nΠροτείνεται η ενοικίαση του διαμερίσματος στην διεύθυνση: ~w\n', [Address])
    ),
    process_all_requests(T).
 
 