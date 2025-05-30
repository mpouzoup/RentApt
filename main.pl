/*
  ΟΜΑΔΑ:
  Φοιτητής 1: Μπουζούκη Πολυξένη - 4535
  Φοιτητής 2: Παπαδοπούλου Ουρανία - 4499
  Περιγραφή: Πρόγραμμα για εύρεση διαμερισμάτων σύμφωνα με τις απαιτήσεις πελατών.
*/

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
    format('3 - Επιλογή πελατών μέσω δημοπρασίας (υπό υλοποίηση)\n'),
    format('0 - Έξοδος\n\n'),
    write('Επιλογή: '),
    read(Choice),
    handle_choice(Choice),
    Choice = 0.

handle_choice(1) :- interactive_mode, !.
handle_choice(2) :- batch_mode, !.
handle_choice(3) :- auction_mode , !.
handle_choice(0) :- writeln('Έξοδος από το πρόγραμμα.'), !.
handle_choice(_) :- writeln('Επίλεξε έναν αριθμό μεταξύ 0 έως 3!'), fail.

% ========== Διαδραστική λειτουργία ==========

interactive_mode :-
    format('\nΔώσε τις παρακάτω πληροφορίες:\n'),
    write('Ελάχιστο Εμβαδόν: '), read(MinSize),
    write('Ελάχιστος αριθμός υπνοδωματίων: '), read(MinRooms),
    write('Να επιτρέπονται κατοικίδια; (yes/no): '), read(Pets),
    write('Από ποιον όροφο και πάνω να υπάρχει ανελκυστήρας; '), read(FloorLimit),
    write('Ποιο είναι το μέγιστο ενοίκιο που μπορείς να πληρώσεις; '), read(MaxRent),
    write('Πόσα θα έδινες για ένα διαμέρισμα στο κέντρο της πόλης (στα ελάχιστα τετραγωνικά); '), read(MaxCenter),
    write('Πόσα θα έδινες για ένα διαμέρισμα στα προάστια της πόλης (στα ελάχιστα τετραγωνικά); '), read(MaxSuburb),
    write('Πόσα θα έδινες για κάθε τετραγωνικό διαμερίσματος πάνω από το ελάχιστο; '), read(ExtraPerM2),
    write('Πόσα θα έδινες για κάθε τετραγωνικό κήπου; '), read(ExtraPerGarden),

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

% ========== Μαζική λειτουργία ==========

batch_mode :-
    findall(
        request(Name, MinSize, MinRooms, Pets, FloorLimit, MaxRent, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden),
        request(Name, MinSize, MinRooms, Pets, FloorLimit, MaxRent, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden),
        Requests
    ),
    process_all_requests(Requests).

process_all_requests([]).
process_all_requests([Request|T]) :-
    Request = request(Name, MinSize, MinRooms, Pets, FloorLimit, MaxRent, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden),
    format('\nΚατάλληλα διαμερίσματα για τον πελάτη: ~w:\n', [Name]),
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

% ========== Εμφάνιση και επεξεργασία δεδομένων ==========

house_info(
    house(Address, Rooms, Size, Floor, Rent, Garden, Lift, Center, Pets)
) :-
    house(Address, Rooms, Size, Pets, Floor, Lift, Center, Garden, Rent).

print_houses([]).
print_houses([house(Address, R, S, F, Rent, Garden, Lift, Center, Pets)|T]) :-
    format('\nΚατάλληλο σπίτι στην διεύθυνση: ~w\n', [Address]),
    format('Υπνοδωμάτια: ~w\n', [R]),
    format('Εμβαδόν: ~w\n', [S]),
    format('Εμβαδόν κήπου: ~w\n', [Garden]),
    format('Είναι στο κέντρο της πόλης: ~w\n', [Center]),
    format('Επιτρέπονται κατοικίδια: ~w\n', [Pets]),
    format('Όροφος: ~w\n', [F]),
    format('Ανελκυστήρας: ~w\n', [Lift]),
    format('Ενοίκιο: ~w\n', [Rent]),
    print_houses(T).

% ========== Συμβατότητα και προσφορές ==========

compatible_house(
    request(_, MinSize, MinRooms, Pets, FloorLimit, _, _, _, _, _),
    house(_, Rooms, Size, Floor, _, _, Lift, _, HousePets)
) :-
    Size >= MinSize,
    Rooms >= MinRooms,
    (Floor < FloorLimit -> true ; Lift == yes),
    (Pets == yes -> HousePets == yes ; true).  % Εάν ο πελάτης θέλει κατοικίδια, το σπίτι πρέπει να τα επιτρέπει
acceptable_offer(Request, House) :-
    offer(Request, House, Price),
    Request = request(_, _, _, _, _, MaxRent, _, _, _, _),
    format('Debug: House ~w, Price ~w, MaxRent ~w~n', [House, Price, MaxRent]),  % Γραμμή για debugging
    Price =< MaxRent.

offer(
    request(_, MinSize, _, _, _, MaxRent, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden),
    house(_, _, Size, _, Rent, Garden, _, Center, _),
    FinalRent
) :-
    (Center == yes -> Base is MaxCenter ; Base is MaxSuburb),
    ExtraSize is max(0, Size - MinSize),
    Extra1 is ExtraSize * ExtraPerM2,
    Extra2 is Garden * ExtraPerGarden,
    CalculatedRent is Base + Extra1 + Extra2,
    % Η τελική τιμή δεν μπορεί να ξεπερνά το MaxRent ΟΥΤΕ το Rent του σπιτιού
    FinalRent is min(CalculatedRent, min(MaxRent, Rent)).
% ========== Επιλογή καλύτερου σπιτιού ==========

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

auction_mode :-
    writeln('Auction mode starting...'),
    findall(
        request(Name, MinSize, MinRooms, Pets, FloorLimit, MaxRent, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden),
        request(Name, MinSize, MinRooms, Pets, FloorLimit, MaxRent, MaxCenter, MaxSuburb, ExtraPerM2, ExtraPerGarden),
        Requests
    ),
    findall(
        house(Address, Rooms, Size, Pets, Floor, Lift, Center, Garden, Rent),
        house(Address, Rooms, Size, Pets, Floor, Lift, Center, Garden, Rent),
        Houses
    ),
    auction_allocate(Requests, Houses, [], Allocations),
    print_allocations(Requests, Allocations).

print_allocations([], _).
print_allocations([request(Name, _, _, _, _, _, _, _, _, _)|Rest], Allocations) :-
    ( member(allocation(Name, house(Address, _, _, _, _, _, _, _, _)), Allocations) ->
        format('O πελάτης ~w θα νοικιάσει το διαμέρισμα στην διεύθυνση: ~w~n', [Name, Address])
    ;
        format('O πελάτης ~w δεν θα νοικιάσει κάποιο διαμέρισμα!~n', [Name])
    ),
    print_allocations(Rest, Allocations).

% Ενημερωμένη υλοποίηση ώστε να καταγράφει και αποτυχίες

auction_allocate([], _, Allocations, Allocations).
auction_allocate([Request|Rest], Houses, TakenHouses, FinalAllocations) :-
    Request = request(Name, MinSize, MinRooms, Pets, FloorLimit, MaxRent, StartRent, _, _, _),
    exclude({TakenHouses}/[H]>>member(H, TakenHouses), Houses, RemainingHouses),
    findall(House,
        (
            member(House, RemainingHouses),
            House = house(_, HouseRooms, HouseSize, HousePets, HouseFloor, HouseLift, _, _, HouseRent),
            HouseRooms >= MinRooms,
            HouseSize >= MinSize,
            (Pets == yes -> HousePets == yes ; true),
            (HouseFloor < FloorLimit -> true ; HouseLift == yes),
            HouseRent >= StartRent,
            HouseRent =< MaxRent
        ),
        MatchingHouses
    ),
    sort(5, @=<, MatchingHouses, SortedHouses),
    (
        SortedHouses = [BestHouse|_] ->
            auction_allocate(Rest, Houses, [BestHouse|TakenHouses], [allocation(Name, BestHouse)|Allocations])
        ;
            auction_allocate(Rest, Houses, TakenHouses, [allocation(Name, none)|Allocations])
    ).

% Εκτύπωση αποτελεσμάτων σε φιλική μορφή

print_allocations([]).
print_allocations([allocation(Name, none)|T]) :-
    format('O πελάτης ~w δεν θα νοικιάσει κάποιο διαμέρισμα!\n', [Name]),
    print_allocations(T).
print_allocations([allocation(Name, house(Address, _, _, _, _, _, _, _, _))|T]) :-
    format('O πελάτης ~w θα νοικιάσει το διαμέρισμα στην διεύθυνση: ~w\n', [Name, Address]),
    print_allocations(T).