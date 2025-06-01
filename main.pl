:- consult('houses.pl').
:- consult('requests.pl').

run :-
    repeat,
    writeln("Μενού:\n======"),
    writeln("1 - Προτιμήσεις ενός πελάτη"),
    writeln("2 - Μαζικές προτιμήσεις πελατών"),
    writeln("3 - Επιλογή πελατών μέσω δημοπρασίας"),
    writeln("0 - Έξοδος"),
    write("Επιλογή: "),
    read(Choice),
    handle_choice(Choice),
    Choice == 0, !.

handle_choice(0) :- writeln("Έξοδος..."), !.
handle_choice(1) :- interactive_mode, !.
handle_choice(2) :- batch_mode, !.
handle_choice(3) :- auction_mode, !.

interactive_mode :-
    writeln("Δώσε τις παρακάτω πληροφορίες:\n=============================="),
    write("Ελάχιστο Εμβαδόν: "), read(MinArea),
    write("Ελάχιστος αριθμός υπνοδωματίων: "), read(MinRooms),
    write("Να επιτρέπονται κατοικίδια; (yes/no) "), read(DesiredPets),
    write("Από ποιον όροφο και πάνω να υπάρχει ανελκυστήρας; "), read(ElevatorFrom),
    write("Ποιο είναι το μέγιστο ενοίκιο που μπορείς να πληρώσεις; "), read(MaxTotal),
    write("Πόσα θα έδινες για ένα διαμέρισμα στο κέντρο της πόλης (στα ελάχιστα τετραγωνικά); "), read(BaseCenterRent),
    write("Πόσα θα έδινες για ένα διαμέρισμα στα προάστια της πόλης (στα ελάχιστα τετραγωνικά); "), read(BaseSuburbRent),
    write("Πόσα θα έδινες για κάθε τετραγωνικό διαμερίσματος πάνω από το ελάχιστο; "), read(ExtraPerSqM),
    write("Πόσα θα έδινες για κάθε τετραγωνικό κήπου; "), read(GardenPerSqM),
    
    nl,
    findall(
        house(Address, Rooms, Area, Center, Floor, Elevator, PetsAllowed, Garden, Rent),
        ( house(Address, Rooms, Area, Center, Floor, Elevator, PetsAllowed, Garden, Rent),
          matches_preferences(
              Rooms, Area, Center, Floor, Elevator, PetsAllowed, Garden, Rent,
              MinRooms, MinArea, DesiredPets, ElevatorFrom,
              BaseCenterRent, BaseSuburbRent,
              ExtraPerSqM, GardenPerSqM, MaxTotal
          )
        ),
        SuitableHouses
    ),
    show_suitable_houses(SuitableHouses), 
    recommend_best_house(SuitableHouses),
    nl.

% -------------------- ΚΡΙΤΗΡΙΑ --------------------

matches_preferences(Rooms, Area, Center, Floor, Elevator, PetsAllowed, Garden, Rent,
                    MinRooms, MinArea, DesiredPets, ElevatorFrom,
                    BaseCenterRent, BaseSuburbRent,
                    ExtraPerSqM, GardenPerSqM, MaxTotal) :-

    % Βασικά κριτήρια
    Rooms >= MinRooms,
    Area >= MinArea,
    PetsAllowed == DesiredPets,
    ( Floor < ElevatorFrom ; (Floor >= ElevatorFrom, Elevator == yes) ),

    % Υπολογισμός αποδεκτής τιμής
    ExtraArea is Area - MinArea,
    ( Center == yes -> BaseRent = BaseCenterRent ; BaseRent = BaseSuburbRent ),
    ExtraAreaCost is ExtraArea * ExtraPerSqM,
    GardenCost is Garden * GardenPerSqM,
    MaxAcceptable is BaseRent + ExtraAreaCost + GardenCost,

    % Πρέπει να πληροί και το άνω όριο και να καλύπτει το ενοίκιο του σπιτιού
    Rent =< MaxTotal,
    Rent =< MaxAcceptable.

% -------------------- ΕΜΦΑΝΙΣΗ --------------------
show_suitable_houses(Houses) :-
    ( Houses == [] ->
        writeln("Δεν υπάρχει κατάλληλο σπίτι!")
    ;
        display_suitable_houses(Houses)
    ).

display_suitable_houses([]).

display_suitable_houses([house(Address, Rooms, Area, Center, Floor, Elevator, Pets, Garden, Rent)|Rest]) :-
    format("Κατάλληλο σπίτι στην διεύθυνση: ~w\n", [Address]),
    format("Υπνοδωμάτια: ~d\n", [Rooms]),
    format("Εμβαδόν: ~d\n", [Area]),
    format("Εμβαδόν κήπου: ~d\n", [Garden]),
    format("Είναι στο κέντρο της πόλης: ~w\n", [Center]),
    format("Επιτρέπονται κατοικίδια: ~w\n", [Pets]),
    format("Όροφος: ~d\n", [Floor]),
    format("Ανελκυστήρας: ~w\n", [Elevator]),
    format("Ενοίκιο: ~d\n\n", [Rent]),
    display_suitable_houses(Rest).

recommend_best_house(Houses) :-
    % Ελάχιστο ενοίκιο από όλα τα σπίτια
    findall(Rent, member(house(_, _, _, _, _, _, _, _, Rent), Houses), Rents),
    min_list(Rents, MinRent),

    % Φιλτράρουμε όσα έχουν το ελάχιστο ενοίκιο
    include(
        [house(_, _, _, _, _, _, _, _, Rent)]>>(Rent =:= MinRent),
        Houses, CheapestHouses
    ),

    % Ανάμεσα στα φθηνότερα, βρίσκουμε όσα έχουν τον μεγαλύτερο κήπο
    findall(Garden, member(house(_, _, _, _, _, _, _, Garden, _), CheapestHouses), Gardens),
    max_list(Gardens, MaxGarden),
    include(
        [house(_, _, _, _, _, _, _, Garden, _)]>>(Garden =:= MaxGarden),
        CheapestHouses, BestGardenHouses
    ),

    % Ανάμεσα σε αυτά, βρίσκουμε όσα έχουν το μεγαλύτερο εμβαδόν
    findall(Area, member(house(_, _, Area, _, _, _, _, _, _), BestGardenHouses), Areas),
    max_list(Areas, MaxArea),
    include(
        [house(_, _, Area, _, _, _, _, _, _)]>>(Area =:= MaxArea),
        BestGardenHouses, FinalChoices
    ),

    % Παίρνουμε το πρώτο της λίστας ως καλύτερη πρόταση
    FinalChoices = [house(Address, _, _, _, _, _, _, _, _)|_],
    format("Προτείνεται η ενοικίαση του διαμερίσματος στην διεύθυνση: ~w\n", [Address]).

batch_mode :-
    findall(
        request(Name, MinArea, MinRooms, DesiredPets, ElevatorFrom, MaxTotal, BaseCenterRent, BaseSuburbRent, ExtraPerSqM, GardenPerSqM),
        request(Name, MinArea, MinRooms, DesiredPets, ElevatorFrom, MaxTotal, BaseCenterRent, BaseSuburbRent, ExtraPerSqM, GardenPerSqM),
        Requests
    ),
    show_requests(Requests),
    format("look", Requests).

show_requests(Requests) :-
    ( Requests = [] ->
        writeln("Nothing")
    ;
        process_requests(Requests)
    ).

process_requests([]).
process_requests([request(Name, MinArea, MinRooms, DesiredPets, ElevatorFrom, MaxTotal, BaseCenterRent, BaseSuburbRent, ExtraPerSqM, GardenPerSqM)|Rest]) :-
    format("\nΚατάλληλα διαμερίσματα για τον πελάτη: ~w\n==============================\n", [Name]),
    
    findall(
        house(Address, Rooms, Area, Center, Floor, Elevator, PetsAllowed, Garden, Rent),
        ( house(Address, Rooms, Area, Center, Floor, Elevator, PetsAllowed, Garden, Rent),
          matches_preferences(
              Rooms, Area, Center, Floor, Elevator, PetsAllowed, Garden, Rent,
              MinRooms, MinArea, DesiredPets, ElevatorFrom,
              BaseCenterRent, BaseSuburbRent,
              ExtraPerSqM, GardenPerSqM, MaxTotal
          )
        ),
        SuitableHouses
    ),
    show_suitable_houses(SuitableHouses),
    recommend_best_house(SuitableHouses),
    process_requests(Rest).

auction_mode :-
    
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

auction_allocate([], _, _, []).

auction_allocate([request(Name, MinSize, MinRooms, Pets, FloorLimit, MaxRent, StartRent, _, _, _) | Rest], Houses, TakenHouses, Allocations) :-
    exclude({TakenHouses}/[H]>>member(H, TakenHouses), Houses, RemainingHouses),
    findall(House,
        (
            member(House, RemainingHouses),
            House = house(_, HouseRooms, HouseSize, HousePets, HouseFloor, HouseLift, _, _, HouseRent),
            HouseRooms >= MinRooms,
            HouseSize >= MinSize,
            (Pets == yes -> HousePets == yes ; true),
            ((HouseFloor =< FloorLimit) ; (HouseLift == yes)),
            HouseRent =< MaxRent
            
        ),
        MatchingHouses
    ),
    sort(9, @=<, MatchingHouses, SortedHouses),
    (
        SortedHouses = [BestHouse|_] ->
            auction_allocate(Rest, Houses, [BestHouse|TakenHouses], RestAllocations),
            Allocations = [allocation(Name, BestHouse) | RestAllocations]
        ;
            auction_allocate(Rest, Houses, TakenHouses, RestAllocations),
            Allocations = [allocation(Name, none) | RestAllocations]
    ).

% Εκτύπωση αποτελεσμάτων σε φιλική μορφή

print_allocations([]).
print_allocations([allocation(Name, none)|T]) :-
    format('O πελάτης ~w δεν θα νοικιάσει κάποιο διαμέρισμα!\n', [Name]),
    print_allocations(T).
print_allocations([allocation(Name, house(Address, _, _, _, _, _, _, _, _))|T]) :-
    format('O πελάτης ~w θα νοικιάσει το διαμέρισμα στην διεύθυνση: ~w\n', [Name, Address]),
    print_allocations(T).