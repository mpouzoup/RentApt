/*
  ΟΜΑΔΑ:
  Μπουζούκη Πολυξένη 4535
  Παπαδοπούλου Ουρανία 4499
*/
% Φόρτωση των δεδομένων από τα αρχεία houses.pl και requests.pl
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

% Χειρισμός των επιλογών του χρήστη
handle_choice(0) :- writeln("Έξοδος..."), !.
handle_choice(1) :- interactive_mode, !.
handle_choice(2) :- batch_mode, !.
handle_choice(3) :- auction_mode, !.
handle_choice(_) :- writeln('Επίλεξε έναν αριθμό μεταξύ 0 έως 3!').

% Λειτουργία 1: διαδραστική εισαγωγή προτιμήσεων πελάτη και εύρεση σπιτιών
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
    % Βρίσκουμε όλα τα σπίτια που ικανοποιούν τις προτιμήσεις
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
    % Εμφάνιση κατάλληλων σπιτιών και πρόταση του καλύτερου
    show_suitable_houses(SuitableHouses), 
    recommend_best_house(SuitableHouses),
    nl.

% ΚΡΙΤΗΡΙΑ ΕΠΙΛΟΓΗΣ

% Ελέγχουμε αν ένα σπίτι ταιριάζει με τις προτιμήσεις του πελάτη
matches_preferences(Rooms, Area, Center, Floor, Elevator, PetsAllowed, Garden, Rent,
                    MinRooms, MinArea, DesiredPets, ElevatorFrom,
                    BaseCenterRent, BaseSuburbRent,
                    ExtraPerSqM, GardenPerSqM, MaxTotal) :-

    % Βασικά κριτήρια
    Rooms >= MinRooms,
    Area >= MinArea,
    PetsAllowed == DesiredPets,
    ( Floor < ElevatorFrom ; (Floor >= ElevatorFrom, Elevator == yes) ),

    % Βάση ενοικίου ανάλογα με το αν είναι στο κέντρο ή στα προάστια
    ExtraArea is Area - MinArea,
    ( Center == yes -> BaseRent = BaseCenterRent ; BaseRent = BaseSuburbRent ),
    ExtraAreaCost is ExtraArea * ExtraPerSqM,
    GardenCost is Garden * GardenPerSqM,
    MaxAcceptable is BaseRent + ExtraAreaCost + GardenCost, %Μέγιστο αποδεκτό ενοίκιο με βάση τα κριτήρια και το μέγιστο όριο


    % Το ενοίκιο του σπιτιού πρέπει να είναι μικρότερο και από τα δύο
    Rent =< MaxTotal,
    Rent =< MaxAcceptable.

% ΕΜΦΑΝΙΣΗ ΚΑΤΑΛΛΗΛΩΝ ΣΠΙΤΙΩΝ

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

% ΛΕΙΤΟΥΡΓΙΑ 2: ΜΑΖΙΚΕΣ ΠΡΟΤΙΜΗΣΕΙΣ

% Συγκεντρώνουμε όλες τις αιτήσεις πελατών από το requests.pl
batch_mode :-
    findall(
        request(Name, MinArea, MinRooms, DesiredPets, ElevatorFrom, MaxTotal, BaseCenterRent, BaseSuburbRent, ExtraPerSqM, GardenPerSqM),
        request(Name, MinArea, MinRooms, DesiredPets, ElevatorFrom, MaxTotal, BaseCenterRent, BaseSuburbRent, ExtraPerSqM, GardenPerSqM),
        Requests
    ),
    show_requests(Requests).

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

% ΛΕΙΤΟΥΡΓΙΑ 3: ΔΗΜΟΠΡΑΣΙΑ

% Απλή ιδέα για δημοπρασία: Για κάθε αίτηση βρίσκουμε σπίτια που πληρούν τα κριτήρια
% και βρίσκουμε το σπίτι με τη μικρότερη τιμή ενοικίου που ικανοποιεί τη ζήτηση.
auction_mode :-
    findall(Name,request(Name,_,_,_,_,_,_,_,_,_),Names), % Βρισκουμε ολα τα ονοματα πελατων
    auction_loop(Names, [],FinalAssignments), %Loop με ολους τους πελατες
    print_final_assignments(FinalAssignments,Names).

auction_loop([],Final,Final).
auction_loop(Names,Current,Final) :-
    findall(
        bid(Name,BestHouse,Offer),
        (
            member(Name,Names),
            request(Name,MinSize,MinBedrooms, Pets,ElevatorFloor ,MaxRent,MaxCenter,MaxSuburbs, ExtraPerM2 ,ExtraPerGardenM2),
            Customer= customer(MinSize, MinBedrooms,Pets,ElevatorFloor,MaxRent,MaxCenter,MaxSuburbs,ExtraPerM2,ExtraPerGardenM2),
            find_compatible_houses(Customer,Houses),
            subtract_assigned(Houses,Current,AvailableHouses),
            AvailableHouses \= [],
            find_best_house(AvailableHouses,BestHouse),
            offer(Customer,BestHouse,Offer)
        ),
        Bids
    ),
    % Επιλεγουμε την καλυτερη προσφορα απο ολους τους πελατες για αυτο το loop
    % Χρησιμοποιούμε sort(3, @>=...) ώστε να πάρουμε τη μεγαλύτερη προσφορά τερμα επανω
    ( Bids= [] -> Final = Current ;
        sort(3, @>=, Bids, [bid(WinnerName,WinnerHouse, _)|_]),
        append(Current, [final_assignment(WinnerName, WinnerHouse)],NewCurrent),
        subtract(Names, [WinnerName],NewNames),
        auction_loop(NewNames,NewCurrent,Final)
    ).

subtract_assigned([], _, []).
subtract_assigned([House|T],Assigned,R) :-
    (member(final_assignment(_,House),Assigned) -> subtract_assigned(T,Assigned,R)
    ; R= [House|Rest],subtract_assigned(T,Assigned,Rest)).

print_final_assignments(Assignments,AllNames) :-
    findall(Name,member(final_assignment(Name, _),Assignments), AssignedNames),
    % Αφαιρουμε τους πελατες που δεν εχουν παρει διαμερισμα
    subtract(AllNames,AssignedNames,UnassignedNames),
    print_assignments(Assignments),
    print_unassigned(UnassignedNames).

print_assignments([]).
print_assignments([final_assignment(Name,House)|T]) :-
    House = house(Address,_,_,_,_,_,_,_,_),
    format('O πελάτης ~w θα νοικιάσει το διαμέρισμα στην διεύθυνση: ~w~n',[Name, Address]),
    print_assignments(T).

print_unassigned([]).
print_unassigned([Name|T]) :-
    format('O πελάτης ~w δεν θα νοικιάσει κάποιο διαμέρισμα!~n', [Name]),
    print_unassigned(T).

find_compatible_houses(Customer,Houses) :-  % Βρισκουμε ολα τα διαμερισματα που ειναι compatible με καθε πελατη
    findall(
        house(Address, Bedrooms, Size, Center, Floor, Elevator, PetsAllowed, Garden, Rent),
        (house(Address, Bedrooms, Size, Center, Floor, Elevator, PetsAllowed, Garden, Rent),
         compatible_house(Customer,house(Address, Bedrooms, Size, Center, Floor, Elevator, PetsAllowed, Garden, Rent))),
        Houses).

compatible_house(  % Ελεγχος compatibility
    customer(MinSize, MinBedrooms, Pets, ElevatorFloor, MaxRent, MaxCenter, MaxSuburbs, ExtraPerM2, ExtraPerGardenM2),
    house(_, Bedrooms, Size, Center, Floor, Elevator, PetsAllowed, Garden, Rent)) :-
    Size>=MinSize,
    Bedrooms>=MinBedrooms,
    (Pets ==yes -> PetsAllowed== yes ;true),
    (Floor>=ElevatorFloor -> Elevator== yes ;true),
    (Center==yes -> BaseRent= MaxCenter; BaseRent=MaxSuburbs),
    ExtraM2 is max(0,Size-MinSize),
    ExtraM2Cost is ExtraM2*ExtraPerM2,
    GardenCost is Garden*ExtraPerGardenM2,
    FinalRent is BaseRent+ExtraM2Cost+GardenCost,
    Rent=<FinalRent,
    Rent=<MaxRent.

print_houses([]).
print_houses([house(Address, Bedrooms, Size, Center, Floor, Elevator, PetsAllowed, Garden, Rent)|T]) :-
    format('\nΚατάλληλο σπίτι στην διεύθυνση: ~w~n', [Address]),
    format('Υπνοδωμάτια: ~w~n', [Bedrooms]),
    format('Εμβαδόν: ~w~n', [Size]),
    format('Εμβαδόν κήπου: ~w~n', [Garden]),
    format('Είναι στο κέντρο της πόλης: ~w~n', [Center]),
    format('Επιτρέπονται κατοικίδια: ~w~n', [PetsAllowed]),
    format('Όροφος: ~w~n', [Floor]),
    format('Ανελκυστήρας: ~w~n', [Elevator]),
    format('Ενοίκιο: ~w~n', [Rent]),
    print_houses(T).


find_best_house([House],House).
find_best_house([H|T],Best) :-
    find_best_house(T,BestRest),
    better_house(H,BestRest,Best).

% Συγκριση σπιτιων με βαση το ενοικιο, το μεγεθος του κηπου και το συνολικο μεγεθος
better_house(H1,H2,H1) :- cheaper(H1,H2), !.
better_house(H1,H2,H1) :- equal_price(H1,H2), bigger_garden(H1,H2), !.
better_house(H1,H2,H1) :- equal_price(H1,H2), equal_garden(H1,H2), bigger_size(H1,H2), !.
better_house(_,H2,H2).

cheaper(house(_,_,_,_,_,_,_,_,Rent1),house(_,_,_,_,_,_,_,_,Rent2)) :- Rent1<Rent2.
equal_price(house(_,_,_,_,_,_,_,_,Rent), house(_,_,_,_,_,_,_,_, Rent)).
bigger_garden(house(_,_,_,_,_,_,_,Garden1,_),house(_,_,_,_,_,_,_,Garden2,_)) :- Garden1>Garden2.
equal_garden(house(_,_,_,_,_,_,_,Garden,_), house(_,_,_,_,_,_,_,Garden,_)).
bigger_size(house(_,_,Size1,_,_,_,_,_,_), house(_,_,Size2,_,_,_,_,_,_)) :- Size1>Size2.

% Υπολογισμος προσφορας με βασει τις απαιτησεις του πελατη και των χαρακτηριστικων του σπιτιου
offer(customer(MinSize,_,_,_,_,MaxCenter,MaxSuburbs,ExtraPerM2,ExtraPerGardenM2),
      house(_,_,Size,Center,_,_,_,Garden,_),Offer) :-
    (Center== yes -> Base=MaxCenter;Base=MaxSuburbs),
    ExtraM2 is max(0,Size-MinSize),
    Offer is Base + ExtraM2 * ExtraPerM2 + Garden * ExtraPerGardenM2.
