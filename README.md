# Apartment Selection System in Prolog

**University Group Project**  
Developed by: Polyxeni Bouzouki (4535) & Ourania Papadopoulou (4499)  

This project is an **apartment selection system** implemented in **Prolog**, designed as a university group project. The system allows users to find suitable apartments based on their personal preferences and constraints.

---

## Features

The system supports three main functionalities:

1. **Interactive Mode**  
   - Allows a single client to input preferences, including:
     - Minimum area
     - Minimum number of bedrooms
     - Pets allowed
     - Floor requirements and elevator availability
     - Maximum rent
     - Base rent for center/suburbs
     - Extra costs for additional square meters and garden area  
   - Finds and displays suitable apartments.
   - Recommends the best apartment based on rent, garden size, and total area.

2. **Batch Mode**  
   - Processes multiple clients from a `requests.pl` file.
   - Finds suitable apartments for each client.
   - Recommends the best apartment for every client.

3. **Auction Mode**  
   - Assigns apartments to multiple clients using a bidding system.
   - Ensures that each apartment is allocated fairly based on client offers.

---

## Data Files

- `houses.pl` – Contains all the apartment data.  
- `requests.pl` – Contains client requests for batch and auction modes.

Each apartment is represented as:  
house(Address, Bedrooms, Area, Center, Floor, Elevator, PetsAllowed, Garden, Rent).

Each client request is represented as:
request(Name, MinArea, MinRooms, DesiredPets, ElevatorFrom, MaxTotal, BaseCenterRent, BaseSuburbRent, ExtraPerSqM, GardenPerSqM).

## Usage

Terminal:

    -swipl

    -[main].
    
    -run.

Follow the interactive menu.

## How it Works

Suitability Criteria: Apartments are filtered based on client preferences and maximum acceptable rent.

Best Apartment Recommendation: The system considers the lowest rent, largest garden, and largest total area.

Auction Mode: Apartments are assigned to clients based on the highest offer that meets their preferences.