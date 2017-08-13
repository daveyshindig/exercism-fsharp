module Allergies

type Allergen = Eggs | Peanuts | Shellfish | Strawberries | Tomatoes
               | Chocolate | Pollen | Cats

let allergicTo allergen x = 
    match allergen with
    | Eggs -> x &&& 1 <> 0
    | Peanuts -> x &&& 2 <> 0
    | Shellfish -> x &&& 4 <> 0
    | Strawberries -> x &&& 8 <> 0
    | Tomatoes -> x &&& 16 <> 0
    | Chocolate -> x &&& 32 <> 0
    | Pollen -> x &&& 64 <> 0
    | Cats -> x &&& 128 <> 0

let allergies x =
    let allAllergies = [Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; 
                        Chocolate; Pollen; Cats]
    let alTo allergen = allergicTo allergen x
    List.filter alTo allAllergies
    