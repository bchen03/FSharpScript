// Basic function
let add1 a = a + 1
let six = add1 5

// Parameterless function
let printHelloWorld () = printfn "Hello World"
printHelloWorld ()


// Construct full name from first and last
let fullName first last = first + " " + last

// Add exclamation point to name
let exclamation name = name + "!"

// animals list
let animals = ["Zebra"; "Fly"; "Giraffe"; "Horse"; "Alligator"; "Pig"]

// printfn prints with newline 
printfn "%s" (fullName "benny" "chen")
printfn "%s" (exclamation "benny")

// map animals -> fullname, sort it, and then print
animals |> Seq.map (fullName  "Hello") |> Seq.sort |> Seq.iter (fun a -> printfn "%s" a)

// Some tuples
let tuple1 = (1,2)
let tuple2 = (2,3)
let tuple3 = (1,2,3)
let tuple4 = 1,2,3,4

// print tuple elements
printfn "%d %d" <|| tuple1

// tuple deconstruction
let (x,y) = tuple1
printfn "%d %d" x y

// underscore is "don't care" symbol
let (z,_) = tuple1
printfn "%d" z

// pattern matching
let tupleOfOneAndTwo tuple = 
    match tuple with 
    | (1,2) -> printfn  "Tuple Matches (1,2)" 
    | _ -> printfn  "Tuple doesn't match (1,2)" 

tupleOfOneAndTwo tuple1
tupleOfOneAndTwo tuple2

let validValue = Some(99)
let invalidValue = None

let optionPatternMatch input =
   match input with
    | Some i -> printfn "input is an int=%d" i
    | None -> printfn "input is missing"

optionPatternMatch validValue
optionPatternMatch invalidValue

// Pattern matching w/ exception handling
let tryParse intStr =
    try
        let i = System.Int32.Parse intStr
        (true, i)
    with _ -> (false, 0)    
//    | _ -> (false, 0)    

tryParse "1"
tryParse "a"

// Exception handling
let exceptionHandling () =
    let divide2 x y =
        try
            let result = Some(x / y)
            printfn "Result: %d" result.Value
            result
        with
        | :? System.ArgumentException as ex -> printfn "Arg Exception : %s" (ex.Message); None
        | :? System.DivideByZeroException as ex -> printfn "DBZ Exception : %s" (ex.Message); None
        //| :? System.Exception as ex  -> printfn "Exception: %s" (ex.Message); None

    divide2 5 2 |> ignore
    divide2 5 0 

exceptionHandling ()


// Type
type Person =  
    {
        FirstName: string;
        LastName: string;
    }

let person1 = { FirstName = "Benny"; LastName = "Chen"}

printfn "Person1: %O" person1

// Discriminated union

type IntOrBool = Int of int | Bool of bool

let i = Int 99
let b = Bool true 

let resultOfIntOrBool iorb = 
    match iorb with
    | Int i -> printfn "resultOfIntOrBool:Int: %i" i
    | Bool b -> printfn "resultOfIntOrBool:Bool: %b" b

resultOfIntOrBool i
resultOfIntOrBool b


// More pattern matching
type Shape = 
    | Circle of radius: int
    | Rectangle of height:int * weight:int
    | Square of length: int

let draw shape =
    match shape with
    | Circle radius -> printfn "Circle has the radius of %i" radius
    | Rectangle (height,width) -> printfn "Rectangle has height %i and width %i"  height width
    | Square length ->  printfn "Square has the length of %i" length
//    | _ -> printfn "Unknown shape!"
 
let circle = Circle(10)
let rectangle = Rectangle(5, 4)
let square = Square(44)

[circle; rectangle; square] |> List.iter draw


// House type
type Knob = Plain | Decorative | Deluxe
type Door = { Color:string; Knob:Knob; }
type Window = Single | DoublePane | Frosted
type RoomType = Kitchen | Living | Bed | Bath | Recreation
type Room = { SqFoot:int; Doors: Door list; Windows: Window list; RoomType: RoomType; }
type House = { LivingRoom: Room; BedRoom: Room list; BathRoom: Room list; Kitchen: Room; RecRoom: Room;  }

// 
type C = Circle of int | Rectangle of int * int
[1..10] |> List.map Circle  // [Circle 1; Circle 2; ...]
[1..10] |> List.zip [21..30] |> List.map Rectangle  // [Rectangle(21,1); Rectangle{22,2); ...]

//
module A =
    type C = Circle of int | Rectangle of int * int
    [1..10] |> List.map Circle  // [Circle 1; Circle 2; ...]


// Currying
let add x y = x + y

let addFive = add 5

printfn "%i" (addFive 3)


// Convert space-separated string to int array and add ints together
let sum = 
    "1 2 3 4 5 6 7".Split ' '
    |> Array.map(System.Int32.Parse)
    |> Array.reduce(fun acc item -> acc + item)

let sum2 = 
    "1 2 3 4 5 6 7".Split ' '
    |> Array.map(System.Int32.Parse) 
    |> Array.toList
    |> List.reduce(fun acc item -> acc + item)


// Fibonacci

let fibSeq = Seq.unfold (fun (a,b) -> Some( a+b, (b, a+b) ) ) (0,1)
let fibList = fibSeq |> Seq.takeWhile (fun x -> x<=100 ) |> Seq.toList

Some(1, 2, 3)

// Fold
let fold = List.fold(fun state item -> state + item) 0 [1..10]
let fold2 = List.foldBack(fun state item -> state + item) [1..10] 0

// Reduce
let reduce = ["Bob"; "Bill"; "Baz"] |> List.reduce(fun state item -> state + "," + item)

// Unfold
let unfold = Seq.unfold(fun state -> Some(state, state * 2)) 1
let under50 = 
    unfold 
    |> Seq.takeWhile(fun i -> i < 50)
    |> Seq.map(fun i -> i.ToString())
    |> Seq.reduce(fun state item -> state + "," + item)

// Deconstruction    

let defunc' = (1, 2)

let (x, y) = defunc'

// More union

type UnionOf3 = 
    | Type1 of string * string * int
    | None

let type3 = Type1( "One", "Two", 3 )
let type4 = None

//

type EmailAddress = EmailAddress of string

let a = "a" |> EmailAddress

printfn "Email Address: %O" a

["a";"b";"c"] |> List.map EmailAddress


// HackerRank array test
// https://www.hackerrank.com/challenges/arrays-ds/problem

// Print all integers in  reverse order as a single line of space-separated integers.

// Sample Input
// 4
// 1 4 3 2

// Sample Output
// 2 3 4 1

System.Console.ReadLine()

System.Console.ReadLine().Split() 
|> Seq.map int 
|> Seq.rev  //reverse
|> Seq.iter(fun x -> printf "%d " x)

// Left rotation
// https://www.hackerrank.com/challenges/array-left-rotation/problem

// Print a single line of space-separated integers denoting the final state of the array after performing  left rotations.

// Sample Input
// 5 4
// 1 2 3 4 5

//Sample Output
// 5 1 2 3 4

module Rotations1 =
    let firstLine = 
        System.Console.ReadLine().Split() 
        |> Seq.map int


    let length = 
        firstLine
        |> Seq.item 0
    let rotations = 
        firstLine
        |> Seq.item 1

    let values = 
        System.Console.ReadLine().Split() 
        |> Seq.map int 


    values
    |> Seq.skip rotations
    |> Seq.iter(fun x -> printf "%d " x)

    values 
    |> Seq.take (rotations)
    |> Seq.iter(fun x -> printf "%d " x)

// Slightly condensed version
module Rotations2 =
    let leftRotate rotations sequence = 
        sequence
        |> Seq.skip rotations
        |> Seq.iter(fun x -> printf "%d " x)
        sequence
        |> Seq.take rotations
        |> Seq.iter(fun x -> printf "%d " x)
       
    let rotations = 
        System.Console.ReadLine().Split() 
        |> Seq.map int
        |> Seq.item 1


    System.Console.ReadLine().Split() 
    |> Seq.map int 
    |> leftRotate rotations


// Read n lines from console
let getArrayFromConsole _ = 
    let
     readLines n = Array.init n (fun _ -> System.Console.ReadLine ())
    System.Console.ReadLine () |> int |> readLines |> Array.map string


//let stack = new System.Collections.Generic.Stack<int>() |> 

getArrayFromConsole() 

//let discriminator = item.Split() |> Seq.map int |> Seq.item 0
//let num = item.Split() |> Seq.map int |> Seq.item 1
  
let getArrayOfStringFromConsole () = 
    let readLines n = Array.init n (fun _ -> System.Console.ReadLine ())
    System.Console.ReadLine () 
        |> int 
        |> readLines 
        |> Array.map string

getArrayOfStringFromConsole() 
    |> Array.iter(fun x -> printf "%s " x)


// https://www.hackerrank.com/challenges/simple-array-sum/problem
// 6
// 1 2 3 4 10 11

open System

let size = Console.ReadLine()
// Console.ReadLine() |> ignore


let sum = 

    Console.ReadLine().Split() 
    |> Array.map(System.Int32.Parse)
    |> Array.reduce(fun acc item -> acc + item)

printfn "%d" sum

// Implicit currying - 
// All multi parameter functions turned into single parameter curried functions
let implicitCurry x y = x + y
let curry = implicitCurry 2
let curry = curry 3

printfn "%d" curry

// Explicit currying
let explicitCurry x = 
    let subFunc y = 
        x + y
    subFunc

let it = explicitCurry 3
let it = it 4

printfn "%d" it

// (+) function - 4 + 5 is converted to (+) 4 5 curried function
printfn "%d" (4 + 5)
printfn "%d" ((+) 4 5)

// Composition
let add1 x = x + 1
let times2 x = x * 2

let add1times2 x = (>>) add1 times2 x

add1times2 5

let add1times2nox = (>>) add1 times2 

add1times2nox 6

let add1times2noxp = add1 >> times2 

add1times2noxp 7

// Parameter pattern matching
type Name =  { first: string; last: string }
let bob = { first = "bob"; last = "smith" }

let f1 name = 
    let  {first=f; last=l} = name
    printfn "first=%s; last=%s" f l

f1 bob

// Compare the triplet
// https://www.hackerrank.com/challenges/compare-the-triplets/problem
//
// Input Format
//
// The first line contains 3 space-separated integers, a0, a1, and a2, describing the respective values in triplet A. 
// The second line contains 3 space-separated integers, b0, b1, and b2, describing the respective values in triplet B.
//
// Output Format
//
// Print two space-separated integers denoting the respective comparison points earned by Alice and Bob.
//
// Sample Input
//
// 5 6 7
// 3 6 10
//
// Sample Output
//
// 1 1 

open System

//let alice = Console.ReadLine().Split() |> Array.map(Int32.Parse) 
//let bob = Console.ReadLine().Split() |> Array.map(Int32.Parse) 
let countLeftTriplet arr1 arr2 = 
    Array.fold2 (fun acc item1 item2 -> if item1 > item2 then acc + 1 else acc) 0 arr1 arr2

let alice = "5 6 7".Split() |> Array.map(System.Int32.Parse) 
let bob = "3 6 10".Split() |> Array.map(System.Int32.Parse)

//let alice = Console.ReadLine().Split() |> Array.map(Int32.Parse) 
//let bob = Console.ReadLine().Split() |> Array.map(Int32.Parse) 


printfn "%d %d" (countLeftTriplet alice bob) (countLeftTriplet bob alice)


// Read console input lines until empty string entered
let input () = 
    fun
     _ -> System.Console.ReadLine()
    |>  Seq.initInfinite // (1)
    |>  Seq.takeWhile ((<>) "") 
    // (2) (3)
    |>  Seq.map System.Int32.Parse

input () |> printfn "%A"


// Grading students
// https://www.hackerrank.com/challenges/grading/problem

// HackerLand University has the following grading policy:

// Every student receives a grade in the inclusive range from 0 to 100.
// Any  grade less than 40 is a failing grade.
// Sam is a professor at the university and likes to round each student's grade according to these rules:

// If the difference between the grade and the next multiple of 5 is less than 3, round grade up to the next multiple of 5.
// If the value of grade is less than 38, no rounding occurs as the result will still be a failing grade.
// For example, grade = 84 will be rounded to 85 but grade = 29 will not be rounded because the rounding would result in a number that is less than 40.

// Given the initial value of grade for each of Sam's n students, write code to automate the rounding process. For each , round it according to the rules above and print the result on a new line.

// Input Format

// The first line contains a single integer denoting  (the number of students). 
// Each line i of the n subsequent lines contains a single integer, grade i, denoting student i's grade.

// For each  of the  grades, print the rounded grade on a new line.

// Sample Input 0

// 4
// 73
// 67
// 38
// 33
// Sample Output 0

// 75
// 67
// 40
// 33

// Read n # ints from console
let getArrayOfIntFromConsole () = 
    let readLines n = Array.init n (fun _ -> System.Console.ReadLine ())
    System.Console.ReadLine () |> int |> readLines |> Array.map int


//getArrayOfIntFromConsole () |> printfn "%A"
let finalGrade grade = 
    if grade > 37 && (grade % 5) > 2 then
        (grade / 5) * 5 + 5
    else
        grade

let result = 
    getArrayOfIntFromConsole ()
    |> Array.map (fun grade -> finalGrade grade)
    |> Array.iter (fun grade -> printfn "%d" grade)


// print each char from input string
System.Console.ReadLine() 
|> Seq.map (fun c -> c)
|> Seq.iter (fun x -> printfn "%c" x)

// Remove side-by-side dups
// Below doesn't works beyond initial dup but shows syntax for Array.fold
let str = System.Console.ReadLine().ToCharArray() |>  Array.map(string)
str |> Array.fold(fun acc item -> if acc = item then "" else acc + item) "" |> printfn "%s" 


[|"a";"a";"b";"c"|] 
|> Array.fold(fun acc item -> if acc = item then "" else acc + item) "" 
|> printfn "%s" 

"aaabccddd".ToCharArray() 
|> Array.map(string) 
|> Array.fold(fun acc item -> if acc = item then "" else acc + item) "" 
|> printfn "%s" 

// F# way to return substring
let ff = "abc".[2..]


// This correctly removes side-by-side dups

// If pattern is in str return true else false
let matchPattern pattern str = 
    System.Text.RegularExpressions.Regex.IsMatch(str, pattern)

// Make delimited string from string array
let makeDelimitedString (separator:string) (array:string[]) = 
    let result = System.String.Join(separator, array)
    result

// Same as matchPattern but takes pattern array
let matchPatternArray array str = 
    let pattern = makeDelimitedString "|" array
    System.Text.RegularExpressions.Regex.IsMatch(str, pattern)

// Remove substring from string    
let removeSubstring substring (str:string) = str.Replace(substring, "")

// Remove array substrings from string
let removeArraySubstrings patterns str =
    Array.fold (fun (acc:string) item -> acc.Replace(item, "")) str patterns
//    Array.fold (fun acc item -> removeSubstring item acc) str patterns

let rec superReduce str:string = 
    // Reduction array
    let patterns =
        [|"aa";"bb";"cc";"dd";"ee";"ff";"gg";"hh";"ii";"jj";"kk";"ll";"mm";
          "nn";"oo";"pp";"qq";"rr";"ss";"tt";"uu";"vv";"ww";"xx";"yy";"zz";|]

    // Nest functions

    // Make delimited string from string array
    let makeDelimitedString (separator:string) (array:string[]) = 
        let result = System.String.Join(separator, array)
        result

    // Same as matchPattern but takes pattern array
    let matchPatternArray array str = 
        let pattern = makeDelimitedString "|" array
        System.Text.RegularExpressions.Regex.IsMatch(str, pattern)

    // Remove substring from string    
    let removeSubstring substring (str:string) = str.Replace(substring, "")

    // Remove array substrings from string
    let removeArraySubstrings patterns str =
        Array.fold (fun (acc:string) item -> acc.Replace(item, "")) str patterns

    // Main
    if str |> matchPatternArray patterns then
        // #1 
        //let result = Array.fold (fun acc item -> removeSubstring item acc) str patterns
        //superReduce result        
        // #2 - removeArraySubstrings hides Array.fold and changes parameter order for piping
        //let result = str |> removeArraySubstrings patterns
        //superReduce result        
        // #3 - Use pipes
        str |> removeArraySubstrings patterns |> superReduce
    else
        str

let printStringResult str = printfn "%s" (if String.length str > 0 then str else "Empty String")

System.Console.ReadLine() |> superReduce |> printStringResult

// Test helper func
matchPattern "aa" "bbaacc"
makeDelimitedString "|" [|"aa";"bb";"cc";"dd"|]
matchPatternArray [|"aa";"bb";"cc";"dd"|] "abccddd"
// Array.fold to remove all array substrings from string
Array.fold (fun (acc:string) item -> acc.Replace(item, "")) "abccddd" [|"aa";"bb";"cc";"dd"|]
Array.fold (fun acc item -> removeSubstring item acc) "abccddd" [|"aa";"bb";"cc";"dd"|]
Array.fold (fun acc item -> removeSubstring item acc) "abaaba" [|"aa";"bb";"cc";"dd"|]
// Test matchPattern
"abccddd" |> matchPattern "aa|bb|cc" 
// Test removeString
"abccddd" |> removeSubstring "aa" |> removeSubstring "bb" |> removeSubstring "cc"
// Test superReduce
superReduce "abccddd" 
superReduce "baab"
superReduce "cabdaadbac"


// fold uses array items in normal order [|1;2;3|]; 
// foldback uses array items in reverse order [|3;2;1|]; 
let subtractArray array1 = 
    Array.fold (
        fun acc elem -> 
            printfn "%d %d" acc elem 
            acc - elem) 0 array1
printfn "%d" (subtractArray [| 1; 2; 3 |])

let subtractArrayBack array1 = 
    Array.foldBack (
        fun elem acc -> 
            printfn "%d %d" acc elem 
            acc - elem) array1 0
printfn "%d" (subtractArrayBack [| 1; 2; 3 |])


// Big Sorting
// https://www.hackerrank.com/challenges/big-sorting/problem
let getArrayFromConsole () = 
    let readLines n = Array.init n (fun _ -> System.Console.ReadLine ())
    System.Console.ReadLine () |> int |> readLines |> Array.map string

//getArrayFromConsole ()
[|"6";"31415926535897932384626433832795";"1";"3";"10";"3";"5";|]
|> Array.map (System.Numerics.BigInteger.Parse)
|> Array.sort 
|> Array.iter (fun item -> printfn "%A" item)

// Active pattern
let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

let (|Bool|_|) str =
   match System.Boolean.TryParse(str) with
   | (true,bool) -> Some(bool)
   | _ -> None
   
let testParse str = 
    match str with
    | Int i -> printfn "The value is an int '%i'" i
    | Bool b -> printfn "The value is a bool '%b'" b
    | _ -> printfn "The value '%s' is something else" str

testParse "12"
testParse "true"
testParse "abc"    

// Tuples
let compare (a,b) = System.String.Compare(a,b)
compare ("b","a")   // Must have parentheses or compiler thinks its a string instead of string*string
let y = "b","a"     // But this is ok
compare y

// 
let add x y = x + y   // explicit
let addp x = (+) x     // point free - compiler knows (+) takes 2 parameters so this returns function with unnamed second parameter

let add2 = add 2
add2 3

let addp2 = addp 2
addp2 3

(+) 2   // returns partial function
2 + 3   // infix - remember, + is a function 
(+) 2 3 // prefix use - same as 2 + 3 but allows for creating partial function

let add1Times2 x = (x + 1) * 2    // explicit
let add1Times2 = (+) 1 >> (*) 2   // point free

// Implementation of standard F# operators
let (|>) x f = f x             // forward pipe
let (<|) f x = f x             // reverse pipe
let (>>) f g x = g (f x)       // forward composition
let (<<) g f x = g (f x)       // reverse composition

// pipe
let add1 a =  a + 1

3 |> add1 
add1 <| 3

// composition
let times2 a =  a * 2

(add1 >> times2) 4
(add1 << times2) 4

let add1times2 = add1 >> times2
let times2add1 = add1 << times2

add1times2 4
times2add1 4

// Sum values using recursion and match expression w/o using fold/reduce
let sumNumbersUpTo max = 

    // recursive helper function with accumulator
    let rec recursiveSum n sumSoFar = 
        match n with
        | 0 -> sumSoFar
        | _ -> recursiveSum (n-1) (n+sumSoFar)

    // call helper function with initial values
    recursiveSum max 0
            
// test
sumNumbersUpTo 10

// Lazy
let lazy1 = lazy(2 + 3)
printfn "%d" (lazy1.Force())

// Calling .NET methods
"acc".Replace ("c","")

// Match expressions
let addit ex = 
    match ex with
    | 1 -> Some(ex)
    | 2 -> Some(ex * 2)
    | 3 | 4 | 5 | 6 -> Some(ex - 1)
    | var -> Some(var)   
    //| _ -> None

let printOption opt = 
    match opt with
    | Some v -> printfn "%d" v
    | None -> printfn "Out of range"

addit 7 |> printOption

type GolfScore =
    | Albatross
    | Eagle
    | Birdie
    | Par
    | Bogey
    | DoubleBogey

let printGolfScore score =
    match score with 
    | GolfScore.Albatross ->  printfn "Albatross"
    | GolfScore.Eagle ->  printfn "Eagle"
    | GolfScore.Birdie ->  printfn "Birdie"
    | GolfScore.Par ->  printfn "Par"
    | GolfScore.Bogey ->  printfn "Bogey"
    | GolfScore.DoubleBogey ->  printfn "DoubleBogey"

printGolfScore GolfScore.Albatross


// List
let l = [1;2;3] @ [4;5;6]   // concatenate
51 :: 50 :: l               // Prepend int to list

// Seq
seq { 0..10..100 }          // sequence in multiples of 10
seq { for i in 1..10 do yield i }   // This and below expression are equivalent 
seq { for i in 1..10 -> i }         

// for..to, for..downto
for x = 1 to 10 do
    printfn "%d" x
    
for x = 10 downto 1 do
    printfn "%d" x

//
let testAssert x y =
    assert(x < y)
    y - x

testAssert 5 6
testAssert 6 5

//
let y1 = seq { for i = 0 to 100 do yield [ i ] }
let y2 = seq { for i = 0 to 100 do yield! [ i ] }
y1
y2

// https://www.hackerrank.com/challenges/gridland-metro/problem
// Input:
// 3 3 2
// 1 1 3
// 2 1 2
// Result:
// 9 total cells - 5 track cells = 4 empty cells

let metroProblem () =
    let firstLine = System.Console.ReadLine().Split() |> Array.map int 
    let gridSize = firstLine |> Array.take 2 |> Array.reduce(fun a b -> a * b)
    let gridCols = firstLine.[1]

    let getRowsFromConsole () = 
        let readLines n = Array.init n (fun _ -> System.Console.ReadLine ())
        firstLine |> Array.item 2 |> readLines

    let buildTracks length (grid:int array) (track:string) = 
        let intTracks = track.Split () |> Array.map int
        let row = intTracks.[0] - 1
        let startCol = intTracks.[1] - 1
        let endCol = intTracks.[2] - 1
        let startPos = (row * length) + startCol
        let endPos = startPos + endCol - startCol
        let front = if startPos > 0 then grid.[..(startPos-1)] else [||]
        let mid = Array.create (endPos - startPos + 1) 1
        let back = grid.[(endPos + 1)..] 
        Array.append (Array.append front mid) back

    // main
    getRowsFromConsole()
    |> Array.fold (fun acc item -> buildTracks gridCols acc item) (Array.create gridSize 0)
    |> Array.sumBy (fun item -> if item = 0 then 1 else 0) 
    |> printf "%d"

metroProblem ()


// https://www.hackerrank.com/challenges/minimum-absolute-difference-in-an-array/problem
// Input: 
// 3
// 3 -7 0
// Result:
// 3 - -7 = 10, 3 - 0 = 3, -7 - 0 = 7 => 3

// Mutable way - ugly

let minAbsoluteDifference () =
    let mutable min = System.Int32.MaxValue

    let intArray = 
        System.Console.ReadLine() |> ignore
        System.Console.ReadLine().Split() 
        |> Array.map int

    for i = 0 to intArray.Length - 1 do
        for j = i + 1 to intArray.Length - 1 do
            let diff = int (abs (intArray.[i] - intArray.[j]))
            if diff < min then
                min <- diff

    printf "%d" min

minAbsoluteDifference ()

// Immutable way (but w/ recursion) - better!
let minAbsoluteDifference2 () =
    let rec absDiff (lst:int list) =
        let tuples = [ for b in lst.Tail do yield! [abs (lst.Item(0) - b)] ]
        if lst.Tail.Length > 0 then
            List.append tuples (absDiff lst.Tail)
        else
            tuples

    // main
    System.Console.ReadLine() |> ignore
    System.Console.ReadLine().Split() 
    |> Array.map int
    |> Array.toList
    |> absDiff
    |> List.min
    |> printfn "%i"
  

//[1;2;3;] |> loopThru |> printf "%A"
//minAbsoluteDifference2 () |> printf "%i"
minAbsoluteDifference2 () |> ignore


// mutable
let mutable mutablee = 10
mutablee <- 20
printfn "%i" mutablee

// ref
let reff = ref 10
reff := 20
printfn "%i" !reff

// 'use' binding - same as 'using' in c# - calls IDisposable
let exampleUseBinding name =
    let makeResource name = 
       { new System.IDisposable 
         with member this.Dispose() = printfn "%s disposed" name }

    use myResource = makeResource name
    printfn "done"

exampleUseBinding "hello"

// 'do' binding - return 'unit'
do printfn "hello";

// ! bindings
//This simple workflow just sleeps for 2 seconds.
open System
let sleepWorkflow  = async {
    printfn "Starting sleep workflow at %O" DateTime.Now.TimeOfDay
    
    // do! means to wait as well
    do! Async.Sleep 2000
    printfn "Finished sleep workflow at %O" DateTime.Now.TimeOfDay
    }

//test
Async.RunSynchronously sleepWorkflow  


// https://projecteuler.net/problem=1

// Sum all multiples of 3 and 5 up to 1000
let max = 1000
[3 .. 3 .. max] @ [5 .. 5 .. max]
|> List.distinct
|> List.sum

// https://projecteuler.net/problem=2

// Sum fibonnaci up to 4 million

// List.fold [function [state] [item]] [seed] [list]
// Seq.unfold [function [currentState]] [initialState]
// Some() returns a 2-element tuple, first item is item to yield, second is state to pass to generator in next iteration

// fold takes a reducer function, seed, and sequence and reduces them to a single value
// unfold takes a generator function and seed and generates a sequence 

Seq.unfold (fun (num, acc) -> 
    printfn "num: %d, acc: %d, Some: (%d, (%d, %d))" num acc (num+acc) acc (num+acc)  |> ignore
    Some(num+acc, (acc, num+acc))) (0,1)
|> Seq.takeWhile (fun i -> i < 4000000)    
|> Seq.filter (fun x -> printfn "%d" x; true)
|> Seq.filter (fun x -> x % 2 = 0)    
|> Seq.sum




