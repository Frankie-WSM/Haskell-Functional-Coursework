--- Part 1---

{-1.a
The first application developed with haskell is Detexify. This is a service that allows a user to draw a LaTeX
symbol such as phi and then the program uses haskell for handwriting recognition. The program then produces a
list of similar LaTeX symbols so the user can type them. The backend to Detexify was previously written in a 
language called Ruby. Haskell has since been implemented as it has proven to work much faster in finding suitable 
symbols for a user.

The second application is Sigma by Facebook. Sigma is used to detect spam and site integrity for Facebook and
monitors every interaction a user has with Facebook, such as a like, and evaluates certain policies for that 
interaction to determine malicious intent. Haskell is used as it is purely functional so policies cant affect
each other. This also makes policies easy to test induvidually. Facebook also used Haskell due to the fact it 
performed faster than their previous choice of language, FXL.

The third application is the Caradano's Plutus and Marlowe. These are esentially libraries in haskell. Haskell
was used for Cardano due to the fact it is a very reliable, high security language. This means that for a high 
value economic operating system like Cardano, the likelyhood of an exploit or code failue is much lower. These
smart contract exploits or code failures otherwise could easily lead to billions in financial loss.   

1.b
One advantage of functional programming is lazy evaluation. This means that a program will only store a value 
when it needs to which stops the evaluation of an input being unecessarily repeated. This also means that a program
lets go of temporary computations which greatly improves the programs time complexity.

A second advantage is the fact that pure functions are used in functional programming. This means that the output
produced will always be the same given the same inputs. It also means that they have no side effects (stricty give
an output or modify global variables). These 2 factors combined means that degbugging is easier as there ar no
hidden outputs.

A third advantage is that signatures are more valid in functional programming. This is due to the variables
being pure meaning that the signature gives information on how the entire fucntion works such as the arguments
required.

The last advantage is that prorams are far more readable. This is all down to the fact that pure values are 
used which makes the programmer treat each function as a value in a way. This makes the program easier to 
memorise.

1.c
A mathematical function denotes how you go from a set, lets say X, to another set which could be set Y. It simply
the way line by line that an element in set X will become a different element in set Y. Haskell functions are very
similar as the way they work is by giving the computer the input, and stating what the output will be as a series
of arithmetic on the input value. For example you could take the mathematical function f(x) = x + 1. In Haskell
this would look very similar (add1 x = x + 1) as both state the output in terms of the input.

Mathematical and Haskell functions are also similar due to the fact that both have referential transparency. This 
means that you are able to replace functions with their value without altering the program they are in. Referential
transparency is due to all functions in maths and haskell being pure. -}

--- Part 2 ---

--2.a

type Dog = (String,Int)

{-2.b - The function takes 2 input lists and uses zip to make a new list with tuples consisting of -}

create_dog_list:: [String] -> [Int] -> [Dog]
create_dog_list xs ys = zip xs ys 

--2.c

sort_dog_list:: [Dog] -> [Dog] --esentially a merge sort although the the data type is Dog instead of Int
sort_dog_list [] = []
sort_dog_list [a] = [a]
sort_dog_list xs = merge_dog_list (sort_dog_list ys) (sort_dog_list zs)
  where
  h = div (length xs) 2
  ys = take h xs
  zs = drop h xs

merge_dog_list:: [Dog] -> [Dog] -> [Dog] --this function is to merge an element and a list although the type is dog
merge_dog_list [] ys = ys
merge_dog_list xs [] = xs
merge_dog_list (x:xs) (y:ys) 
  |snd(x) < snd(y) = x : merge_dog_list xs (y:ys) --snd means the second value from the tuple which is the height
  |otherwise =  y: merge_dog_list (x:xs) ys 

{-2.d - The funtion sorts the input list using the functions above, and then removes the first k elements which
will be the smallest after sorting-}

remove_smallest_dogs:: Int -> [Dog] -> [Dog]
remove_smallest_dogs k xs = drop k (sort_dog_list xs)

{-2.e - The function makes a new list from an old one, only passing over values less than 80 from the second value
in each tuple-}

remove_tall_dogs:: [Dog] -> [Dog]
remove_tall_dogs [] = []
remove_tall_dogs x = [y | y <- x, snd y < 80]

--- Part 3 ---

--3.a

star:: Int -> String -- forms a line of stars n characters long
star n = (replicate n '*' ++ "\n")

spaces:: Int -> String -- forms a line of spaces n characters long
spaces n = concat(replicate (n) " ")

reversesteps:: Int -> Int -> Int -> String -- creates the top half of the steps
reversesteps m n 0 = []
reversesteps m n p = reversesteps m n (p-1) ++ concat(replicate m (spaces (n*p) ++ star (n*p)))

normalsteps:: Int -> Int -> Int -> String -- creates the bottom half of the steps 
normalsteps m n 0 = []
normalsteps m n p = concat(replicate m (spaces (n*p) ++ star (n*p))) ++ normalsteps m n (p-1)

steps:: Int -> Int -> Int -> String -- adjoins functions that create both halves of the steps
steps m n 0 = []
steps m n p = reversesteps m n p ++ normalsteps m n p

--3.b

makeflag:: Int -> String
makeflag n = unlines ( lowerhalf ( line n '*' : map newline [0 .. y]) n)
  where y = div (n-3) 2
        newline x = lowerhalf ( concat ["*", line x ' ', "+", line (y - x) ' ']) n

lowerhalf:: Integral a => [b] -> a -> [b]
lowerhalf x n 
  | n `mod` 2 == 0 = x ++ reverse x
  | otherwise = x ++ tail (reverse x)

line:: Int -> Char -> String
line n x
  | n == 0 = ""
  | otherwise = x : line (n-1) x 

flagpattern:: Int -> Int -> String
flagpattern n m
  | n < 5 || m < 1 = ""
  | otherwise = makeflag n ++ flagpattern n (m- 1)

--- Part 5 ---

--nsplit [a]
