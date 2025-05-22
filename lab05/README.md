# lab05

Q: What is (b:_) for?
This is pattern matching that takes a list and binds the first element to b and ignores the rest with _. It's used to extract the second element of the original list (since we're already inside a pattern match where a is the first element).
Q: What does the t@ do?
This is an "as pattern" in Haskell. The pattern t@(b:_) means "match the pattern (b:_) and also bind the entire matched value to t". So t will reference the tail of the original list (everything except the first element a).
Understanding the recursive fibs definition:
The definition fibs = 0 : 1 : next fibs creates an infinite list that:

Starts with 0 and 1
Continues with values calculated by the next function
next takes a list, extracts its first two elements (a and b), adds them together, and consumes to construct the rest of the list recursively

Bonus with zipWith:
The alternate definition fibsZip = 0 : 1 : zipWith (+) fibsZip (tail fibsZip) works by:

Starting with 0 and 1
Using zipWith to add each element of fibsZip with the corresponding element of tail fibsZip (which is fibsZip without its first element)
This creates a new list where each element is the sum of two consecutive elements from the original list

One-liner bonus task:
The count function uses zipWith to compare each element of the input list with the target value, returning 1 when they match and 0 otherwise. Then it uses sum to count the total matches.