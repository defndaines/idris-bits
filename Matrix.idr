import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []


-- This is just zipWith (::)
transposeHelper : (x : Vect n elem) ->
                  (xsTrans : Vect n (Vect len elem)) ->
                  Vect n (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys


transposeMat : Vect m (Vect n elem) ->
               Vect n (Vect m elem)
transposeMat [] = replicate _ []
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         zipWith (::) x xsTrans


-- Exercise 3.3.2
addMatrix : Num numType =>
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys


-- Exercise 3.3.3
-- check: multMatrix [[1,2], [3,4], [5,6]] [[7,8,9.10], [11,12,13,14]]
-- equals: [[29, 32,,,], [65,,,],,,]
multMatrix : Num numType =>
             Vect n (Vect m numType) ->
             Vect m (Vect p numType) ->
             Vect n (Vect p numType)
multMatrix xs ys = ?multMatrix_rhs1 xs (transposeMat ys)
