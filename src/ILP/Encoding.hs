module ILP.Encoding(
     encoding
    ) where

import Numeric.Limp.Solvers.Cbc
import Numeric.Limp.Rep
import Numeric.Limp.Program

-- encoding' :: Direction -> PetriNet -> [Id] -> Id -> Program String String IntDouble
-- encoding' dir net inputs res = 
--     where
--         places = pnPlaces net
--         trans = pnTransitions net
--         flows = pnFlows net


encoding :: Direction -> [Int] -> [Int] -> [[Int]] -> [[Int]] ->[Int] -> [Int] -> Int -> Program String String IntDouble
encoding dir a t oute ine ima fma k = 
    program dir
            c0
            ((f_constr t k) :&& (init_marking ima a) :&& (final_marking fma a k) :&& (prefire_constr a t k oute) :&& (postfire_constr a t k oute ine))
            (f_bounds t k ++ p_bounds a k)

f_bounds :: [Int] -> Int -> [Bounds String String IntDouble]
f_bounds t k = concat [map (\tn -> lowerZ 0 (makeF_str tn i)) t | i <- [0..(k-1)]]

p_bounds :: [Int] -> Int -> [Bounds String String IntDouble]
p_bounds a k = concat [map (\pn -> lowerZ 0 (makeP_str pn i)) a | i <- [0..k]]

test :: Constraint String String IntDouble
test = c0 .+. z1 "p" .+. z1 "f" .+. z1 "g" :== c1

test2 :: Constraint String String IntDouble
test2 = z1 "p00" :== c1

f_constr :: [Int] -> Int -> Constraint String String IntDouble
f_constr t k = foldl (:&&) (CTrue) [constr_f i | i <- [0..(k-1)]]
    where
        constr_f i = (foldl func c0 t) :== c1
            where
                func a x = a .+. (makeF x i)

init_marking :: [Int] -> [Int] -> Constraint String String IntDouble
init_marking ima a = foldl (:&&) (CTrue) (map func a)
    where
        func pn = (makeP pn 0) :== conZ (Z (length . filter (pn==) $ ima))

final_marking :: [Int] -> [Int] -> Int -> Constraint String String IntDouble
final_marking fma a k = foldl (:&&) (CTrue) (map func a)
    where
        func pn = (makeP pn k) :== conZ (Z (length . filter (pn==) $ fma))

prefire_constr :: [Int] -> [Int] -> Int -> [[Int]] -> Constraint String String IntDouble
prefire_constr a t k oute = foldl (:&&) (CTrue) allconstr
    where allconstr = [Z (oute !! pn !! tn) *. (makeF tn i) :<= (makeP pn i)  | pn <- a, tn <- t, i <- [0..(k-1)]]

postfire_constr :: [Int] -> [Int] -> Int -> [[Int]] -> [[Int]] -> Constraint String String IntDouble
postfire_constr a t k oute ine = foldl (:&&) (CTrue) allconstr
    where allconstr = [ if ine !! tn !! pn == 0 && oute !! pn !! tn == 0 then CTrue else
                        (makeP pn i) :== (makeP pn (i-1)) .+. sum pn tn i
                    | pn <- a, tn <- t, i <- [1..k]]
            where
                sum pn tn i = foldl (.+.) c0 [(Z (ine !! ttn !! pn - oute !! pn !! ttn)) *. (makeF ttn (i-1)) | ttn <- t]


makeF tn i = z1 $ makeF_str tn i
makeP pn i = z1 $ makeP_str pn i
makeF_str tn i = "F_" ++ (show tn) ++ "_" ++ (show i)
makeP_str pn i = "P_" ++ (show pn) ++ "_" ++ (show i)