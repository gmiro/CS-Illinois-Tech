import Data.HashMap.Strict as H
import Data.List

-- Initial types

type ForthState = (IStack, CStack, Dictionary)

type IStack = [Integer]
initialIStack = []

type CStack = [[String]]
initialCStack = []

-- Type for the symbol dictionary

type Dictionary = H.HashMap String [Entry]

data Entry =
     Prim ([Integer] -> [Integer])
   | Def [String]
   | Num Integer
   | Unknown String

instance Show Entry where
  show (Prim f)    = "Prim"
  show (Def s)     = show s
  show (Num i)     = show i
  show (Unknown s) = "Unknown: " ++ s

-- Dictionary helpers

wrap2 f (x:y:xs) = (f x y):xs
wrap2 f _ = error "Value stack underflow!"

revwrap2 f (x:y:xs) = (f y x):xs
revwrap2 f _ = error "Value stack underflow!"

dlookup :: String -> Dictionary -> Entry
dlookup word dict =
  case H.lookup word dict of
    Nothing -> case reads word of
                 [(i,"")] -> Num i
                 _        -> Unknown word
    Just x  -> head x

dinsert :: String -> Entry -> Dictionary -> Dictionary
dinsert key val dict =
   case H.lookup key dict of
      Nothing -> H.insert key [val] dict
      Just x  -> H.insert key (val:x) dict

-- Helper Functions Begin --
cond f x y =
  if (f y x) then -1
  else 0

dup :: [Integer] -> [Integer]
dup [] = []
dup (x:xs) = (x:x:xs)

dropf [] = []
dropf (x:xs) = xs

swap (x:y:xs) = (y:x:xs)

rotate = reverse

printList [] str = str
printList (x:xs) str = printList xs ((show x) ++ " " ++ str)

addfunction xx diction = dinsert (head (fst (break (==";") xx))) (Def (tail (fst (break(==";") xx)))) diction
restInput xx = (tail (snd (break (==";") xx)))

breakinput xx = (fst (break (== (findlast xx))xx)) ++ [(head (snd (break (==(findlast xx)) xx)))]
findlast xx
  |(elemIndex "again" xx) > (elemIndex "exit" xx) = "again"
  |otherwise                                      = "exit"

-- Control Flow --

funReg xx dict = dinsert (head yy) (Def (tail yy)) dict
  where yy = fst (break (== ";") yy)

loopbody xx = xx

ifStm x yy
  | x == (-1) && (elem "else" yy) = removeElse yy
  | x == (-1) = Data.List.delete "then" yy
  | x == 0 && (elem "else" yy) = removeIf yy
  | otherwise = removeElse yy

removeIf [] = []
removeIf xx = Data.List.delete "then" x
  where x = tail (snd (break (== "else") xx))

removeElse [] = []
removeElse xx
  | (snd x) == [] = z
  | otherwise     = (fst x) ++ z
  where x = (break (== "else") xx)
        z = (tail (snd (break (== "then") xx)))

--- Helper Functions End --



-- Initial Dictionary

dictionary1 = dinsert "+" (Prim $ wrap2 (+)) H.empty
dictionary2 = dinsert "-" (Prim $ wrap2 (-)) dictionary1
dictionary3 = dinsert "*" (Prim $ wrap2 (*)) dictionary2
dictionary4 = (dinsert "=="  (Prim (wrap2 (cond (==)))) dictionary3)
dictionary5 = (dinsert "<"   (Prim (wrap2 (cond (<)))) dictionary4)
dictionary6 = (dinsert ">"    (Prim (wrap2 (cond (>)))) dictionary5)
dictionary7 = (dinsert "+"    (Prim (wrap2 (+))) dictionary6)
dictionary8 = (dinsert "-"    (Prim (revwrap2 (-))) dictionary7)
dictionary9 = (dinsert "/"    (Prim (revwrap2 (div))) dictionary8)
dictionary10 = (dinsert "*"    (Prim (wrap2 (*))) dictionary9)
dictionary11 = (dinsert "rot"  (Prim (rotate)) dictionary10)
dictionary12 = (dinsert "dup"  (Prim (dup)) dictionary11)
dictionary13 = (dinsert "swap" (Prim (swap)) dictionary12)
dictionary14 = (dinsert "drop" (Prim (dropf)) dictionary13)
dictionary  = (dinsert "dup"  (Prim (dup)) dictionary14)

-- The Evaluator

temp=[]

eval :: [String] -> ForthState -> IO ForthState
eval []    (istack, [],     dict) = return (istack, [], dict)
eval words (istack, cstack, dict) =
  case dlookup (head words) dict of
    Num i        -> eval xs (i:istack, cstack, dict)
    Prim f       -> eval xs (f istack, cstack, dict)
    Def p        -> eval (p ++ xs) (istack, cstack, dict)
    Unknown "."  -> do { putStrLn $ show (head istack);
                         eval xs (tail istack, cstack, dict) }
    Unknown ".S" -> do { putStrLn $ (printList istack "");
       eval xs (istack,cstack,dict) }
    Unknown ":"      -> eval (restInput xs) (istack,cstack,(addfunction xs dict))
    Unknown "if"     -> eval (ifStm x xs) ((tail istack), cstack, dict)
    Unknown "begin"  -> eval xs (istack, ((breakinput xs) :  cstack), dict)
    Unknown "again"  -> eval ((head cstack)++xs) (istack,cstack,dict)
    Unknown "exit"   -> eval (tail xs) (istack,(tail cstack),dict)
    Unknown x  -> do { putStrLn ("Unknown " ++ show x);
       eval xs (tail istack, cstack, dict) }

  where xs = tail words
  x  = head istack

printer [] = do{putStr $ ""}
printer (x:xs) = do{printer xs; putStr $ " "++ show x}

swapper (x:y:ys) = do{return (y:(x:(ys)))}

rotter (x1:x2:x3:xs) = do{return (x3:x1:x2:xs)}

--restInput (x:xx) = (tail (snd (break == ";")) xx);

repl :: ForthState -> IO ForthState
repl state =
  do putStr "> " ;
     input <- getLine
     nustate <- eval (words input) state
     repl nustate

main = do
  putStrLn "Welcome to your forth interpreter!"
  repl (initialIStack, initialCStack, dictionary)



-- Question 8 --

-- a recursive factorial
-- : factorial dup 1 == if 1 * else dup 1 - factorial * then ;
-- play with: 6 factorial .S

-- an iterative factorial
-- : fact-helper begin dup 1 == if * exit else dup rot swap * swap 1 - then again ;
-- : factorial 1 swap fact-helper ;
-- play with: 6 factorial .S
