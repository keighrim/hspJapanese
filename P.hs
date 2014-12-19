module P where

import Data.List
import Data.Char
import FPH
import Lexicon

data ParseTree a b =  Ep | Leaf a | Branch b [ParseTree a b] 
                   deriving Eq

instance (Show a, Show b) => Show (ParseTree a b) where
  show Ep            = "[]"
  show (Leaf t)      = show t
  show (Branch l ts) = "[." ++ show l  ++ " " 
                            ++ show ts ++ "]"

snowwhite = Branch "S" 
            [Branch "NP" [Leaf "Snow White"],
             Branch "VP" [Branch "TV" [Leaf "loved"],
                          Branch "NP" [Leaf "the dwarfs"]]]

type Pos = [Int]

pos ::  ParseTree a b -> [Pos]
pos Ep            = [[]]
pos (Leaf _)      = [[]]
pos (Branch _ ts) = [] : [ i:p | (i,t) <- zip [0..] ts, 
                                     p <- pos t ]

subtree :: ParseTree a b -> Pos -> ParseTree a b 
subtree t             []     = t
subtree (Branch _ ts) (i:is) = subtree (ts!!i) is 

subtrees :: ParseTree a b -> [ParseTree a b]
subtrees t = [ subtree t p | p <- pos t ]

type Rel a = [(a,a)]

properdominance :: ParseTree a b -> Rel Pos 
properdominance t = [ (p,q) | p <- pos t, 
                              q <- pos t, 
                              p /= q, 
                              prefix p q ]

dominance :: ParseTree a b -> Rel Pos 
dominance t = [ (p,q) | p <- pos t, 
                        q <- pos t, 
                        prefix p q ]

sisters :: Pos -> Pos -> Bool
sisters [i]    [j]    = i /= j
sisters (i:is) (j:js) = i == j && sisters is js
sisters  _      _     = False 

sisterhood :: ParseTree a b -> Rel Pos 
sisterhood t = [ (p,q) | p <- pos t, 
                         q <- pos t, 
                         sisters p q ]

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

cCommand :: ParseTree a b -> Rel Pos 
cCommand t = (sisterhood t) @@ (dominance t)

branchingPos :: ParseTree a b -> [Pos]
branchingPos t = let ps = pos t in 
  [ p | p <- ps, (p++[0]) `elem` ps, (p++[1]) `elem` ps ]

precede :: Pos -> Pos -> Bool
precede (i:is) (j:js) = i < j || (i == j && precede is js)
precede  _      _     = False

precedence :: ParseTree a b -> Rel Pos
precedence t = [ (p,q) | p <- pos t, 
                         q <- pos t, 
                         precede p q ]

split2 :: [a] -> [([a],[a])]
split2 []     = [([],[])]
split2 (x:xs) = [([],(x:xs))]
             ++ (map (\(ys,zs) -> ((x:ys),zs)) (split2 xs))

splitN :: Int -> [a] -> [[[a]]]
splitN n xs 
  | n <= 1    = error "cannot split"
  | n == 2    = [ [ys,zs] | (ys,zs) <- split2 xs ]
  | otherwise = [ ys:rs   | (ys,zs) <- split2 xs, 
                             rs     <- splitN (n-1) zs ]

recognize :: String -> Bool
recognize = \ xs -> 
    null xs || xs == "a" || xs == "b" || xs == "c"
    || or [ recognize ys | ["a",ys,"a"] <- splitN 3 xs ]
    || or [ recognize ys | ["b",ys,"b"] <- splitN 3 xs ]
    || or [ recognize ys | ["c",ys,"c"] <- splitN 3 xs ]

gener :: Int -> String -> [String]
gener 0 alphabet = [[]]
gener n alphabet = [ x:xs | x  <- alphabet,
                            xs <- gener (n-1) alphabet ]

gener' :: Int -> String -> [String]
gener' n alphabet = gener   n    alphabet 
                 ++ gener' (n+1) alphabet

generateAll  :: String -> [String]
generateAll alphabet = gener' 0 alphabet

generate = filter recognize (generateAll alphabet) 
  where alphabet = ['a','b','c']

parse :: String -> [ParseTree String String]
parse = \ xs -> 
    [Leaf "[]" | null xs ] 
 ++ [Leaf "a"  | xs == "a" ]    
 ++ [Leaf "b"  | xs == "b" ] 
 ++ [Leaf "c"  | xs == "c" ] 
 ++ [Branch "A" [Leaf "a", t, Leaf "a"] | 
                ["a",ys,"a"] <- splitN 3 xs,
                t            <- parse ys   ]
 ++ [Branch "A" [Leaf "b", t, Leaf "b"] | 
                ["b",ys,"b"] <- splitN 3 xs,
                t            <- parse ys   ]
 ++ [Branch "A" [Leaf "c", t, Leaf "c"] | 
                ["c",ys,"c"] <- splitN 3 xs,
                t            <- parse ys   ] 

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b 
succeed r xs = [(r,xs)]

failp :: Parser a b
failp xs = []

symbol :: Eq a => a -> Parser a a 
symbol c []                 = []
symbol c (x:xs) | c == x    = [(x,xs)]
                | otherwise = [] 

token :: Eq a => [a] -> Parser a [a]
token cs xs | cs == take n xs   = [(cs,drop n xs)]
            | otherwise         = []
         where n = length cs

satisfy :: (a -> Bool) -> Parser a a 
satisfy p []                 = []
satisfy p (x:xs) | p x       = [(x,xs)]
                 | otherwise = []

digit :: Parser Char Char 
digit = satisfy isDigit

just :: Parser a b -> Parser a b 
just p = filter (null.snd) . p

infixr 4 <|>
 
(<|>) :: Parser a b -> Parser a b -> Parser a b 
(p1 <|> p2) xs = p1 xs ++ p2 xs 

(<*>) :: Parser a [b] -> Parser a [b] -> Parser a [b]
(p <*> q) xs = [ (r1 ++ r2,zs) | (r1,ys) <- p xs, 
                                 (r2,zs) <- q ys ]

pS,pNP,pVP,pD,pN :: Parser String String

pS  = pNP <*> pVP
pNP = symbol "Alice"  <|> symbol "Dorothy" <|> (pD <*> pN)
pVP = symbol "smiled" <|> symbol "laughed"
pD  = symbol "every"  <|> symbol "some"    <|> symbol "no"
pN  = symbol "dwarf"  <|> symbol "wizard"

infixl 7 <$>

(<$>) :: (a -> b) -> Parser s a -> Parser s b
(f <$> p) xs = [ (f x,ys) | (x,ys) <- p xs ]

digitize :: Parser Char Int
digitize = f <$> digit 
  where f c = ord c - ord '0'

type PARSER a b = Parser a (ParseTree a b)

epsilonT :: PARSER a b 
epsilonT = succeed Ep

symbolT :: Eq a => a -> PARSER a b
symbolT s = (\ x -> Leaf x) <$> symbol s

infixl 6 <:>

(<:>) :: Parser a b -> Parser a [b] -> Parser a [b]
(p <:> q) xs = [ (r:rs,zs) | (r,ys)  <- p xs, 
                             (rs,zs) <- q ys ]

collect :: [Parser a b] -> Parser a [b] 
collect []     = succeed []
collect (p:ps) = p <:> collect ps 

parseAs :: b -> [PARSER a b] -> PARSER a b
parseAs label ps = (\ xs -> Branch label xs) <$> collect ps

sent, np, vp, det, cn :: PARSER String Char
sent =  parseAs 'S' [np,vp]
np   =  symbolT "Alice"  <|> symbolT "Dorothy" 
    <|> parseAs 'N' [det,cn]
det  =  symbolT "every"  <|> symbolT "some" <|> symbolT "no"
cn   =  symbolT "man"    <|> symbolT "woman" 
vp   =  symbolT "smiled" <|> symbolT "laughed" 

palindrome :: PARSER Char Char 
palindrome = 
  epsilonT <|> symbolT 'a' <|> symbolT 'b' <|> symbolT 'c'
           <|> parseAs 'A' [symbolT 'a', palindrome, symbolT 'a']
           <|> parseAs 'A' [symbolT 'b', palindrome, symbolT 'b']
           <|> parseAs 'A' [symbolT 'c', palindrome, symbolT 'c']

many :: Parser a b -> Parser a [b]
many p = (p <:> many p) <|> (succeed [])

parseManyAs :: b -> PARSER a b -> PARSER a b
parseManyAs l p = (\ xs -> Branch l xs) <$> many p 

colour, answer, guess, reaction, turn, game 
   :: PARSER String String

colour   =  symbolT "red"   <|> symbolT "yellow" 
        <|> symbolT "blue"  <|> symbolT "green"
answer   =  symbolT "black" <|> symbolT "white" 
guess    =  parseAs "GUESS" [colour,colour,colour,colour]
reaction =  parseManyAs "REACTION" answer
turn     =  parseAs "TURN" [guess,reaction]
game     =  turn <|> parseAs "GAME" [turn,game]

genderFs   = [Masc,Fem]
personFs   = [Fst,Snd,Thrd]
gcaseFs    = [Nom,Gen,Acc,Dat,Loc,Abl,Comp,Voc,Ins,Top]
pronTypeFs = [Pers,Refl,Wh]
tenseFs    = [Past,Pres,Fut]
modalFs    = [May,Must] 
postTypeFs = [Only,Until,On,With,By]
verbFormFs = [Te,Ta,Nai,Masu,Desu,Reru,You,Stem]
verbTypeFs = [Dan5,Dan1,Irre,Iadj,Nadj]
verbMoodFs = [Decl,Intrg,Intrj,Imper,Hypo,Caus,Pass,Nega]
honorifFs  = [Poli,Resp,Humb,Neutr,Unoff]
animacyFs  = [Anim,Inanim]
repTypeFs  = [Tpiadj,Tpdan5,Tpdan1]
repFormFs  = [Tpiadj,Tpdan5,Tpdan1]
repMoodFs  = [Mddecl,Mdintrg]

gender, person, gcase, pronType, tense, postType, verbType, verbForm, verbMood, honorif, animacy, reType, repMood, repForm
     :: Agreement -> Agreement
gender   = filter (`elem` genderFs)
person   = filter (`elem` personFs)
gcase    = filter (`elem` gcaseFs)
pronType = filter (`elem` pronTypeFs)
tense    = filter (`elem` tenseFs)
modal    = filter (`elem` modalFs)
postType = filter (`elem` postTypeFs)
verbForm = filter (`elem` verbFormFs)
verbType = filter (`elem` verbTypeFs)
verbMood = filter (`elem` verbMoodFs)
honorif  = filter (`elem` honorifFs)
animacy  = filter (`elem` animacyFs)
repType  = filter (`elem` repTypeFs)
repForm  = filter (`elem` repFormFs)
repMood  = filter (`elem` repMoodFs)

instance Show Cat where
  show (Cat "_"  label agr subcatlist) = label ++ show agr
  show (Cat phon label agr subcatlist) = phon  ++ " " 
                                               ++ label ++ show agr

phon :: Cat -> String
phon (Cat ph _ _ _) = ph

catLabel :: Cat -> CatLabel
catLabel (Cat _ label _ _) = label

fs :: Cat -> Agreement 
fs (Cat _ _ agr _) = agr

subcatList :: Cat -> [Cat]
subcatList (Cat _ _ _ cats) = cats

transfrom :: Feat -> Feat
transfrom s
  | s == Mdintrg = Intrg
  | s == Tpdan5 = Dan5
  | s == Tpdan1 = Dan1
  | s == Tpiadj = Iadj
  | s == Tfstem = Stem
  | s == Tfnai = Nai
  | s == Tfte = Te
  | s == Tfta = Ta
  | s == Tfmasu = Masu
  | s == Tfreru = Reru
  | s == Tfyou = You
  | otherwise = Decl

repFilter :: Cat -> Agreement -> Agreement -> Agreement
repFilter cat filt feats
  | filt == repFormFs && length (filter (`elem` filt) (fs cat)) >= 1
      = filter (not . (`elem` verbFormFs)) feats
  | filt == repTypeFs && length (filter (`elem` filt) (fs cat)) >= 1
      = filter (not . (`elem` verbTypeFs)) feats
  | filt == repMoodFs && length (filter (`elem` filt) (fs cat)) >= 1
      = filter (not . (`elem` verbMoodFs)) feats
  | otherwise = feats

combine :: Cat -> Cat -> [Agreement]
combine cat1 cat2 
  | length (repType (fs cat2) ++ repForm (fs cat2) ++ repMood (fs cat2)) >= 1 
      = rawCombine (repFilter cat2 verbTypeFs 
                    (repFilter cat2 verbFormFs
                     (repFilter cat2 verbMoodFs (fs cat1 ++ fs cat2))))
  | otherwise 
      = rawCombine (fs cat1 ++ fs cat2)

rawCombine :: Agreement -> [Agreement]
rawCombine fss = 
 [ feats | length (gender   feats) <= 1,  
           length (person   feats) <= 1, 
           length (gcase    feats) <= 1,
           length (pronType feats) <= 1,
           length (tense    feats) <= 1,
           length (postType feats) <= 1,
           length (verbType feats) <= 1,
           length (verbForm feats) <= 1,
           length (verbMood feats) <= 1,
           length (honorif  feats) <= 1,
           length (animacy  feats) <= 1]
  where 
    feats = (nub . sort) fss

combineNoRep :: Cat -> Cat -> [Agreement]
combineNoRep cat1 cat2 = rawCombine (fs cat1 ++ fs cat2)
 --[ feats | length (gender   feats) <= 1,  
 --          length (person   feats) <= 1, 
 --          length (gcase    feats) <= 1,
 --          length (pronType feats) <= 1,
 --          length (tense    feats) <= 1,
 --          length (postType feats) <= 1,
 --          length (verbType feats) <= 1,
 --          length (verbForm feats) <= 1,
 --          length (verbMood feats) <= 1,
 --          length (honorif  feats) <= 1,
 --          length (animacy  feats) <= 1]
 -- where 
 --   feats = (nub . sort) (fs cat1 ++ fs cat2)
 --    Will deleting prune break everything? Let's see
    --feats = (prune . nub . sort) (fs cat1 ++ fs cat2)

agree :: Cat -> Cat -> Bool
agree cat1 cat2 = not (null (combine cat1 cat2))

assign :: Feat -> Cat -> [Cat]
assign f c@(Cat phon label fs subcatlist) = 
  [Cat phon label fs' subcatlist | 
         fs' <- combine c (Cat "" "" [f] []) ]

scan :: String -> String
scan []                      = []
scan (x:xs) | x `elem` ".,?" = ' ':x:scan xs
            | otherwise      =     x:scan xs

type Words = [String]

lexer :: String -> Words 
lexer = preproc . words . (map toLower) . scan

preproc :: Words -> Words
preproc []                 = []
preproc ["."]              = []
preproc ["?"]              = []
preproc (",":xs)           = preproc xs
preproc (x:xs)             = x : preproc xs


lookupWord :: (String -> [Cat]) -> String -> [Cat]
lookupWord db w = [ c | c <- db w ]

collectCats :: (String -> [Cat]) -> Words -> [[Cat]]
collectCats db words = 
  let
    listing = map (\ x -> (x,lookupWord db x)) words
    unknown = map fst (filter (null.snd) listing)
  in
    if unknown /= [] then 
      error ("unknown words: " ++ show unknown)
    else initCats (map snd listing) 

initCats :: [[Cat]] -> [[Cat]]
initCats []         = [[]]
initCats (cs:rests) = [ c:rest | c    <- cs, 
                                 rest <- initCats rests ]

t2c :: ParseTree Cat Cat -> Cat
t2c (Leaf   c)   = c
t2c (Branch c _) = c

agreeC :: ParseTree Cat Cat -> ParseTree Cat Cat -> Bool
agreeC t1 t2 = agree (t2c t1) (t2c t2) 

leafP :: CatLabel -> PARSER Cat Cat
leafP label []     = []
leafP label (c:cs) = [(Leaf c,cs) | catLabel c == label ]

assignT :: Feat ->  ParseTree Cat Cat 
                -> [ParseTree Cat Cat]
assignT f (Leaf   c)    = [Leaf   c'    | c' <- assign f c]
assignT f (Branch c ts) = [Branch c' ts | c' <- assign f c]

{--
sRule :: PARSER Cat Cat
sRule = \ xs -> 
       [ (Branch (Cat "_" "S" [] []) [np',vp],zs) | 
         (np,ys) <- parseNP xs,
         (vp,zs) <- parseVP ys, 
         np'     <- assignT Nom np,
         agreeC np vp,
         subcatList (t2c vp) == [] ]
--}


-- Consider original vpRule below 
-- Consider parseNPsorPPs below
-- use the "many" operator in some way to choose an arbitrary # of PartPs, PPs, or a VP
sRule :: PARSER Cat Cat
sRule = \ xs -> 
       [ (Branch (Cat "_" "S" [] []) xps,zs) | 
         (xps,zs) <- parsePartPsorPPsorVP xs ]

parseSent :: PARSER Cat Cat
parseSent = sRule 

-- New particle phrase implementation, no agreement necessary
-- KRIM: should takes multiple particles (cururu + sann + ni) 
partpRule :: PARSER Cat Cat 
partpRule = \ xs -> 
  [ (Branch (Cat "_" "PartP" fs []) (np:part),zs) | 
    (np,ys)   <- parseNP xs, 
    (part,zs) <- parseParts  ys,
    fs        <- superCombine np part ]

parsePartP :: PARSER Cat Cat
parsePartP = partpRule

parsePart :: PARSER Cat Cat
parsePart = leafP "CASE"

parseNPorPart :: PARSER Cat Cat
parseNPorPart = parseNP <|> parsePart

parseParts :: [Cat] -> [([ParseTree Cat Cat],[Cat])]
parseParts = many parsePart
    
-- This npRule can be deleted if we choose to ignore the 3 determiners we have
-- And also ignore the possibility of relative clauses (which is probably fine)
-- I am worried if recursion is okay here, if not then we would need to change 
-- all our labels from NP to something else
npRule :: PARSER Cat Cat 
npRule = \ xs -> 
  [ (Branch (Cat "_" "N" fs []) [det,np],zs) | 
    (det,ys) <- parseDET xs, 
    (np,zs)  <- parseNP  ys,
    fs       <- combine (t2c det) (t2c np) ]

parseNP :: PARSER Cat Cat
parseNP = leafP "N" <|> npRule

parseDET :: PARSER Cat Cat
parseDET = leafP "DET"

{--
npRule :: PARSER Cat Cat 
npRule = \ xs -> 
  [ (Branch (Cat "_" "NP" fs []) [det,cn],zs) | 
    (det,ys) <- parseDET xs, 
    (cn,zs)  <- parseCN  ys,
    fs       <- combine (t2c det) (t2c cn),
    agreeC det cn ]

ppRule :: PARSER Cat Cat
ppRule = \ xs -> 
   [ (Branch (Cat "_" "PP" fs []) [prep,np'],zs) | 
     (prep,ys) <- parsePrep xs, 
     (np,zs)   <- parseNP ys,
      np'      <- assignT AccOrDat np, 
      fs       <- combine (t2c prep) (t2c np') ]
--}


-- A new PP rule for postpositions
-- Our postpositions don't have features, so no need to assign them to NP like 
-- old rule above
ppRule :: PARSER Cat Cat
ppRule = \ xs -> 
   [ (Branch (Cat "_" "PP" fs []) [np,post],zs) | 
     (np,ys)   <- parseNP xs, 
     (post,zs) <- parsePost ys,
      fs       <- combine (t2c np) (t2c post) ]

parsePP :: PARSER Cat Cat
parsePP = ppRule 

parsePartPorPPorVP :: PARSER Cat Cat
parsePartPorPPorVP = parsePartP <|> parsePP <|> parseVP

parsePartPsorPPsorVP :: [Cat] -> [([ParseTree Cat Cat],[Cat])]
parsePartPsorPPsorVP = many parsePartPorPPorVP

-- Compiler complains if we leave these out because of line 713
-- Consider pruning that section later if we have time
parseNPorPP :: PARSER Cat Cat
parseNPorPP = parseNP <|> parsePP

parseNPsorPPs :: [Cat] -> [([ParseTree Cat Cat],[Cat])]
parseNPsorPPs = many parseNPorPP

-- No more CNs or Preps
{--
parseCN :: PARSER Cat Cat
parseCN = leafP "CN"

parsePrep :: PARSER Cat Cat
parsePrep = leafP "PREP"
--}

-- Now we have Postpositions
parsePost :: PARSER Cat Cat
parsePost = leafP "POST"

parseAux :: PARSER Cat Cat
parseAux = leafP "AUX"

parseVP :: PARSER Cat Cat 
parseVP = vpRule
--parseVP = finPastVpRule <|> finPresVpRule <|> finFutVpRule <|> finPerfVpRule <|> auxVpRule

-- Here is where the real headaches begin - the VP
-- No subcat frames, but all those glorious Auxen + endings
vpRule :: PARSER Cat Cat
vpRule = \xs -> 
 [ (Branch (Cat "_" "V" fs []) (vp:xps),zs) |  
   (vp,ys)  <- leafP "V" xs, 
   (xps,zs) <- parseFins ys,
   fs       <- superCombine vp xps,
   and (map (\x -> agreeC vp x) xps)]
   
superCombine :: ParseTree Cat Cat -> [ParseTree Cat Cat] -> [Agreement]
superCombine cat1 catlist =
    concat (map (\x -> combine (t2c cat1) (t2c x)) catlist)
   
finRule :: PARSER Cat Cat
finRule = \ xs -> 
   [ (Branch (Cat "_" "FIN" fs []) [end,fin],zs) | 
     (end,ys) <- parseEndorInf xs, 
     (fin,zs) <- parseFin ys,
      fs      <- combine (t2c end) (t2c fin),
      agreeC end fin]
    
parseEnd :: PARSER Cat Cat
parseEnd = leafP "END" 

parseFin :: PARSER Cat Cat
parseFin = leafP "FIN" <|> finRule

parseInf :: PARSER Cat Cat
parseInf = leafP "INF"

parseEndorInf :: PARSER Cat Cat
parseEndorInf = parseEnd <|> parseInf

parseFins :: [Cat] -> [([ParseTree Cat Cat],[Cat])]
parseFins = many parseFin

-- We do not need to match subcat lists, but we do need to check if Auxen are in
-- the right order. We can give them numeric features, and if something in category
-- 1 is before 2 or higher then it is no good. The only problem is determining
-- if there is an order that always works. Since we can have Auxen which come after
-- the main verb, and they would also need their own endings. So maybe we treat
-- main verb and Auxen separately, and simply combine them if main Verb is in Te form?
match :: [Cat] -> [Cat] -> Bool
match []     []     = True
match _      []     = False
match []      _     = False
match (x:xs) (y:ys) = catLabel x == catLabel y 
            && agree x y 
            && match xs ys 

finPastVpRule :: PARSER Cat Cat
finPastVpRule = \xs -> [(vp',ys) | (vp,ys) <- vpRule xs, 
              vp'    <- assignT Past vp ]

finPresVpRule :: PARSER Cat Cat
finPresVpRule = \xs -> [(vp',ys) | (vp,ys) <- vpRule xs, 
              vp'    <- assignT Pres vp ]

finFutVpRule :: PARSER Cat Cat
finFutVpRule = \xs -> [(vp',ys) | (vp,ys) <- vpRule xs, 
             vp'    <- assignT Fut vp ]

-- No perfect
{--
finPerfVpRule :: PARSER Cat Cat
finPerfVpRule = \xs -> [(vp',ys) | (vp,ys) <- vpRule xs, 
              vp'    <- assignT Perf vp ]
--}


-- Need new auxVpRule - a nightmare?
auxVpRule :: PARSER Cat Cat
auxVpRule = \xs -> 
 [(Branch (Cat "_" "VP" (fs (t2c aux)) []) [aux,inf'],zs) | 
  (aux,ys) <- parseAux xs,
  (inf,zs) <- vpRule ys, 
  inf'    <- assignT Must inf ] 
{--
auxVpRule :: PARSER Cat Cat
auxVpRule = \xs -> 
 [(Branch (Cat "_" "VP" (fs (t2c aux)) []) [aux,inf'],zs) | 
  (aux,ys) <- parseAux xs,
  (inf,zs) <- vpRule ys, 
  inf'    <- assignT Infl inf ]  
--}

prs :: String -> [ParseTree Cat Cat]
prs string = let ws = lexer string 
     in  [ s | catlist <- collectCats lexicon ws, 
            (s,[])  <- parseSent catlist ]

----------------------------------------------------------------------------
-- I am not sure we need anything below here, since prs uses parseSent which uses
-- sRule. Below seems like ways to deal with ambiguous parse trees? I guess I will
-- look at it more later.
            
type StackParser a b = [a] -> [a] -> [(b,[a],[a])]

type SPARSER a b = StackParser a (ParseTree a b)

infixr 4 <||>

(<||>) :: StackParser a b -> StackParser a b 
            -> StackParser a b 
(p1 <||> p2) stack xs = p1 stack xs ++ p2 stack xs 

infixl 6 <::>

(<::>) :: StackParser a b  -> StackParser a [b] 
                           -> StackParser a [b]
(p <::> q) us xs = [(r:rs,ws,zs) | (r,vs,ys)  <- p us xs,
                                   (rs,ws,zs) <- q vs ys ]

succeedS :: b -> StackParser a b 
succeedS r us xs = [(r,us,xs)]

manyS :: StackParser a b -> StackParser a [b]
manyS p = (p <::> manyS p) <||> succeedS []

push :: Cat -> SPARSER Cat Cat -> SPARSER Cat Cat 
push c p stack = p (c:stack) 

pop :: CatLabel -> SPARSER Cat Cat 
pop c []     xs                   = []
pop c (u:us) xs | catLabel u == c = [(Leaf u, us, xs)]
                | otherwise       = []

leafPS :: CatLabel -> SPARSER Cat Cat
leafPS l _ []         = [] 
leafPS l s (c:cs) = [(Leaf c,s,cs) | catLabel c == l ]

prsTXT :: SPARSER Cat Cat
prsTXT = conjR <||> prsS

conjR :: SPARSER Cat Cat 
conjR = \ us xs -> 
   [ (Branch (Cat "_" "TXT" [] []) [s, conj, txt], ws, zs) | 
       (s,vs,ys)      <- prsS us xs,
       (conj,vs1,ys1) <- leafPS "CONJ" vs ys, 
       (txt,ws,zs)    <- prsTXT vs1 ys1            ]

prsS :: SPARSER Cat Cat
prsS = spR <||> cond1R <||> cond2R

spR :: SPARSER Cat Cat 
spR = \ us xs -> 
 [ (Branch (Cat "_" "S" (fs (t2c np)) []) [np',vp],ws,zs) | 
       (np,vs,ys) <- prsNP us xs,
       (vp,ws,zs) <- prsVP vs ys, 
        np'       <- assignT Nom np, 
       agreeC np vp,
       subcatList (t2c vp) == [] ]

cond1R :: SPARSER Cat Cat 
cond1R = \ us xs -> 
   [ (Branch (Cat "_" "S" [] []) [cond,s1,s2], ws, zs) | 
       (cond,vs,ys) <- leafPS "COND" us xs, 
       (s1,vs1,ys1) <- prsS vs ys,
       (s2,ws,zs)   <- prsS vs1 ys1 ]

cond2R :: SPARSER Cat Cat 
cond2R = \ us xs -> 
     [ (Branch (Cat "_" "S" [] []) [cond,s1,s2], ws, zs) | 
         (cond,vs,ys) <- leafPS "COND" us xs, 
         (s1,vs1,ys1) <- prsS vs ys,
         (_,vs2,ys2)  <- leafPS "THEN" vs1 ys1, 
         (s2,ws,zs)   <- prsS vs2 ys2 ]

prsNP :: SPARSER Cat Cat 
prsNP = leafPS "NP" <||> npR <||> pop "NP" 

npR :: SPARSER Cat Cat
npR = \ us xs -> 
  [ (Branch (Cat "_" "NP" fs []) [det,cn], (us++ws), zs) | 
      (det,vs,ys) <- prsDET [] xs, 
      (cn,ws,zs)  <- prsCN vs ys,
       fs         <- combine (t2c det) (t2c cn),
      agreeC det cn ]

prsDET :: SPARSER Cat Cat
prsDET = leafPS "DET"

prsCN :: SPARSER Cat Cat
prsCN = leafPS "CN" <||> cnrelR 

prsVP :: SPARSER Cat Cat
prsVP = finPastVpR <||> finPresVpR <||> finFutVpR <||> auxVpR
--prsVP = finPastVpR <||> finPresVpR <||> finFutVpR <||> finPerfVpR <||> auxVpR

vpR :: SPARSER Cat Cat
vpR = \us xs -> 
 [(Branch (Cat "_" "VP" (fs (t2c vp)) []) (vp:xps),ws,zs) |  
             (vp,vs,ys)  <- leafPS "VP" us xs, 
             subcatlist  <- [subcatList (t2c vp)],
             (xps,ws,zs) <- prsNPsorPPs vs ys, 
             match subcatlist (map t2c xps) ]

finPastVpR :: SPARSER Cat Cat
finPastVpR = \us xs -> [(vp',vs,ys) | (vp,vs,ys) <- vpR us xs,
                                   vp' <- assignT Past vp ]

finPresVpR :: SPARSER Cat Cat
finPresVpR = \us xs -> [(vp',vs,ys) | (vp,vs,ys) <- vpR us xs,
                                   vp' <- assignT Pres vp ]

finFutVpR :: SPARSER Cat Cat
finFutVpR = \us xs -> [(vp',vs,ys) | (vp,vs,ys) <- vpR us xs,
                                   vp' <- assignT Fut vp ]

-- Japanese is already perfect
{--
finPerfVpR :: SPARSER Cat Cat
finPerfVpR = \us xs -> [(vp',vs,ys) | (vp,vs,ys) <- vpR us xs,
                                   vp' <- assignT Perf vp ]
--}

-- More Auxen handling
auxVpR :: SPARSER Cat Cat
auxVpR = \us xs -> 
     [ (Branch (Cat "_" "VP" (fs (t2c aux)) []) 
               [aux,inf'], ws, zs) | 
                 (aux,vs,ys) <- prsAUX us xs,
                 (inf,ws,zs) <- vpR vs ys,
                  inf'       <- assignT Must inf ] 
{--
auxVpR :: SPARSER Cat Cat
auxVpR = \us xs -> 
     [ (Branch (Cat "_" "VP" (fs (t2c aux)) []) 
               [aux,inf'], ws, zs) | 
                 (aux,vs,ys) <- prsAUX us xs,
                 (inf,ws,zs) <- vpR vs ys,
                  inf'       <- assignT Infl inf ] 
--}

prsAUX :: SPARSER Cat Cat
prsAUX = leafPS "AUX" <||> pop "AUX" 

prsPP :: SPARSER Cat Cat
prsPP = ppR <||> pop "PP" 


-- New ppR to consider
ppR :: SPARSER Cat Cat
ppR = \us xs -> 
  [ (Branch (Cat "_" "PP" fs []) [prep,np'], ws, zs) | 
      (prep,vs,ys) <- prsPREP us xs, 
      (np,ws,zs)   <- prsNP vs ys,
       np'         <- assignT Acc np, 
       fs          <- combine (t2c prep) (t2c np') ]
{--
ppR :: SPARSER Cat Cat
ppR = \us xs -> 
  [ (Branch (Cat "_" "PP" fs []) [prep,np'], ws, zs) | 
      (prep,vs,ys) <- prsPREP us xs, 
      (np,ws,zs)   <- prsNP vs ys,
       np'         <- assignT AccOrDat np, 
       fs          <- combine (t2c prep) (t2c np') ]
--}

prsPREP :: SPARSER Cat Cat
prsPREP = leafPS "PREP"

prsNPorPP :: SPARSER Cat Cat
prsNPorPP = prsNP <||> prsPP

prsNPsorPPs :: [Cat] -> [Cat] 
       -> [([ParseTree Cat Cat],[Cat],[Cat])]
prsNPsorPPs = manyS prsNPorPP

cnrelR :: SPARSER Cat Cat
cnrelR = \us xs -> 
     [ (Branch (Cat "_" "CN" (fs (t2c cn)) []) 
               [cn,rel], ws, zs) |
                 (cn,vs,ys)  <- leafPS "CN" us xs, 
                 (rel,ws,zs) <- prsREL vs ys, 
                 agreeC cn rel ]

prsREL :: SPARSER Cat Cat 
prsREL = relclauseR <||> thatlessR 

relclauseR :: SPARSER Cat Cat
relclauseR = \us xs -> 
  [(Branch (Cat "_" "COMP" fs []) [rel,s], ws, zs) |
      (rel,vs,ys) <- leafPS "REL" us xs, 
       fs         <- [fs (t2c rel)],
       gap        <- [Cat "#" "NP" fs []],
       (s,ws,zs)  <- push gap prsS vs ys ]

       
-- Comparative?
thatlessR :: SPARSER Cat Cat 
thatlessR = \ us xs -> 
        [ (Branch (Cat "_" "COMP" [] []) [s], vs, ys) | 
           gap       <- [Cat "#" "NP" [Comp] []], 
           (s,vs,ys) <- push gap prsS us xs, 
           notElem Wh (fs (t2c s))                       ]
{--
thatlessR :: SPARSER Cat Cat 
thatlessR = \ us xs -> 
        [ (Branch (Cat "_" "COMP" [] []) [s], vs, ys) | 
           gap       <- [Cat "#" "NP" [AccOrDat] []], 
           (s,vs,ys) <- push gap prsS us xs, 
           notElem Wh (fs (t2c s))                       ]
--}

prsYN :: SPARSER Cat Cat 
prsYN = \us xs -> 
   [(Branch (Cat "_" "YN" [] []) [aux,s], ws,zs) | 
       (aux,vs,ys) <- prsAUX us xs, 
       gap         <- [Cat "#" "AUX" (fs (t2c aux)) [] ], 
       (s,ws,zs)   <- push gap prsS vs ys ]

isWH :: ParseTree Cat Cat -> Bool
isWH tr = Wh `elem` (fs (t2c tr))

prsWH :: SPARSER Cat Cat 
prsWH = \us xs -> 
   [ (Branch (Cat "_" "WH" [] []) [wh,yn], ws,zs) | 
       (wh,vs,ys) <- prsNPorPP us xs, 
       isWH wh, 
       gapfs      <- [filter (/= Wh) (fs (t2c wh))],
       gap        <- [Cat "#" (catLabel (t2c wh)) gapfs []], 
       (yn,ws,zs) <- push gap prsYN vs ys ]

parses :: String -> [ParseTree Cat Cat]
parses str = let ws = lexer str 
             in  [ s | catlist   <- collectCats lexicon ws, 
                       (s,[],[]) <- prsTXT [] catlist  
                                 ++ prsYN  [] catlist   
                                 ++ prsWH  [] catlist ]

testSuite1 :: [String]
testSuite1 = 
 [ "Alice admired Dorothy.",
   "Did Alice admire Dorothy?", 
   "Who did Alice admire?",
   "Atreyu gave the sword to the princess.",
   "Did Atreyu give the sword to the princess?",
   "Who did Atreyu give the sword to?",
   "To whom did Atreyu give the sword?",
   "Goldilocks helped the girl " 
    ++ "that Atreyu gave the sword to.",

  "Did Goldilocks help the girl " 
    ++ "that Atreyu gave the sword to.",
   "Goldilocks helped the boy that helped the princess " 
    ++ "that Atreyu gave the sword to." ]

testSuite2 :: [String]
testSuite2 =  
 [ "Dorothy admired the boy that Alice helped Atreyu",
   "Dorothy admired the boy that helped",
   "Dorothy admired the girl that " 
    ++ "Atreyu helped the princess that gave the sword to" ]

data Term = Const String | Var Int deriving (Eq,Ord)

data GQ = Sm | All | Th | Most | Many | Few 
        deriving (Eq,Show,Ord) 

data Abstract = MkAbstract Int LF deriving (Eq,Ord) 

data LF = Rel String [Term] 
        | Eq   Term Term
        | Neg  LF 
        | Impl LF LF 
        | Equi LF LF 
        | Conj [LF]
        | Disj [LF] 
        | Qt GQ Abstract Abstract 
     deriving (Eq,Ord)

instance Show Term where
  show (Const name) = name 
  show (Var i)      = 'x': show i

instance Show Abstract where 
  show (MkAbstract i lf) = 
   "(\\ x" ++ show i ++ " " ++ show lf ++ ")"

instance Show LF where
  show (Rel r args)   = r ++ show args
  show (Eq t1 t2)     = show t1 ++ "==" ++ show t2
  show (Neg lf)       = '~': (show lf)
  show (Impl lf1 lf2) = "(" ++ show lf1 ++ "==>" 
                            ++ show lf2 ++ ")"
  show (Equi lf1 lf2) = "(" ++ show lf1 ++ "<=>" 
                            ++ show lf2 ++ ")"
  show (Conj [])      = "true" 
  show (Conj lfs)     = "conj" ++ concat [ show lfs ]
  show (Disj [])      = "false" 
  show (Disj lfs)     = "disj" ++ concat [ show lfs ]
  show (Qt gq a1 a2)   = show gq ++ (' ' : show a1) 
                                 ++ (' ' : show a2)

transS :: ParseTree Cat Cat -> LF
transS (Branch (Cat _ "S" _ _) [np,vp]) = 
  (transNP np) (transVP vp)

transS (Branch (Cat _ "YN" _ _) 
       [Leaf (Cat "did"    "AUX" _ []),s]) = transS s 
transS (Branch (Cat _ "YN" _ _) 
       [Leaf (Cat "didn't" "AUX" _ []),s]) = Neg (transS s)

transNP :: ParseTree Cat Cat -> 
                (Term -> LF) -> LF
transNP (Leaf (Cat "#"  "NP" _ _)) = \ p -> p (Var 0)
transNP (Leaf (Cat name "NP" _ _)) = \ p -> p (Const name)
transNP (Branch (Cat _ "NP" _ _) [det,cn]) = 
                             (transDET det) (transCN cn) 

transDET :: ParseTree Cat Cat -> (Term -> LF)
                              -> (Term -> LF) 
                              -> LF
transDET (Leaf (Cat "every" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt All       (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))

transDET (Leaf (Cat "all" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt All       (MkAbstract i (p (Var i)))  
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "some" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Sm      (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "a" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Sm      (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "several" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Sm      (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "no" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Neg (Qt Sm (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i))))
transDET (Leaf (Cat "the" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Th        (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "most" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Most      (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "many" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Many      (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "few" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Neg (Qt Many (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i))))

transDET (Leaf (Cat "which" "DET" _ _)) = 
  \ p q -> Conj [p (Var 0),q (Var 0)]

transCN :: ParseTree Cat Cat -> Term -> LF
transCN (Leaf   (Cat name "CN" _ _))          = \ x -> 
                                              Rel name [x]
transCN (Branch (Cat _    "CN" _ _) [cn,rel]) = \ x -> 
                       Conj [transCN cn x, transREL rel x]

transREL :: ParseTree Cat Cat -> Term -> LF
transREL (Branch (Cat _ "COMP" _ _ ) [rel,s]) = 
  \ x -> sub x (transS s)
transREL (Branch (Cat _ "COMP" _ _ ) [s])     = 
  \ x -> sub x (transS s)

transPP :: ParseTree Cat Cat -> (Term -> LF) -> LF
transPP (Leaf   (Cat "#" "PP" _ _)) = \ p -> p (Var 0)
transPP (Branch (Cat _   "PP" _ _) [prep,np]) = transNP np

transVP :: ParseTree Cat Cat -> Term -> LF
transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat name "VP" _ [])]) = 
        \ t -> Rel name [t]
transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat name "VP" _ [_]),np]) = 
        \ subj -> transNP np (\ obj -> Rel name [subj,obj])

transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat name "VP" _ [_,_]),np,pp]) = 
        \ subj   -> transNP np 
        (\ obj   -> transPP pp
         (\ iobj -> Rel name [subj,obj,iobj]))
transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat "did" "AUX" _ []),vp]) = 
        transVP vp 
transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat "didn't" "AUX" _ []),vp]) = 
        \x -> Neg ((transVP vp) x)
transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat "#" "AUX" _ []),vp]) = 
        transVP vp 

transWH :: ParseTree Cat Cat -> Abstract
transWH (Branch (Cat _ "WH" _ _ ) [wh,s]) = 
  MkAbstract 0 (Conj [transW wh, transS s])

transW :: ParseTree Cat Cat -> LF
transW (Branch (Cat _ "NP" fs _) [det,cn]) = 
                            transCN cn (Var 0)
transW (Leaf (Cat _ "NP" fs _)) 
      | Masc      `elem` fs = Rel "man"    [Var 0]
      | Fem       `elem` fs = Rel "woman"  [Var 0]
      | otherwise           = Rel "thing"  [Var 0]

transW (Branch (Cat _ "PP" fs _) [prep,np]) 
      | Masc      `elem` fs = Rel "man"    [Var 0]
      | Fem       `elem` fs = Rel "woman"  [Var 0]
      | otherwise           = Rel "thing"  [Var 0]
{--
transW (Leaf (Cat _ "NP" fs _)) 
      | Masc      `elem` fs = Rel "man"    [Var 0]
      | Fem       `elem` fs = Rel "woman"  [Var 0]
      | MascOrFem `elem` fs = Rel "person" [Var 0]
      | otherwise           = Rel "thing"  [Var 0]

transW (Branch (Cat _ "PP" fs _) [prep,np]) 
      | Masc      `elem` fs = Rel "man"    [Var 0]
      | Fem       `elem` fs = Rel "woman"  [Var 0]
      | MascOrFem `elem` fs = Rel "person" [Var 0]
      | otherwise           = Rel "thing"  [Var 0]
--}

subst :: Term -> Term -> Term 
subst x (Const name)         = Const name
subst x (Var n) | n == 0     = x
                | otherwise  = Var n
                | x == Var n = error "bad substitution"

sub :: Term -> LF -> LF 
sub x (Rel name ts)     = Rel name (map (subst x) ts)
sub x (Eq t1 t2)        = Eq (subst x t1) (subst x t2)
sub x (Neg lf)          = Neg (sub x lf)
sub x (Impl lf1 lf2)    = Impl (sub x lf1) (sub x lf2)
sub x (Equi lf1 lf2)    = Equi (sub x lf1) (sub x lf2)
sub x (Conj lfs)        = Conj (map (sub x) lfs) 
sub x (Disj lfs)        = Disj (map (sub x) lfs) 
sub x (Qt gq abs1 abs2) = Qt gq (sb x abs1) (sb x abs2)

sb :: Term -> Abstract -> Abstract
sb x (MkAbstract 0 lf) = MkAbstract 0 lf
sb x (MkAbstract n lf) = MkAbstract n (sub x lf)

bInLF :: LF -> [Int]
bInLF (Rel _ _)         = []
bInLF (Eq _  _)         = []

bInLF (Neg lf)          = bInLF lf 
bInLF (Impl lf1 lf2)    = bInLFs [lf1,lf2] 
bInLF (Equi lf1 lf2)    = bInLFs [lf1,lf2]
bInLF (Conj lfs)        = bInLFs lfs 
bInLF (Disj lfs)        = bInLFs lfs 
bInLF (Qt gq abs1 abs2) = bInAs [abs1,abs2] 

bInLFs :: [LF] -> [Int]
bInLFs = nub . concat . map bInLF

bInA :: Abstract -> [Int]
bInA (MkAbstract i lf) = i: bInLF lf

bInAs :: [Abstract] -> [Int]
bInAs = nub . concat . map bInA

freshIndex  :: [LF] -> Int
freshIndex lfs = i+1
  where i      = foldr max 0 (bInLFs lfs)

fresh :: [Term -> LF] -> Int
fresh preds   = freshIndex (map ($ dummy) preds) 
  where dummy = Const ""    

process :: String -> [LF]
process string = map transS (parses string)

processW :: String -> [Abstract]
processW string = map transWH (parses string)

