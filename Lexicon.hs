module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = 

    Fst | Snd | Thrd | Masc | Fem
    -- lists from Wikipedia http://en.wikipedia.org/wiki/Japanese_particles
    -- Case markers 
    | Nom | Gen | Acc | Dat | Loc | Alla | Abl | Comp | Voc | Ins | Top
    | Only | Until | On | With | By                             -- postpositions
    | Intrg | Intrj | Imper | Volit | Hypo | Caus | Pass | Nega -- Verb Moods
    | Poli | Resp | Humb | Neutr | Unoff                        -- honorifics 
    | Dan5 | Dan1 | Irre | Iadj | Nadj                          -- Verb types
    | Stem | Te | Ta | Nai | Masu | Desu | Reru | You           -- Ending types 
    | Pers  | Refl | Wh 
    | Past  | Pres | Fut | May | Must                         -- tense and modal
    | Tpiadj | Tpdan5 | Tpdan1                                -- Transformation 
    | Tfstem | Tfnai | Tfte | Tfta | Tfmasu | Tfreru | Tfyou  -- features
    | Anim  | Inanim                                          -- Noun features  
    deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]

-- Pronouns
lexicon "watashi"   = [Cat "watashi"  "N" [Anim, Fst, Poli]  []]  -- I
lexicon "boku"      = [Cat "boku"     "N" [Anim, Fst]        []]  -- I
lexicon "anata"     = [Cat "anata"    "N" [Anim, Snd, Poli]  []]  -- You
lexicon "kimi"      = [Cat "kimi"     "N" [Anim, Snd]        []]  -- You
lexicon "kare"      = [Cat "kare"     "N" [Anim, Thrd, Masc] []]  -- He
lexicon "kanojo"    = [Cat "kanojo"   "N" [Anim, Thrd, Fem]  []]  -- She
lexicon "kore"      = [Cat "kore"     "N" [Inanim]    []]         -- This
lexicon "sore"      = [Cat "sore"     "N" [Inanim]    []]         -- That
lexicon "are"       = [Cat "are"      "N" [Inanim]    []]         -- That
lexicon "koko"      = [Cat "koko"     "N" [Inanim]    []]         -- Here
lexicon "soko"      = [Cat "soko"     "N" [Inanim]    []]         -- There
lexicon "asoko"     = [Cat "asoko"    "N" [Inanim]    []]         -- There
-- Reflexive
lexicon "jibun"     = [Cat "jibun"    "N" [Anim]      []]         -- Self

-- Names just like normal nouns
lexicon "rim"           = [Cat "rim"          "N" [Anim] []]
lexicon "curcuru"       = [Cat "curcuru"      "N" [Anim] []]
lexicon "arisu"         = [Cat "arisu"        "N" [Anim] []]  -- alice
lexicon "doroshi"       = [Cat "doroshi"      "N" [Anim] []]  -- dorothy
lexicon "sunouhowaito"  = [Cat "sunouhowaito" "N" [Anim] []]  -- snowwhite

-- Determiners
lexicon "kono"    = [Cat "kono"    "DET" []    []]
lexicon "sono"    = [Cat "sono"    "DET" []    []]
lexicon "ano"     = [Cat "ano"     "DET" []    []]

-- Nouns
lexicon "banana"    = [Cat "banana"     "N" [Inanim] []] -- Banana
lexicon "jitensha"  = [Cat "jitensha"   "N" [Inanim] []] -- Bicycle
lexicon "arubaito"  = [Cat "arubaito"   "N" [Inanim] []] -- Part-time Job
lexicon "atama"     = [Cat "atama"      "N" [Inanim] []] -- Head
lexicon "hito"      = [Cat "hito"       "N" [Anim]   []] -- Person
lexicon "kuma"      = [Cat "kuma"       "N" [Anim]   []] -- Bear
lexicon "neko"      = [Cat "neko"       "N" [Anim]   []] -- Cat
lexicon "eki"       = [Cat "eki"        "N" [Inanim] []] -- Station
lexicon "sensei"    = [Cat "sensei"     "N" [Anim]   []] -- Teacher
lexicon "mono"      = [Cat "mono"       "N" [Inanim] []] -- Thing
lexicon "yama"      = [Cat "yama"       "N" [Inanim] []] -- Mountain

-- Function words for nouns
lexicon "sann"   = [Cat "sann"   "CASE" [Voc, Resp, Thrd] []] -- vocative
lexicon "kunn"   = [Cat "kunn"   "CASE" [Voc, Thrd] []]
lexicon "chann"  = [Cat "chann"  "CASE" [Voc, Thrd, Unoff] []] 
lexicon "ga"     = [Cat "ga"     "CASE" [Nom] []] -- subjective
lexicon "wo"     = [Cat "wo"     "CASE" [Acc] []] -- objective
lexicon "no"     = [Cat "no"     "CASE" [Gen] []] -- genetive
lexicon "wa"     = [Cat "wa"     "CASE" [Top] []] -- topic
lexicon "ni"     = [Cat "ni"     "CASE" [Dat] [], -- dative (to SOMEONE)
                    Cat "ni"     "CASE" [Loc] []] -- locative (at, in)
lexicon "he"     = [Cat "he"     "CASE" [Alla] [],-- allative (to SOMEWHERE)
                    Cat "he"     "POST" [] []]
lexicon "kara"   = [Cat "kara"   "CASE" [Abl] []] -- ablative (from SOMEWHERE)
lexicon "de"     = [Cat "de"     "CASE" [Ins] [], -- instumental (with, by)
                    Cat "de"     "FIN"  [Te, Nadj] []] -- Te ending for N-adj
lexicon "yori"   = [Cat "yori"   "CASE" [Comp] []] 
lexicon "da"     = [Cat "da"     "CASE" [] [], -- declarative 
                    Cat "da"     "FIN"  [Pres, Neutr, Nadj] [], 
                    Cat "da"     "FIN"  [Ta, Neutr, Nadj] []]
lexicon "desu"   = [Cat "desu"   "CASE" [Poli] []] -- Copula

lexicon "to"     = [Cat "to"     "CONJ" [] []]
lexicon "made"   = [Cat "made"   "POST" [] []]
lexicon "madeni" = [Cat "madeni" "POST" [] []]


-- The cream of the crop: Verbs
-- godan verbs;
-- shin(u): die
-- koros(u): kill
-- ar(u): there exists (inanim)
-- ichdan verbs;
-- i(ru): there exists (anim)
-- mi(ru): look
-- tabe(ru): eat

-- Stems
lexicon "shin"  = [Cat "shin"   "V"   [Dan5, Stem] []]
lexicon "koros" = [Cat "koros"  "V"   [Dan5, Stem] []]
lexicon "ar"    = [Cat "ar"     "V"   [Dan5, Inanim, Stem] [],
                   Cat "ar"     "AUX" [Dan5, Inanim] []]

lexicon "i"     = [Cat "i"     "V"   [Dan1, Anim, Stem] [], 
                   Cat "i"     "V"   [Dan1, Nai, Anim] [], 
                   Cat "i"     "V"   [Dan1, Masu, Anim] [], 
                   Cat "i"     "V"   [Dan1, Te, Anim] [], 
                   Cat "i"     "V"   [Dan1, Ta, Anim] [], 
                   Cat "i"     "V"   [Dan1, Reru, Anim] [], 
                   Cat "i"     "V"   [Dan1, You, Anim] [], 
                   -- aux 'iru'
                   Cat "i"     "AUX"   [Dan1, Anim, Stem] [], 
                   Cat "i"     "AUX"   [Dan1, Nai, Anim] [], 
                   Cat "i"     "AUX"   [Dan1, Masu, Anim] [], 
                   Cat "i"     "AUX"   [Dan1, Te, Anim] [], 
                   Cat "i"     "AUX"   [Dan1, Ta, Anim] [], 
                   Cat "i"     "AUX"   [Dan1, Reru, Anim] [], 
                   Cat "i"     "AUX"   [Dan1, You, Anim] [], 
                   -- final for Iadj
                   Cat "i"    "FIN"  [Neutr, Iadj, Stem] [], 
                   -- 'i' dan inflections for Dan5
                   Cat "i"     "INF"  [Stem, Tfmasu, Dan5] [], 
                   Cat "i"     "INF"  [Stem, Tfte, Dan5] [],
                   Cat "i"     "INF"  [Stem, Tfta, Dan5] [], 
                   Cat "i"     "INF"  [Desu, Iadj] []] 
                  
lexicon "mi"    = [Cat "mi"     "V"   [Dan1, Nai] [], 
                   Cat "mi"     "V"   [Dan1, Stem] [], 
                   Cat "mi"     "V"   [Dan1, Masu] [], 
                   Cat "mi"     "V"   [Dan1, Te] [], 
                   Cat "mi"     "V"   [Dan1, Ta] [], 
                   Cat "mi"     "V"   [Dan1, Reru] [], 
                   Cat "mi"     "V"   [Dan1, You] []]
                  
lexicon "tabe"  = [Cat "tabe"     "V"   [Dan1, Stem] [], 
                   Cat "tabe"     "V"   [Dan1, Nai] [], 
                   Cat "tabe"     "V"   [Dan1, Masu] [], 
                   Cat "tabe"     "V"   [Dan1, Te] [], 
                   Cat "tabe"     "V"   [Dan1, Ta] [], 
                   Cat "tabe"     "V"   [Dan1, Reru] [], 
                   Cat "tabe"     "V"   [Dan1, You] []] 

-- I-Adj
lexicon "taka"  = [Cat "taka"  "V"  [Iadj, Stem] []]
lexicon "ama"   = [Cat "ama"   "V"  [Iadj, Stem] []]
lexicon "yo"    = [Cat "yo"    "V"  [Iadj, Stem] []]

-- Na-Adj
lexicon "kire"  = [Cat "kire"  "N"  [Nadj, Stem] []]
lexicon "suki"  = [Cat "suki"  "N"  [Nadj, Stem] []]

-- Verb Morphemes
lexicon "e"     = [Cat "e"     "INF"  [Stem, Tfreru, Dan5, Tpdan1] []] -- 'e' dan inf
lexicon "o"     = [Cat "o"     "INF"  [Stem, Tfyou, Dan5] []]          -- 'o' dan inf

-- more inflections for Dan5
-- need to dictinct Te form and Ta form because of I-adj
lexicon "a"     = [Cat "a"     "INF"  [Stem, Tfnai, Dan5] []] 
lexicon "t"     = [Cat "t"     "INF"  [Stem, Tfte, Dan5] [], 
                   Cat "t"     "INF"  [Stem, Tfta, Dan5] []] 
lexicon "k"     = [Cat "k"     "INF"  [Stem, Tfte, Dan5] [], 
                   Cat "k"     "INF"  [Stem, Tfta, Dan5] []] 
lexicon "p"     = [Cat "p"     "INF"  [Stem, Tfte, Dan5] [], 
                   Cat "p"     "INF"  [Stem, Tfta, Dan5] []] 
lexicon "nn"    = [Cat "nn"    "INF"  [Stem, Tfte, Dan5] [], 
                   Cat "nn"    "INF"  [Stem, Tfta, Dan5] []] 

-- more inflections for other than Dan5
lexicon "kak"   = [Cat "kak"   "INF"  [Stem, Tfta, Iadj] []] 
lexicon "dat"   = [Cat "dak"   "INF"  [Stem, Tfta, Nadj] []] 
lexicon "ku"    = [Cat "ku"    "INF"  [Stem, Tfnai, Iadj] [], 
                   Cat "ku"    "INF"  [Stem, Tfte, Iadj] []] 
lexicon "dewa"  = [Cat "dewa"  "INF"  [Stem, Tfnai, Nadj] []] 
lexicon "re"    = [Cat "re"    "INF"  [Reru, Dan1, May, Tfstem] [],
                   Cat "re"    "END"  [Nai, Pass, Tpdan1, Tfstem] []]  
lexicon "rare"  = [Cat "rare"  "INF"  [Reru, Dan1, May, Tfstem] []]

-- endings
-- nai : negative
-- seru: causative
-- reru: passive
-- ba: conditional
-- masu/desu: polite
lexicon "na"    = [Cat "na"    "END"  [Nai, Tfstem, Tpiadj, Nega] []]  
lexicon "nai"   = [Cat "nai"   "FIN"  [Nai, Nega, Tpiadj] []]  
lexicon "se"    = [Cat "ser"   "END"  [Nai, Caus, Tpdan1, Tfstem] []]  
lexicon "mas"   = [Cat "mas"   "END"  [Poli, Masu, Dan5, Tpdan5, Tfstem] [],
                   Cat "mas"   "END"  [Poli, Masu, Dan1, Tpdan5, Tfstem] []]
lexicon "des"   = [Cat "des"   "END"  [Poli, Desu, Iadj, Tfstem, Tpdan5] [], 
                   Cat "des"   "END"  [Poli, Desu, Nadj, Tfstem, Tpdan5] []]
lexicon "ba"    = [Cat "ba"    "END"  [Reru, Hypo] []]

-- finalizing ending - followed by verb/aux (compound verb) or nouns (relative)
-- u(N): pres, neutral
-- ta(N): past, neutral
-- you: let's ...
-- te(V): connecting verbs
lexicon "u"     = [Cat "u"      "FIN"  [Stem, Pres, Neutr, Dan5] [], -- 'u' dan inf
                   Cat "u"      "FIN"  [You, Dan5, Volit] []]    -- let's 'u'
lexicon "ru"    = [Cat "ru"     "FIN"  [Stem, Pres, Neutr, Dan1] []]
lexicon "te"    = [Cat "te"     "FIN"  [Te, Dan5] [], 
                   Cat "te"     "FIN"  [Te, Dan1] [], 
                   Cat "te"     "FIN"  [Te, Iadj] []]
lexicon "ta"    = [Cat "ta"     "FIN"  [Ta, Neutr, Past, Dan5] [], 
                   Cat "ta"     "FIN"  [Ta, Neutr, Past, Dan1] [], 
                   Cat "ta"     "FIN"  [Ta, Neutr, Past, Iadj] []]
lexicon "you"   = [Cat "you"    "FIN"  [You, Dan1, Volit] []]
lexicon "shyou" = [Cat "shyou"  "FIN"  [You, Dan1, Volit, Poli] []]
