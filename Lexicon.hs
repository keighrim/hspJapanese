module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = 
    -- gender should be simplified, numbers we don't need
    Fst | Snd | Thrd | Masc | Fem
    -- but need a lot cases and other kinds of particle

    -- lists from Wikipedia http://en.wikipedia.org/wiki/Japanese_particles
    -- Case markers 
    -- | NO | De | Ni | Wo | Ga | Ha | Mo | Kara | Made | Madeni
    -- が nominative の genitive を accusative に locative, dative 
    -- へ allative と で instumental から ablative より comparative
    | Nom | Gen | Acc | Dat | Loc | Alla | Abl | Comp | Voc | Ins | Top
    -- Need just a few prepositional (postpositional) particle.
    -- Note that case marking and postpositionals are pretty similar..
    | Only | Until | On | With | By
    -- We don't want a full fledged parser, keep this simple

    -- Sentence ending particles
    -- か、の、や、な、わ、とも、かしら
    -- Interjectory particles (間投助詞 kantō-joshi?)
    -- さ、よ、ね
    | Decl | Intrg | Intrj | Imper | Volit
    | Hypo | Caus | Pass | Nega -- Conflict Neg P.hs line 700

    -- honorifics 
    | Poli | Resp | Humb | Neutr | Unoff
    -- need features for regular (non-honorific) and unofficial
    -- Adverbial verb endings - There are too many adverbials 
    -- not meaningful to give each of them a different, say, semantic feature
    -- maybe we need rough dictinctions eg> temporal, spacial, manner, ...
    -- ばかり、まで、だけ、ほど、くらい、など、なり、やら
    | Advb 
    -- But we need syntactic features
    | Dan5 | Dan1 | Irre | Iadj | Nadj         -- Verb types
    | Stem | Te | Ta | Nai | Masu | Desu | Reru | You -- Ending types (morphological form)
    -- eg> verb [Te] only combines ending [Te]

    -- Parallel markers -- I've never seen such things
    -- か、の、や、に、と、やら、なり、だの
    -- Binding particles (係助詞 kakari-joshi?)
    -- は、も、こそ、でも、しか、さえ、だに
    -- Conjunctive particles (接続助詞 setsuzoku-joshi?)
    -- や、が、て、のに、ので、から、ところが、けれども
    -- I don't know how to interpret the above in English...

    | Pers  | Refl | Wh 
    -- tense and modal
    | Past  | Pres | Fut | May | Must
    -- Transformation features
    | Tpiadj | Tpdan5 | Tpdan1
    | Mddecl | Mdintrg
    | Tfstem | Tfnai | Tfte | Tfta | Tfmasu | Tfreru | Tfyou
    -- TpXXX assigns type XXX to combined VP 
    -- similarly, MdXXX assigns mood XXX to combined VP 

    -- misc features
    | Anim  | Inanim
    deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]

-- Should these have a CatLabel (see line 6 above) of NP or maybe Noun? They can 
-- have relative phrases/demonstrative pronouns e.g. "kono watashi" and 
-- "mizu wo nonde iru kare"
-- KRIM: let's just stick to NP, because they don't have determiners/articles
lexicon "watashi"   = [Cat "watashi"    "N" [Anim, Fst, Poli]  []]
lexicon "boku"      = [Cat "boku"       "N" [Anim, Fst]  []]
lexicon "anata"     = [Cat "anata"      "N" [Anim, Snd, Poli]  []]
lexicon "kimi"      = [Cat "kimi"       "N" [Anim, Snd]  []]
lexicon "kare"      = [Cat "kare"       "N" [Anim, Thrd, Masc]  []]
lexicon "kanojo"    = [Cat "kanojo"     "N" [Anim, Thrd, Fem]  []]
lexicon "kore"      = [Cat "kore"       "N" [Inanim]    []]
lexicon "sore"      = [Cat "sore"       "N" [Inanim]    []]
lexicon "are"       = [Cat "are"        "N" [Inanim]    []]
lexicon "koko"      = [Cat "koko"       "N" [Inanim]    []]
lexicon "soko"      = [Cat "soko"       "N" [Inanim]    []]
lexicon "asoko"     = [Cat "asoko"      "N" [Inanim]    []]
-- Japanese doesn't really have reflexives
lexicon "jibun" = [Cat "jibun" "N" [Anim] []]

-- Names just like normal nouns
-- Should we treat honorifics separately? Sounds like a hassle
-- KRIM: I guess we can take vocative cases separately and make them honorific using relavant features
lexicon "rim"           = [Cat "rim"          "N" [Anim] []]
lexicon "curcuru"       = [Cat "curcuru"      "N" [Anim] []]
lexicon "arisu"         = [Cat "arisu"        "N" [Anim] []]  -- alice
lexicon "doroshi"       = [Cat "doroshi"      "N" [Anim] []]  -- dorothy
lexicon "sunouhowaito"  = [Cat "sunouhowaito" "N" [Anim] []]  -- snowwhite

-- Good old determiners. No features needed?
-- KRIM: I don't think so. Should we keep DET tag? or just ADJ
lexicon "kono"    = [Cat "kono"    "DET" []    []]
lexicon "sono"    = [Cat "sono"    "DET" []    []]
lexicon "ano"     = [Cat "ano"     "DET" []    []]

-- Nouns are cheesy easy. Do we need a CN distinction? Should pronouns and names
-- be CN? Or just N?
-- KRIM: My guess is that CN only needed to take DET to form NP.
-- Since Japanese don't have the same kind of determiners 
-- as English does, I bet on direct NP
lexicon "banana"    = [Cat "banana"     "N" [Inanim] []]
lexicon "jitensha"  = [Cat "jitensha"   "N" [Inanim] []]
lexicon "arubaito"  = [Cat "arubaito"   "N" [Inanim] []]
lexicon "atama"     = [Cat "atama"      "N" [Inanim] []]
lexicon "hito"      = [Cat "hito"       "N" [Anim]   []]
lexicon "kuma"      = [Cat "kuma"       "N" [Anim]   []]
lexicon "neko"      = [Cat "neko"       "N" [Anim]   []]
lexicon "eki"       = [Cat "eki"        "N" [Inanim] []]
lexicon "sensei"    = [Cat "sensei"     "N" [Anim]   []]
lexicon "mono"      = [Cat "mono"       "N" []   []]
lexicon "yama"      = [Cat "yama"       "N" []   []]

-- Function words for nouns
lexicon "sann"  = [Cat "sann"  "CASE" [Voc, Resp, Thrd] []] -- vocative
lexicon "kunn"  = [Cat "kunn"  "CASE" [Voc, Thrd] []]
lexicon "chann" = [Cat "chann" "CASE" [Voc, Thrd, Unoff] []] 
lexicon "ga"   = [Cat "ga"   "CASE" [Nom] []] -- subjective
lexicon "wo"   = [Cat "wo"   "CASE" [Acc] []] -- objective
lexicon "no"   = [Cat "no"   "CASE" [Gen] []] -- genetive
lexicon "wa"   = [Cat "wa"   "CASE" [Top] []] -- topic
lexicon "ni"   = [Cat "ni"   "CASE" [Dat] [], -- dative (to SOMEONE)
                  Cat "ni"   "CASE" [Loc] []] -- locative (at, in)
lexicon "he"   = [Cat "he"   "CASE" [Alla] [],-- allative (to SOMEWHERE)
                  Cat "he"   "POST" [] []]
lexicon "kara" = [Cat "kara" "CASE" [Abl] []] -- ablative (from SOMEWHERE)
lexicon "de"   = [Cat "de"   "CASE" [Ins] [], -- instumental (with, by)
                  Cat "de"     "FIN"  [Te, Nadj] []] -- Te ending for N-adj
lexicon "yori" = [Cat "yori" "CASE" [Comp] []] 
lexicon "da"   = [Cat "da"   "CASE" [Decl] [], -- declarative 
                  Cat "da"    "FIN"  [Pres, Neutr, Decl, Nadj] [], 
                  Cat "da"     "FIN"  [Ta, Neutr, Decl, Nadj] []]
lexicon "desu" = [Cat "desu" "CASE" [Decl, Poli] []] 

lexicon "to"     = [Cat "to"     "CONJ" [] []]
lexicon "made"   = [Cat "made"   "POST" [] []]
lexicon "madeni" = [Cat "madeni" "POST" [] []]


-- The cream of the crop: Verbs
-- godan verbs;
-- shin(u): die
-- ar(u): there exists (inanim)
-- ichdan verbs;
-- i(ru): there exists (anim)
-- mi(ru): look
-- tabe(ru): eat
lexicon "shin"  = [Cat "shin"   "V"   [Dan5, Stem] []] -- keep lexicons of stem only
lexicon "koros" = [Cat "koros"  "V"   [Dan5, Stem] []]
lexicon "ar" = [Cat "ar"  "V"   [Dan5, Inanim, Stem] [],
                Cat "ar"  "AUX"   [Dan5, Inanim] []]

lexicon "i"    = [Cat "i"     "V"   [Dan1, Anim, Stem] [], 
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
                  Cat "i"    "FIN"  [Neutr, Decl, Iadj, Stem] [], 
                  -- 'i' dan inflections for Dan5
                  Cat "i"     "INF"  [Masu, Dan5] [], 
                  Cat "i"     "INF"  [Te, Dan5] [],
                  Cat "i"     "INF"  [Ta, Dan5] [], 
                  Cat "i"     "INF"  [Desu, Iadj] []] 
lexicon "mi"    = 
                   [Cat "mi"     "V"   [Dan1, Nai] [], 
                   Cat "mi"     "V"   [Dan1, Stem] [], 
                   Cat "mi"     "V"   [Dan1, Masu] [], 
                   Cat "mi"     "V"   [Dan1, Te] [], 
                   Cat "mi"     "V"   [Dan1, Ta] [], 
                   Cat "mi"     "V"   [Dan1, Reru] [], 
                   Cat "mi"     "V"   [Dan1, You] []]
lexicon "tabe"    = [Cat "tabe"     "V"   [Dan1, Stem] [], 
                     Cat "tabe"     "V"   [Dan1, Nai] [], 
                     Cat "tabe"     "V"   [Dan1, Masu] [], 
                     Cat "tabe"     "V"   [Dan1, Te] [], 
                     Cat "tabe"     "V"   [Dan1, Ta] [], 
                     Cat "tabe"     "V"   [Dan1, Reru] [], 
                     Cat "tabe"     "V"   [Dan1, You] []] 

lexicon "taka"  = [Cat "taka"  "V"  [Iadj, Stem] []]
lexicon "ama"  = [Cat "ama"  "V"  [Iadj, Stem] []]

lexicon "kire"  = [Cat "kire"  "N"  [Nadj, Stem] []]
lexicon "suki"  = [Cat "suki"  "N"  [Nadj, Stem] []]
-- need to handle irregular 'kuru', 'suru' here - hardcoding is the best way

-- then combining "i" + "mas" + "ita" makes VP [Anim, Poli, Decl, Past]

lexicon "e"     = [Cat "e"     "INF"  [Stem, Tfreru, Dan5, Tpdan1] []] -- 'e' dan inf
lexicon "o"     = [Cat "o"     "INF"  [Stem, Tfyou, Dan5] []]          -- 'o' dan inf

-- more inflections for Dan5
-- need to dictinct Te form and Ta form because of I-adj
lexicon "a" = [ Cat "a"     "INF"  [Stem, Tfnai, Dan5] []] 
lexicon "t" = [Cat "t"     "INF"  [Stem, Tfte, Dan5] [], 
                   Cat "t"     "INF"  [Stem, Tfta, Dan5] []] 
lexicon "k" = [Cat "k"     "INF"  [Stem, Tfte, Dan5] [], 
                   Cat "k"     "INF"  [Stem, Tfta, Dan5] []] 
lexicon "p" = [Cat "p"     "INF"  [Stem, Tfte, Dan5] [], 
                   Cat "p"     "INF"  [Stem, Tfta, Dan5] []] 
lexicon "nn"= [Cat "nn"     "INF"  [Stem, Tfte, Dan5] [], 
                    Cat "nn"     "INF"  [Stem, Tfta, Dan5] []] 

-- more inflections for other than Dan5
lexicon "kak" = [Cat "kak"     "INF"  [Stem, Tfta, Iadj] []] 
lexicon "dat" = [Cat "dak"     "INF"  [Stem, Tfta, Nadj] []] 
lexicon "ku"  = [Cat "ku"     "INF"  [Stem, Tfnai, Iadj] [], 
                    Cat "ku"     "INF"  [Stem, Tfte, Iadj] []] 
lexicon "dewa"= [Cat "dewa"     "INF"  [Stem, Tfnai, Nadj] []] 
lexicon "re"  = [Cat "re"    "INF"  [Reru, Dan1, May, Tfstem] [],
                    Cat "re"     "END"  [Nai, Pass, Tpdan1, Tfstem] []]  
lexicon "rare"= [Cat "rare"     "INF"  [Reru, Dan1, May, Tfstem] []]
-- need imperative 'e'[Dan5] and 'ro'[Dan1]

-- endings
-- nai : negative
-- seru: causative
-- reru: passive
-- ba: conditional
-- masu/desu: polite
lexicon "na"     = [Cat "na"     "END"  [Nai, Nega, Tpiadj, Tfstem] []]  
lexicon "nai"     = [Cat "nai"     "FIN"  [Nai, Nega, Tpiadj] []]  
lexicon "se"     = [Cat "ser"     "END"  [Nai, Caus, Tpdan1, Tfstem] []]  

lexicon "mas"    = [Cat "mas"    "END"  [Poli, Masu, Dan5, Tpdan5, Tfstem] [],
                    Cat "mas"    "END"  [Poli, Masu, Dan1, Tpdan5, Tfstem] []]
lexicon "des"    = [Cat "des"    "END"  [Poli, Desu, Iadj, Tfstem, Tpdan5] [], 
                    Cat "des"    "END"  [Poli, Desu, Nadj, Tfstem, Tpdan5] []]
lexicon "ba"     = [Cat "ba"     "END"  [Reru, Hypo] []]
-- how to implement 'na' for Na-adj

-- finalizing ending - followed by verb/aux (compound verb) or nouns (relative)
-- u(N): pres, neutral
-- ta(N): past, neutral
-- you: let's ...
-- te(V): connecting verbs
lexicon "u"  = [Cat "u"  "FIN"  [Stem, Pres, Neutr, Decl, Dan5] [], -- 'u' dan inf
                Cat "u"     "FIN"  [You, Dan5, Volit] []]    -- let's 'u'
                -- TODO: need a mark for COMPLETELY FINALIZED
lexicon "ru"    = [Cat "ru"    "FIN"  [Stem, Pres, Neutr, Decl, Dan1] []]
lexicon "te"     = [Cat "te"     "FIN"  [Te, Dan5] [], 
                    Cat "te"     "FIN"  [Te, Dan1] [], 
                    Cat "te"     "FIN"  [Te, Iadj] []]
lexicon "ta"     = [Cat "ta"     "FIN"  [Ta, Neutr, Past, Dan5] [], 
                    Cat "ta"     "FIN"  [Ta, Neutr, Past, Dan1] [], 
                    Cat "ta"     "FIN"  [Ta, Neutr, Past, Iadj] []]
lexicon "you"     = [Cat "you"     "FIN"  [You, Dan1, Volit] []]
lexicon "shyou"     = [Cat "shyou"     "FIN"  [You, Dan1, Volit, Poli] []]
