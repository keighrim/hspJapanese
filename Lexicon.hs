module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = 
    -- gender should be simplified, number we don't need
    Fst | Snd | Thrd | Masc | Fem
    -- but need a lot cases and other kinds of particle

    -- these lists are from Wikipedia
    -- http://en.wikipedia.org/wiki/Japanese_particles
    -- Case markers 
    -- | NO | De | Ni | Wo | Ga | Ha | Mo | Kara | Made | Madeni
    -- が nominative の genitive を accusative に locative, dative へ ablative と で instumental から ablative より comparative
    | Nom | Gen | Acc | Dat | Loc | Abl | Comp | Voc | Ins | Top
    -- Need just a few prepositional (postpositional) particle. Note that case marking and postpositionals are pretty similar..
    | Only | Until | On | With | By
        -- We don't want a full fledged parser, keep this simple

    -- Parallel markers -- I've never seen such things
    -- か、の、や、に、と、やら、なり、だの

    -- Sentence ending particles
    -- か、の、や、な、わ、とも、かしら
    -- Interjectory particles (間投助詞 kantō-joshi?)
    -- さ、よ、ね
    | Decl | Intrg | Intrj | Imper | Hypo | Caus | Pass | Nega -- Conflict Neg P.hs line 700
    -- honorifics 
    | Poli | Resp | Humb | Neutr | Unoff
    -- need features for regular (non-honorific) and unofficial
    -- Adverbial verb endings - There are too many adverbials 
    -- not meaningful to give each of them a different, say, semantic feature
    -- maybe we need rough dictinctions eg> temporal, spacial, manner, ...
    -- ばかり、まで、だけ、ほど、くらい、など、なり、やら
    | Advb 
    -- But we need syntactic features
    | Godan | Idan | Irre     -- Verb types
    | Te | Nai | Masu               -- Ending types (morphological form)
        -- verb [Te] only combines ending [Te]

    -- Binding particles (係助詞 kakari-joshi?)
    -- は、も、こそ、でも、しか、さえ、だに
    -- Conjunctive particles (接続助詞 setsuzoku-joshi?)
    -- や、が、て、のに、ので、から、ところが、けれども
    -- I don't know how to interpret the above in English...

    | Pers  | Refl | Wh 
    -- tense and modal
    | Past  | Pres | Fut | May | Must

    -- misc features
    | Anim  | Inanim
    deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]

-- Should these have a CatLabel (see line 6 above) of NP or maybe Noun? They can 
-- have relative phrases/demonstrative pronouns e.g. "kono watashi" and 
-- "mizu wo nonde iru kare"
-- KRIM: let's just stick to NP, because they don't have determiners/articles
lexicon "watashi"   = [Cat "watashi"    "NP" [Anim, Fst, Poli]  []]
lexicon "boku"      = [Cat "boku"       "NP" [Anim, Fst]  []]
lexicon "anata"     = [Cat "anata"      "NP" [Anim, Snd, Poli]  []]
lexicon "kimi"      = [Cat "kimi"       "NP" [Anim, Snd]  []]
lexicon "kare"      = [Cat "kare"       "NP" [Anim, Thrd, Masc]  []]
lexicon "kanojo"    = [Cat "kanojo"     "NP" [Anim, Thrd, Fem]  []]
lexicon "kore"      = [Cat "kore"       "NP" [Inanim]    []]
lexicon "sore"      = [Cat "sore"       "NP" [Inanim]    []]
lexicon "are"       = [Cat "are"        "NP" [Inanim]    []]

{--
lexicon "i"   = [Cat "i"   "NP" [Pers,Fst,Sg,Nom]        []]
lexicon "me"  = [Cat "me"  "NP" [Pers,Fst,Sg,AccOrDat]   []]
lexicon "we"  = [Cat "we"  "NP" [Pers,Fst,Pl,Nom]        []]
lexicon "us"  = [Cat "us"  "NP" [Pers,Fst,Pl,AccOrDat]   []]
lexicon "you" = [Cat "you" "NP" [Pers,Snd]               []]
lexicon "he"  = [Cat "he"  "NP" [Pers,Thrd,Sg,Nom,Masc]  []]
lexicon "him" = [Cat "him" "NP" [Pers,Thrd,Sg,AccOrDat,Masc] 
                    []]
lexicon "she" = [Cat "she" "NP" [Pers,Thrd,Sg,Nom,Fem]   []]
lexicon "her" = [Cat "her" "NP" [Pers,Thrd,Sg,AccOrDat,Fem] 
                    []]
lexicon "it"  = [Cat "it"  "NP" [Pers,Thrd,Sg,Neutr]     []]
lexicon "they" = [Cat "they" "NP" [Pers,Thrd,Pl,Nom]     []]
lexicon "them" = [Cat "them" "NP" [Pers,Thrd,Pl,AccOrDat] 
                        []]
--}

-- Japanese doesn't really have reflexives
lexicon "jibun" = [Cat "jibun" "NP" [Anim] []]

{--
lexicon "myself"     = 
 [Cat "myself"     "NP" [Refl,Sg,Fst,AccOrDat] []]
lexicon "ourselves"  = 
 [Cat "ourselves"  "NP" [Refl,Pl,Fst,AccOrDat] []]
lexicon "yourself"   = 
 [Cat "yourself"   "NP" [Refl,Sg,Snd,AccOrDat] []]
lexicon "yourselves" = 
 [Cat "yourselves" "NP" [Refl,Pl,Snd,AccOrDat] []]
lexicon "himself"    = 
 [Cat "himself"    "NP" [Refl,Sg,Thrd,AccOrDat,Masc]  []]
lexicon "herself"    = 
 [Cat "herself"    "NP" [Refl,Sg,Thrd,AccOrDat,Fem]   []]
lexicon "itself"     = 
 [Cat "itself"     "NP" [Refl,Sg,Thrd,AccOrDat,Neutr] []]
lexicon "themselves" = 
 [Cat "themselves" "NP" [Refl,Pl,Thrd,AccOrDat] []]
 --}

-- No relative pronouns? Only word order
-- KRIM: you are right
{--
lexicon "who"     = [Cat "who" "NP"  [Wh,Thrd,MascOrFem] [], 
     Cat "who" "REL" [MascOrFem]         []]
lexicon "whom"    = 
 [Cat "whom" "NP"  [Sg,Wh,Thrd,AccOrDat,MascOrFem] [], 
  Cat "whom" "REL" [Sg,MascOrFem,AccOrDat]         []]
lexicon "what"    = 
 [Cat "what" "NP"  [Wh,Thrd,AccOrDat,Neutr]    []]
lexicon "that"    = [Cat "that"  "REL" []      [], 
                     Cat "that"  "DET" [Sg]    []]
lexicon "which"   = [Cat "which" "REL" [Neutr] [], 
                     Cat "which" "DET" [Wh]    []]
--}

-- Names just like normal nouns
-- Should we treat honorifics separately? Sounds like a hassle
-- KRIM: I guess we can take vocative cases separately and make them honorific using relavant features
lexicon "rim"       = [Cat "rim"        "NP" [Anim] []]
lexicon "curcuru"   = [Cat "curcuru"    "NP" [Anim] []]

{--
lexicon "snowwhite"    = 
 [Cat "snowwhite"  "NP" [Thrd,Fem,Sg]  []]
lexicon "alice"        = 
 [Cat "alice"      "NP" [Thrd,Fem,Sg]  []]
lexicon "dorothy"      = 
 [Cat "dorothy"    "NP" [Thrd,Fem,Sg]  []]
lexicon "goldilocks"   = 
 [Cat "goldilocks" "NP" [Thrd,Fem,Sg]  []]
lexicon "littlemook"   = 
 [Cat "littlemook" "NP" [Thrd,Masc,Sg] []]
lexicon "atreyu"       = 
 [Cat "atreyu"     "NP" [Thrd,Masc,Sg] []]
--}


-- Good old determiners. No features needed?
-- KRIM: I don't think so
lexicon "kono"    = [Cat "kono"    "DET" []    []]
lexicon "sono"    = [Cat "sono"    "DET" []    []]
lexicon "ano"     = [Cat "ano"     "DET" []    []]


{--
lexicon "every"   = [Cat "every"   "DET" [Sg]  []]
lexicon "all"     = [Cat "all"     "DET" [Pl]  []]
lexicon "some"    = [Cat "some"    "DET" []    []]
lexicon "several" = [Cat "several" "DET" [Pl]  []]
lexicon "a"       = [Cat "a"       "DET" [Sg]  []]
--lexicon "no"      = [Cat "no"      "DET" []    []]
lexicon "the"     = [Cat "the"     "DET" []    []]

lexicon "most"    = [Cat "most"    "DET" [Pl]  []]
lexicon "many"    = [Cat "many"    "DET" [Pl]  []]
lexicon "few"     = [Cat "few"     "DET" [Pl]  []]
lexicon "this"    = [Cat "this"    "DET" [Sg]  []]
lexicon "these"   = [Cat "these"   "DET" [Pl]  []]
lexicon "those"   = [Cat "those"   "DET" [Pl]  []]

lexicon "less_than" = [Cat "less_than" "DF" [Pl] []]
lexicon "more_than" = [Cat "more_than" "DF" [Pl] []]
--}


-- Nouns are cheesy easy. Do we need a CN distinction? Should pronouns and names
-- be CN? Or just N?
-- KRIM: My guess is that CN only needed to take DET to form NP.
-- Since Japanese don't have the same kind of determiners 
-- as English does, I bet on direct NP
lexicon "banana"    = [Cat "banana"     "NP" [Inanim] []]
lexicon "jitensha"  = [Cat "jitensha"   "NP" [Inanim] []]
lexicon "arubaito"  = [Cat "arubaito"   "NP" [Inanim] []]
lexicon "atama"     = [Cat "atama"      "NP" [Inanim] []]
lexicon "hito"      = [Cat "hito"       "NP" [Anim]   []]
lexicon "kuma"      = [Cat "kuma"       "NP" [Anim]   []]
lexicon "neko"      = [Cat "neko"       "NP" [Anim]   []]
lexicon "eki"       = [Cat "eki"        "NP" [Inanim] []]
lexicon "sensei"    = [Cat "sensei"     "NP" [Anim]   []]
lexicon "mono"      = [Cat "mono"       "NP" []   []]

{--
lexicon "thing"   = [Cat "thing"   "CN" [Sg,Neutr,Thrd] []]
lexicon "things"  = [Cat "things"  "CN" [Pl,Neutr,Thrd] []]
lexicon "person"  = [Cat "person"  "CN" [Sg,Masc,Thrd]  []]
lexicon "persons" = [Cat "persons" "CN" [Pl,Masc,Thrd]  []]
lexicon "boy"     = [Cat "boy"     "CN" [Sg,Masc,Thrd]  []]
lexicon "boys"    = [Cat "boys"    "CN" [Pl,Masc,Thrd]  []]
lexicon "man"     = [Cat "man"     "CN" [Sg,Masc,Thrd]  []]
lexicon "men"     = [Cat "men"     "CN" [Pl,Masc,Thrd]  []]
lexicon "girl"    = [Cat "girl"    "CN" [Sg,Fem,Thrd]   []]
lexicon "girls"   = [Cat "girls"   "CN" [Pl,Fem,Thrd]   []]
lexicon "woman"   = [Cat "woman"   "CN" [Sg,Fem,Thrd]   []]
lexicon "women"   = [Cat "women"   "CN" [Pl,Fem,Thrd]   []]
lexicon "princess" = [Cat "princess" "CN" [Sg,Fem,Thrd] []]
lexicon "princesses" = [Cat "princesses" "CN" [Pl,Fem,Thrd] []]
lexicon "dwarf"    = [Cat "dwarf"    "CN" [Sg,Masc,Thrd] []]
lexicon "dwarfs"   = [Cat "dwarfs"   "CN" [Pl,Masc,Thrd] []]
lexicon "dwarves"  = [Cat "dwarves"  "CN" [Pl,Masc,Thrd] []]
lexicon "giant"    = [Cat "giant"    "CN" [Sg,Masc,Thrd] []]
lexicon "giants"   = [Cat "giants"   "CN" [Pl,Masc,Thrd] []]

lexicon "wizard"   = [Cat "wizard"   "CN" [Sg,Masc,Thrd]  []]
lexicon "wizards"  = [Cat "wizards"  "CN" [Pl,Masc,Thrd]  []]
lexicon "sword"    = [Cat "sword"    "CN" [Sg,Neutr,Thrd] []]
lexicon "swords"   = [Cat "swords"   "CN" [Pl,Neutr,Thrd] []]
lexicon "dagger"   = [Cat "dagger"   "CN" [Sg,Neutr,Thrd] []]
lexicon "daggers"  = [Cat "daggers"  "CN" [Pl,Neutr,Thrd] []]
--}


-- Auxen will need some srs consideration
-- KRIM: Absolutely. But as said earlier, let's keep it simple
-- I'm rather thing of like this;
lexicon "i"      = [Cat "i"      "VP"  [Idan, Anim] []] -- stem
lexicon "a"      = [Cat "a"      "VP"  [Idan, Inanim] []]
lexicon "aru"    = [Cat "aru"    "VP"  [Idan, Inanim, Neutr, Decl, Pres] []]
lexicon "i"      = [Cat "i"      "AUX" [Idan, Anim] []]
lexicon "a"      = [Cat "a"      "AUX" [Idan, Inanim] []]
lexicon "aru"    = [Cat "aru"    "AUX" [Idan, Inanim, Neutr, Decl, Pres] []]
lexicon "mas"    = [Cat "mas"    "END" [Poli] []]
lexicon "masa"   = [Cat "mas"    "END" [Nai, Poli] []]
lexicon "u"      = [Cat "u"      "FIN" [Decl, Pres] []] -- Finalizing ending
lexicon "ru"     = [Cat "ru"     "FIN" [Decl, Pres] []]
lexicon "ta"     = [Cat "ta"     "FIN" [Te, Decl, Past] []]
lexicon "ita"    = [Cat "ita"    "FIN" [Te, Decl, Past] []]
-- then combining "i" + "mas" + "ita" makes VP [Anim, Poli, Decl, Past]

{--
lexicon "imasu"      = [Cat "imasu"      "AUX" [Pres] []]
lexicon "imashita"   = [Cat "imashita"   "AUX" [Past] []]
lexicon "itte"       = [Cat "itte"       "AUX" [Te]   []]
lexicon "arimasu"    = [Cat "arimasu"    "AUX" [Pres] []]
lexicon "arimashita" = [Cat "arimashita" "AUX" [Past] []]
lexicon "atte"       = [Cat "atte"       "AUX" [Te]   []]
lexicon "okimasu"    = [Cat "okimasu"    "AUX" [Pres] []]
lexicon "okimashita" = [Cat "okimashita" "AUX" [Past] []]
lexicon "oite"       = [Cat "oite"       "AUX" [Te]   []]
--}

{--
lexicon "did"    = [Cat "did"    "AUX" [] []]
lexicon "didn't" = [Cat "didn't" "AUX" [] []]
--}


-- Function words. Should probably change the tag from PREP to POST and/or
-- PART
-- What exactly uses the CatLabel? I think it might be only in lexicon entries
-- Like verbs below
-- "No" conflicts with FSynF.hs line 33, used "NO" for now.

-- KRIM: Would this be better? (Instead of using word itself as Feature)
-- Case markers
lexicon "san"  = [Cat "san"  "CASE" [Voc, Resp, Thrd] []] -- vocative
lexicon "kun"  = [Cat "kun"  "CASE" [Voc, Thrd] []]
lexicon "chan" = [Cat "chan" "CASE" [Voc, Thrd, Unoff] []] 
lexicon "ga"   = [Cat "ga"   "CASE" [Nom] []] -- subjective
lexicon "wo"   = [Cat "wo"   "CASE" [Acc] []] -- objective
lexicon "no"   = [Cat "no"   "CASE" [Gen] []] -- genetive
lexicon "wa"   = [Cat "wa"   "CASE" [Top] []] -- topic
lexicon "ni"   = [Cat "ni"   "CASE" [Dat] []] -- dative (to SOMEONE)
lexicon "ni"   = [Cat "ni"   "CASE" [Loc] []] -- locative (at, in)
lexicon "ni"   = [Cat "ni"   "CASE" [Abl] []] -- ablative (to SOMEWHERE)
lexicon "kara" = [Cat "kara" "CASE" [Abl] []] -- ablative (from SOMEWHERE)
lexicon "de"   = [Cat "de"   "CASE" [Ins] []] -- instumental (with, by)
lexicon "yori" = [Cat "yori" "CASE" [Comp] []] 
lexicon "da"   = [Cat "da"   "CASE" [Decl] []] -- declarative 
lexicon "desu" = [Cat "desu" "CASE" [Decl, Poli] []] 

{--
lexicon "no"    = [Cat "no"     "PREP" [NO] []]
lexicon "de"    = [Cat "de"     "PREP" [De] []]
lexicon "ni"    = [Cat "ni"     "PREP" [Ni] []]
lexicon "wo"    = [Cat "wo"     "PREP" [Wo] []]
lexicon "ga"    = [Cat "ga"     "PREP" [Ga] []]
lexicon "ha"    = [Cat "ha"     "PREP" [Ha] []]
lexicon "mo"    = [Cat "mo"     "PREP" [Mo] []]
lexicon "kara"  = [Cat "kara"   "PREP" [Kara] []]
lexicon "made"  = [Cat "made"   "PREP" [Made] []]
lexicon "madeni" = [Cat "madeni" "PREP" [Madeni] []]
--}

-- KRIM: of course we need a separate tag like "POST" as you suggested
-- not sure what kinds of features these should take
lexicon "to"     = [Cat "to"     "CONJ" [] []]
lexicon "e"      = [Cat "e"      "POST" [] []]
lexicon "made"   = [Cat "made"   "POST" [] []]
lexicon "madeni" = [Cat "madeni" "POST" [] []]

{--
lexicon "on"   = [Cat "on"   "PREP" [On]   []]
lexicon "with" = [Cat "with" "PREP" [With] []]
lexicon "by"   = [Cat "by"   "PREP" [By]   []]
--lexicon "to"   = [Cat "to"   "PREP" [To]   []]
lexicon "from" = [Cat "from" "PREP" [From] []]

lexicon "and"   = [Cat "and"  "CONJ" [] []]
lexicon "."     = [Cat "."    "CONJ" [] []]
lexicon "if"    = [Cat "if"   "COND" [] []]
lexicon "then"  = [Cat "then" "THEN" [] []]
--}


-- The cream of the crop: Verbs
-- Is this the kind of structure we want to do with particles and nouns?
-- Should the NPs need any features here? In the English examples it's AccOrDat,
-- but we are selecting whether it is Acc or Dat, etc. through the use of particles
--
-- KRIM: I guess we don't need verb-subcategorization, because Japanese has
-- fairly free word-orders and in many cases anything can be ommitted.
-- Instead we'd better focus on AUX, END, and FIN and their feature unification
-- each verb has different forms:
-- there are options to represent thses forms
lexicon "shi"      = [Cat "shi"      "VP"  [Godan] []] 
lexicon "shina"    = [Cat "shina"    "VP"  [Godan, Nai] []] 
lexicon "shini"    = [Cat "shini"    "VP"  [Godan, Masu] []] 
lexicon "shinu"    = [Cat "shinu"    "VP"  [Godan, Decl, Pres] []] 
lexicon "shine"    = [Cat "shine"    "VP"  [Godan, Imper] []] 
lexicon "shine"    = [Cat "shine"    "VP"  [Idan, May] []] 
lexicon "shineru"  = [Cat "shineru"  "VP"  [Idan, Neutr, May, Decl] []] 
lexicon "shinou"   = [Cat "shinou"   "VP"  [Godan, Neutr, Imper] []] 
lexicon "shinn"    = [Cat "shinn"    "VP"  [Godan, Te] []] 
-- or below is a more compositional way
lexicon "shin"  = [Cat "shi"    "VP"   [Godan] []] -- keep lexicons of stem only
lexicon "a"     = [Cat "a"      "END"  [Godan, Nai] []] 
lexicon "i"     = [Cat "i"      "END"  [Godan, Masu] []] 
lexicon "e"     = [Cat "e"      "FIN"  [Godan, Neutr, Imper] []] 
-- ending 'e' transforms godan verbs into shimo 1 dan. how can we represent this?
lexicon "e"     = [Cat "e"      "END"  [Godan, Idan, Neutr, May] []] 
lexicon "ou"    = [Cat "ou"     "FIN"  [Godan, Neutr, Imper] []] 
lexicon "ouka"  = [Cat "ouka"   "FIN"  [Godan, Neutr, Imper] []] 

-- kami/shimo 1 dan - much easier
lexicon "mi"       = [Cat "mi"       "VP"  [Idan] []] -- stem
-- 1 dan verbs rarely inflects
lexicon "mi"       = [Cat "mi"       "VP"  [Idan, Nai] []] 
lexicon "mi"       = [Cat "mi"       "VP"  [Idan, Masu] []] 
lexicon "mi"       = [Cat "mi"       "VP"  [Idan, Te] []] 
lexicon "miru"     = [Cat "miru"     "VP"  [Idan, Neutr, Decl, Pres] []] 

-- need to handle irregular 'kuru', 'suru' here

-- endings
lexicon "reru"    = [Cat "reru"     "FIN"  [Idan, Neutr, May] []] 
lexicon "rareru"  = [Cat "rareru"   "FIN"  [Idan, Neutr, May] []] 
lexicon "you"     = [Cat "you"      "FIN"  [Idan, Neutr, Imper] []] 
lexicon "nai"     = [Cat "nai"      "END"  [Nai, Nega] []] -- Do we need Nai and Negative?
lexicon "masu"    = [Cat "masu"     "FIN"  [Masu] []] 
lexicon "te"      = [Cat "te"       "FIN"  [Te] []] 
lexicon "reru"    = [Cat "reru"     "FIN"  [Nai, Godan, Pass] []] -- Nai?
lexicon "seru"    = [Cat "seru"     "FIN"  [Nai, Godan, Caus] []] -- Nai?



{--
lexicon "shinimasu" = 
    [Cat "shinimasu" "VP" [Pres] [],
     Cat "shinimasu" "VP" [Pres] [Cat "_" "NP" [Anim] [],
                                  Cat "_" "PREP" [Ga] []]]
lexicon "shinimashita" = 
    [Cat "shinimashita" "VP" [Past] [],
     Cat "shinimashita" "VP" [Past] [Cat "_" "NP" [Anim] [],
                                     Cat "_" "PREP" [Ga] []]]
lexicon "shinde" = 
    [Cat "shinde" "VP" [Te] [],
     Cat "shinde" "VP" [Te] [Cat "_" "NP" [Anim] [],
                             Cat "_" "PREP" [Ga] []]]
                             
lexicon "koroshimasu" =
    [Cat "koroshimasu" "VP" [Pres] [Cat "_" "NP" [Anim] [],
                                    Cat "_" "PREP" [Wo] []],
     Cat "koroshimasu" "VP" [Pres] [Cat "_" "NP" [Anim] [],
                                    Cat "_" "PREP" [Wo] [],
                                    Cat "_" "NP" [Inanim] [],
                                    Cat "_" "PREP" [De] []]]
lexicon "koroshimashita" =
    [Cat "koroshimashita" "VP" [Past] [Cat "_" "NP" [Anim] [],
                                       Cat "_" "PREP" [Wo] []],
     Cat "koroshimashita" "VP" [Past] [Cat "_" "NP" [Anim] [],
                                       Cat "_" "PREP" [Wo] [],
                                       Cat "_" "NP" [Inanim] [],
                                       Cat "_" "PREP" [De] []]]
lexicon "koroshite" =
    [Cat "koroshite" "VP" [Te] [Cat "_" "NP" [Anim] [],
                                Cat "_" "PREP" [Wo] []],
     Cat "koroshite" "VP" [Te] [Cat "_" "NP" [Anim] [],
                                Cat "_" "PREP" [Wo] [],
                                Cat "_" "NP" [Inanim] [],
                                Cat "_" "PREP" [De] []]]
--}
                                

{--
lexicon "smiled"    = [Cat "smiled"    "VP" [Past] []]
lexicon "smile"     = [Cat "smile"     "VP" [Infl]  []]
lexicon "laughed"   = [Cat "laughed"   "VP" [Past] []]
lexicon "laugh"     = [Cat "laugh"     "VP" [Infl]  []]
lexicon "cheered"   = [Cat "cheered"   "VP" [Past] []]
lexicon "cheer"     = [Cat "cheer"     "VP" [Infl]  []]
lexicon "shuddered" = [Cat "shuddered" "VP" [Past] []]
lexicon "shudder"   = [Cat "shudder"   "VP" [Infl]  []]

lexicon "loved"        = 
 [Cat "loved"    "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "love"         = 
 [Cat "love"     "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "admired"      = 
 [Cat "admired"  "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "admire"       = 
 [Cat "admire"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "helped"       = 
 [Cat "helped"   "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "help"         = 
 [Cat "help"     "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeated"       = 
 [Cat "defeated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeat"         = 
 [Cat "defeat"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "gave"         = 
 [Cat "gave" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                    Cat "_" "PP" [To]       []], 
  Cat "gave" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                     Cat "_" "NP" [AccOrDat]  []]]
lexicon "give"         = 
 [Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]
lexicon "sold" = 
 [Cat "sold" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "sold" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]
lexicon "sell" = 
 [Cat "sell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "sell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]

lexicon "kicked" = 
 [Cat "kicked" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                             Cat "_" "PP" [With]     []], 
  Cat "kicked" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "kick" = 
 [Cat "kick"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                      Cat "_" "PP" [With]     []], 
  Cat "kick"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 

lexicon "took" = 
 [Cat "took" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                    Cat "_" "PP" [From]     []], 
  Cat "took" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "take" = 
 [Cat "take" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [From]     []], 
  Cat "take" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 

lexicon "shouted"    = [Cat "shouted"    "VP" [Past] []]
lexicon "shout"    = [Cat "shout"    "VP" [Pres,Sg,Fst] [],
  Cat "shout"    "VP" [Pres,Sg,Snd] [],
  Cat "shout"     "VP" [Pres,Pl]  [],
  Cat "shout"     "VP" [Infl]  []]
lexicon "shouts"    = [Cat "shouts"    "VP" [Pres,Sg,Thrd] []]
lexicon "will_shout"    = [Cat "will_shout"    "VP" [Fut] []]
lexicon "have_shouted"  = [Cat "have_shouted"  "VP" [Perf,Sg,Fst] [],
  Cat "have_shouted"     "VP" [Perf,Sg,Snd] [],
  Cat "have_shouted"     "VP" [Perf,Pl] []]
lexicon "has_shouted"   = [Cat "has_shouted"   "VP" [Perf,Sg,Thrd] []]

lexicon _ = []
--}
