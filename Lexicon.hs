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
    -- Need just a few prepositional (postpositional) particle. Note that case marking and postpositionals are pretty similar..
    | Only | Until | On | With | By
    -- We don't want a full fledged parser, keep this simple

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
    | 5dan | 1dan | Irre | Iadj | Nadj           -- Verb types
    | Te | Nai | Masu               -- Ending types (morphological form)
    -- verb [Te] only combines ending [Te]

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
-- Japanese doesn't really have reflexives
lexicon "jibun" = [Cat "jibun" "NP" [Anim] []]

-- Names just like normal nouns
-- Should we treat honorifics separately? Sounds like a hassle
-- KRIM: I guess we can take vocative cases separately and make them honorific using relavant features
lexicon "rim"           = [Cat "rim"          "NP" [Anim] []]
lexicon "curcuru"       = [Cat "curcuru"      "NP" [Anim] []]
lexicon "arice"         = [Cat "arice"        "NP" [Anim] []]  -- alice
lexicon "doroshi"       = [Cat "doroshi"      "NP" [Anim] []]  -- dorothy
lexicon "sunouhowaito"  = [Cat "sunouhowaito" "NP" [Anim] []]  -- snowwhite

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
lexicon "ni"   = [Cat "ni"   "CASE" [Dat] [], -- dative (to SOMEONE)
                  Cat "ni"   "CASE" [Loc] []] -- locative (at, in)
lexicon "he"   = [Cat "he"   "CASE" [Alla] [],-- allative (to SOMEWHERE)
                  Cat "he"   "POST" [] []]
lexicon "kara" = [Cat "kara" "CASE" [Abl] []] -- ablative (from SOMEWHERE)
lexicon "de"   = [Cat "de"   "CASE" [Ins] []] -- instumental (with, by)
lexicon "yori" = [Cat "yori" "CASE" [Comp] []] 
lexicon "da"   = [Cat "da"   "CASE" [Decl] []] -- declarative 
lexicon "desu" = [Cat "desu" "CASE" [Decl, Poli] []] 

lexicon "to"     = [Cat "to"     "CONJ" [] []]
lexicon "made"   = [Cat "made"   "POST" [] []]
lexicon "madeni" = [Cat "madeni" "POST" [] []]


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
{--
lexicon "shi"      = [Cat "shi"      "VP"  [5dan] []] 
lexicon "shina"    = [Cat "shina"    "VP"  [5dan, Nai] []] 
lexicon "shini"    = [Cat "shini"    "VP"  [5dan, Masu] []] 
lexicon "shinu"    = [Cat "shinu"    "VP"  [5dan, Decl, Pres] []] 
lexicon "shine"    = [Cat "shine"    "VP"  [5dan, Imper] []] 
lexicon "shine"    = [Cat "shine"    "VP"  [1dan, May] []] 
lexicon "shineru"  = [Cat "shineru"  "VP"  [1dan, Neutr, May, Decl] []] 
lexicon "shinou"   = [Cat "shinou"   "VP"  [5dan, Neutr, Imper] []] 
lexicon "shinn"    = [Cat "shinn"    "VP"  [5dan, Te] []] 
--}

-- kami/shimo 1 dan - much easier
-- 1 dan verbs rarely inflects
{--
lexicon "mi"       = [Cat "mi"       "VP"  [1dan, Nai] []] 
lexicon "mi"       = [Cat "mi"       "VP"  [1dan, Masu] []] 
lexicon "mi"       = [Cat "mi"       "VP"  [1dan, Te] []] 
lexicon "miru"     = [Cat "miru"     "VP"  [1dan, Neutr, Decl, Pres] []] 
--}

-- or below is a more compositional way
lexicon "shin"  = [Cat "shin"    "VP"   [5dan] []] -- keep lexicons of stem only
lexicon "koros" = [Cat "koros"  "VP"   [5dan] []]
lexicon "tabe"  = [Cat "tabe"   "VP"   [5dan] []]
lexicon "mi"    = [Cat "mi"       "VP"  [1dan] []]

-- need to handle irregular 'kuru', 'suru' here

-- endings
lexicon "a"     = [Cat "a"       "END"  [5dan, Nai] [],
                   Cat "a"       "AUX"  [1dan, Inanim] [],
                   Cat "a"       "VP"   [1dan, Inanim] []]
lexicon "i"     = [Cat "i"       "END"  [5dan, Masu] [],
                   Cat "i"       "AUX"  [1dan, Anim] [],
                   Cat "i"       "VP"   [1dan, Anim] []] -- stem
lexicon "e"     = [Cat "e"       "FIN"  [5dan, Neutr, Imper] [],
                   Cat "e"       "END"  [5dan, 1dan, Neutr, May] []] -- ending 'e' transforms godan verbs into shimo 1 dan. how can we represent this?

lexicon "ru"     = [Cat "ru"     "FIN"  [1dan, Neutr, May] [], --stem + "e" before this
                    Cat "ru"     "FIN"  [Decl, Pres] []] 
lexicon "rareru" = [Cat "rareru" "FIN"  [1dan, Neutr, May] []] 
lexicon "you"    = [Cat "you"    "FIN"  [1dan, Neutr, Imper] []] 
lexicon "ou"     = [Cat "ou"     "FIN"  [5dan, Neutr, Imper] []] 
lexicon "ouka"   = [Cat "ouka"   "FIN"  [5dan, Neutr, Imper] []] 
lexicon "nai"    = [Cat "nai"    "END"  [Nai, Nega] []] 

lexicon "masu"   = [Cat "masu"   "FIN"  [Masu] []] 
lexicon "mas"    = [Cat "mas"    "END"  [Poli] []]
lexicon "masa"   = [Cat "mas"    "END"  [Nai, Poli] []]

lexicon "te"     = [Cat "te"     "FIN"  [Te] []] 
lexicon "reru"   = [Cat "reru"   "FIN"  [Nai, 5dan, Pass] []] -- Nai
lexicon "seru"   = [Cat "seru"   "FIN"  [Nai, 5dan, Caus] []] -- Nai

lexicon "aru"    = [Cat "aru"    "VP"   [1dan, Inanim, Neutr, Decl, Pres] [],
                    Cat "aru"    "AUX"  [1dan, Inanim, Neutr, Decl, Pres] []]

lexicon "u"      = [Cat "u"      "FIN"  [Decl, Pres] []] -- Finalizing ending

lexicon "ta"     = [Cat "ta"     "FIN"  [Te, Decl, Past] []]
lexicon "ita"    = [Cat "ita"    "FIN"  [Te, Decl, Past] []]
-- then combining "i" + "mas" + "ita" makes VP [Anim, Poli, Decl, Past]

