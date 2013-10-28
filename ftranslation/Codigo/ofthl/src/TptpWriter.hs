module TptpWriter(tptpFormat) where

import Control.Monad.State

import OFTranslation


tptpFormat :: [FOL] -> String
tptpFormat = unlines . flip evalState 1 . mapM formatFormula

formatFormula :: FOL -> State Int String
formatFormula f =
    do
        i <- get
        put (i+1)
        return $ concat ["fof(", "formula_" ++ (show i), ", negated_conjecture, (\n",
                         formula f, ")\n",
                         ")."]


formula :: FOL -> String
formula (l :== r)     = concat ["(", term l,  " = ", term r,  ")"]
formula (P i x)     = concat [relP i, "(", term x, ")"]
formula (R i x y)   = concat [relR i, "(", term x, ", ", term y, ")"]
formula (Not f)      = concat ["~(", formula  f, ")"]
formula (l :&& r)     = concat ["(", formula  l, " & ", formula  r, ")"]
formula (l :|| r)     = concat ["(", formula  l, " | ", formula r, ")"]
formula (l :--> r)    = concat ["(", formula  l, " => ", formula r, ")"]
formula (l :<--> r)   = concat ["(", formula l, " <=> ", formula r, ")"]
formula (E v f)    = concat ["? [", printVar v, "] : (", formula f, ")"]
formula (A v f)    = concat ["! [", printVar v, "] : (", formula f, ")"]


term :: Term -> String
term (V i) = if i == (X 1) then "x1" else printVar i
term (N n)     = nom n
term (F r i t) = (relR r) ++ "(" ++ ("X"++(show i)) ++ ", " ++ term t ++ ")"

nom :: NomId -> String
nom  = ("n" ++)

relR :: RelId -> String
relR = ('r':)

relP :: PropId -> String
relP = ('p':)
