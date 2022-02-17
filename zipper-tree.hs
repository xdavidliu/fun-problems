-- https://www.hackerrank.com/challenges/tree-manager/problem?isFullScreen=false
-- J. Functional Programming 7 (5): 549–554, September 1997
-- "FUNCTIONAL PEARL" The Zipper, Huet

data Tree = Item Int | Section Int [Tree]

value (Item x) = x
value (Section x _) = x
locValue (Loc t _) = value t

-- need value of parent node for goUp
data Path = Top | Node [Tree] Int Path [Tree]

data Loc = Loc Tree Path

goLeft (Loc t p) = case p of
    Node (l:ls) x up rs -> Loc l (Node ls x up (t:rs))
    _ -> error "cannot goLeft"

goRight (Loc t p) = case p of
    Node ls x up (r:rs) -> Loc r (Node (t:ls) x up rs)
    _ -> error "cannot goRight"

goUp (Loc t p) = case p of
    Node ls x up rs -> Loc (Section x (reverse ls ++ (t:rs))) up
    _ -> error "cannot goUp"

goDown (Loc t p) = case t of
    Section x (c:cs) -> Loc c (Node [] x p cs)
    _ -> error "cannot goDown"

goNth n loc = iterate goRight (goDown loc) !! (n-1)

change x (Loc (Item _) p) = Loc (Item x) p
change x (Loc (Section _ cs) p) = Loc (Section x cs) p

insertLeft y (Loc t p) = case p of
    Node ls x up rs -> Loc t (Node (Item y : ls) x up rs)
    _ -> error "cannot insertLeft"

insertRight y (Loc t p) = case p of
    Node ls x up rs -> Loc t (Node ls x up (Item y : rs))
    _ -> error "cannot insertRight"

-- Huet strangely also *goes* to the inserted child. We refuse.
-- Also we handle the Item case, unlike Huet.
insertDown y (Loc t p) = case t of
    Item x -> Loc (Section x [Item y]) p
    Section x cs -> Loc (Section x (Item y : cs)) p

-- slightly different from Huet
deleteCurrent (Loc _ p) = case p of
    Node [] x up [] -> Loc (Item x) up
    Node ls x up rs -> Loc (Section x (reverse ls ++ rs)) up
    _ -> error "cannot deleteCurrent"

perform [] _ = return ()
perform (cmd:rest) loc = do
    newLoc <- performOne (words cmd) loc
    perform rest newLoc

performOne ["print"] loc = do
    print $ locValue loc
    return loc
performOne ["change", x] loc = return $ change (read x) loc
performOne ["visit", "left"] loc = return $ goLeft loc
performOne ["visit", "right"] loc = return $ goRight loc
performOne ["visit", "parent"] loc = return $ goUp loc
performOne ["visit", "child", n] loc = return $ goNth (read n) loc
performOne ["insert", "left", x] loc = return $ insertLeft (read x) loc
performOne ["insert", "right", x] loc = return $ insertRight (read x) loc
performOne ["insert", "child", x] loc = return $ insertDown (read x) loc
performOne ["delete"] loc = return $ deleteCurrent loc
performOne _ _ = error "cannot performOne"

main = do
    getLine
    cmds <- lines <$> getContents
    perform cmds (Loc (Item 0) Top)

