module Sudoku (sudokuG) where

import Data.Graph
import Data.Graph.Construction

-- The infamous Sudoku board -- isomorphic graph

data Node = Line Int | Column Int | Block Int Int | Square Int Int deriving Show

n = 2

n2 = n*n

n4 = n2*n2

index (Line   x)   = x                     -- n2 items
index (Column x)   = x           + n2      -- n2 items
index (Block  x y) = n *x + y    + 2*n2    -- n2 items
index (Square x y) = n2*x + y    + 3*n2    -- n4 items

totalVertices      =               3*n2+n4

ns  = [0..n-1]
n2s = [0..n2-1]

relations =    [(Line   x,   Square    x        y    ) | x <- n2s, y <- n2s] 
            ++ [(Column   y, Square    x        y    ) | x <- n2s, y <- n2s] 
            ++ [(Block  x y, Square (n*x+x') (n*y+y')) | x <- ns,  y <- ns, x' <- ns, y' <- ns]

sudokuG = undirG $ buildG (0,totalVertices-1) [(index a, index b) | (a,b) <- relations]
