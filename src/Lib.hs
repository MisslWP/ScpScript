module Lib
    ( someFunc
    ) where

import Data.Tree (Tree(..), drawTree)

type VarName = String

data LiteralType =
                   StringType
                 | IntegerType
                 | BooleanType

instance Show LiteralType where
  show StringType = "string"
  show IntegerType = "int"
  show BooleanType = "bool"

data Literal =
               S String
             | I Int
             | B Bool

type Block = [Expression]

instance Show Literal where
  show (S s) = "\"" ++ s ++ "\""
  show (I i) = show i
  show (B b) = show b

data Token =
              IfT Expression
            | LoopT
            | ThenT
            | ElseT
            | RootT
            | DelayedT deriving (Show)

exprNormal :: Expression -> String
exprNormal = e'
  where
  e' (Variable name varType) = show varType ++ " " ++ name
  e' (Assign name expr)      = name ++ " = " ++ e' expr
  e' (Const l)               = show l
  e' (Echo expr)             = "echo " ++ e' expr
  e' (GetVar name)           = name
  e' (Tok LoopT)             = "loop:"
  e' (Tok (IfT cond))        = "if" ++ " (" ++ e' cond ++ "):"
  e' (Tok ThenT)             = "then:"
  e' (Tok ElseT)             = "else:"
  e' (Equal e1 e2)           = e' e1 ++ " == " ++ e' e2
  e' (NotEqual e1 e2)        = e' e1 ++ " != " ++ e' e2
  e' Break                   = "break"
  e' (Sub e1 e2)             = e' e1 ++ " - " ++ e' e2
  e' _                       = ""

data Expression  =
              Variable VarName LiteralType
            | If Expression Block (Maybe Block)
            | While Expression Block
            | For Expression Expression Expression Block
            | AssignDecl LiteralType VarName Expression
            | ThenInner Block
            | ElseInner Block
            | Assign VarName Expression
            | Const Literal
            | Echo Expression
            | GetVar VarName
            | More Expression Expression
            | Less Expression Expression
            | Equal Expression Expression
            | MoreOrEqual Expression Expression
            | LessOrEqual Expression Expression
            | NotEqual Expression Expression
            | And Expression Expression
            | Or Expression Expression
            | Loop Block
            | Continue
            | Break
            | Tok Token
            | Add Expression Expression
            | Sub Expression Expression
            | Mul Expression Expression
            | Mod Expression Expression
            | Div Expression Expression
            | Root Block
            | Delayed Block deriving (Show)

red :: Expression -> [Expression]
red (Loop block)                     = [Loop (concatMap red block)]
red (While cond block)               = [Loop $ If cond [Break] Nothing : concatMap red block]
red (AssignDecl varType name expr)   = [Variable name varType, Assign name expr]
red (If cond thn (Just els))         = [If cond (concatMap red thn) (Just (concatMap red els))]
red (If cond thn Nothing)            = [If cond (concatMap red thn) Nothing]
red (For initialize cond iter block) = red initialize ++ red (While cond (block ++ [iter]))
red ex = [ex]

reduction :: Expression -> Expression
reduction (Root block) = Root (concatMap red block)
reduction ex           = ex

exprTree :: Expression -> Tree Expression
exprTree (If cond thn (Just els)) = Node (Tok $ IfT cond) [exprTree (ThenInner thn), exprTree (ElseInner els)]
exprTree (If cond thn Nothing)    = Node (Tok $ IfT cond) [exprTree (ThenInner thn)]
exprTree (ThenInner thn)          = Node (Tok ThenT) $ map exprTree thn
exprTree (ElseInner els)          = Node (Tok ElseT) $ map exprTree els
exprTree (Loop expr)              = Node (Tok LoopT) (map exprTree expr)
exprTree (Root block)             = Node (Tok RootT) (map exprTree block)
exprTree x                        = Node x []

tab :: String
tab = "    "

sumInts :: [Int] -> Int
sumInts []     = 0
sumInts [i]    = i
sumInts (i:is) = i + sumInts is

blank :: [Char]
blank = ['\n', '\t', ' ']
isBlank :: String -> Bool
isBlank s = length s == containsChars blank s

containsChars :: [Char] -> String -> Int
containsChars [] _     = 0
containsChars [c] s    = containsChar c s
containsChars (c:cs) s = containsChar c s + containsChars cs s

containsChar :: Char -> String -> Int
containsChar c s = sumInts (map (val . (==) c) s)
  where
    val :: Bool -> Int
    val Prelude.False = 0
    val Prelude.True  = 1

exprTreeToString :: Tree Expression -> String
exprTreeToString = unlines . filter (not . isBlank) . draw

draw :: Tree Expression -> [String]
draw (Node expr sub) = exprNormal expr : drawSub sub
  where
    drawSub :: [Tree Expression] -> [String]
    drawSub []     = []
    drawSub [e] = "" : shift "" tab (draw e)
    drawSub (e:es) = "" : shift "" tab (draw e) ++ drawSub es

    shift first other = zipWith (++) (first : repeat other)

someFunc :: IO ()
someFunc = do
             let code2 = Root [For (AssignDecl IntegerType "i" $ Const $ I 5)
                                   (NotEqual (GetVar "i") (Const $ I 0))
                                   (Assign "i" $ Sub (GetVar "i") $ Const $ I 1)
                                   [Echo $ GetVar "i", Echo $ Const $ S "Still not 0!"]
                              ]
             let code2Text = "That:\nfor (int i = 5; i != 0; i = i - 1):\n    echo i\n    echo \"Still not 0!\"\n\nTranslates to that:"
             let tree2 = exprTree $ reduction code2
             putStrLn $ drawTree $ exprNormal <$> tree2
             putStrLn code2Text
             putStrLn $ exprTreeToString tree2
