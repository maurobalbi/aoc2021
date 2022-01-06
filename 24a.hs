{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

import AOC
import qualified Prelude as P
import Data.Either
import Data.Functor
import Control.Lens hiding ((.>))
import Data.SBV
import qualified Data.Map as M
import Debug.Trace

type Data = SWord64

data Register = X | Y | Z | W deriving (Eq, Show)

data B = Var Register | Number Int deriving (Eq, Show)

data Instruction =
  Input Register
  | Instruction Op Register B
    deriving (Eq, Show)

data Op =
    Add
  | Mul
  | Div
  | Mod
  | Eql
    deriving (Eq, Show)

data RegState = RegState {_x::Data, _y::Data, _z::Data, _w::Data}
makeLenses ''RegState

initialState :: RegState
initialState = RegState 0 0 0 0

toNum :: [Data] -> Data
toNum = foldl1 (\acc e -> acc * 10 + e)

readVal :: B -> RegState -> Data
readVal (Number i) _ = fromIntegral i
readVal (Var reg) state = readReg reg state

readReg ::Register -> RegState -> Data
readReg X = _x
readReg Y = _y
readReg Z = _z
readReg W = _w

writeReg :: RegState -> Register -> Data -> RegState
writeReg state X v = state {_x = v}
writeReg state Y v = state {_y = v}
writeReg state Z v = state {_z = v}
writeReg state W v = state {_w = v}


opFunc :: Op -> Data -> Data -> Data
opFunc Add = (+)
opFunc Mul = (*)
opFunc Div = sDiv
opFunc Mod = sMod
opFunc Eql = \a b -> ite (a .== b) 1 0

interpret :: [Data] -> [Instruction] -> RegState
interpret = interpret' initialState

interpret' :: RegState -> [Data] -> [Instruction] -> RegState
interpret' state [] [] = state
interpret' state (inp:rest) ((Input reg): ins) = interpret' (writeReg state reg inp) rest ins
interpret' state inp ((Instruction op a b): inss) = 
  interpret' (writeReg state a (opFunc op (readReg a state) (readVal b state))) inp inss
interpret' state _ _ = error "someting went wrong"


run :: [Instruction] -> IO OptimizeResult
run inp = optimize Lexicographic $ do
    x <- mkExistVars 14
    constrain $ isValid x
    maximize "value" $ toNum x
    where
      isValid :: [Data] -> SBool
      isValid i = sAll (\n -> n .>= 1 .&& n .<= 9) i .&& interpret i inp ^. z .== 0

main :: IO ()
main = do
  content <- lines <$> getContents
  x <-  run $ ins content
  print x
    where
      ins content = parseList p content

p :: Parser Instruction
p = do
  inp <|> instruction
    where
      inp = try $ string "inp " >> Input <$> reg
      instruction = do
        o <- op
        r <- reg
        Instruction o r <$> be
      op = add <|> mul <|> div <|> mod <|> eql
      add = try $ string "add " $> Add
      mul = try $ string "mul " $> Mul
      div = try $ string "div " $> Div
      mod = try $ string "mod " $> Mod
      eql = try $ string "eql " $> Eql

be:: Parser B
be = space *> ((Var <$> reg) <|> (Number <$> signedInteger ))

reg :: Parser Register
reg = char 'x' $> X <|> char 'y' $> Y <|> char 'z' $> Z <|> char 'w' $> W