{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

import Text.Parsec hiding (State)
import Control.Monad.State
import Control.Monad.Loops
import Control.Applicative hiding (many)
import Data.Char (chr, ord)
import System.Environment (getArgs)
import Input

data Tape a = Tape [a] a [a]
    deriving Show

type Program a = InputT a (State (Tape a)) [a]

runProgram :: Program a -> [a] -> Tape a -> (([a], [a]), Tape a)
runProgram p input tape = runState (runInputT p input) tape

loopProgram :: (Num a, Eq a) => Program a -> Program a
loopProgram = fmap concat . whileM (liftM ((/= 0) . readTape) get)

left :: Tape a -> Tape a
left (Tape (l:ls) a rs) = Tape ls l (a:rs)

right :: Tape a -> Tape a
right (Tape ls a (r:rs)) = Tape (a:ls) r rs

incr :: Num a => Tape a -> Tape a
incr (Tape ls a rs) = Tape ls (a + 1) rs

decr :: Num a => Tape a -> Tape a
decr (Tape ls a rs) = Tape ls (a - 1) rs

modify' :: (Tape a -> Tape a) -> Program a
modify' f = modify f >> return []

output :: Program a
output = (\x -> [readTape x]) <$> get

readTape :: Tape a ->  a
readTape (Tape _ a _ ) = a

writeTape :: a -> Tape a -> Tape a
writeTape a (Tape ls _ rs) = Tape ls a rs

writeInput :: Program a
writeInput = input >>= modify' . writeTape

program = fmap concat . sequence
      <$> many (choice [loop, rightP, leftP, incrP, decrP, printP, inputP])
    where rightP = instruction '>' right
          leftP  = instruction '<' left
          incrP  = instruction '+' incr
          decrP  = instruction '-' decr
          printP = output <$ char '.'
          inputP = writeInput <$ char ','
          loop   = fmap loopProgram
                 $ between (char '[') (char ']') program
          instruction c f = modify' f <$ char c

initialTape = Tape (repeat 0) 0 (repeat 0)

fromRight (Right x) = x
parseProgram = fromRight . parse program ""

toFilter :: Program Int -> (String -> String)
toFilter prog = \s -> map chr . fst . fst $ runProgram prog (map ord s) initialTape

main = runProgramAtPath . head =<< getArgs

runProgramAtPath p = do
    prog <- parseProgram . filter (`elem` "><+-[].,") <$> readFile p
    interact (toFilter prog)
