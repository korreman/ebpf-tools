module Ebpf.AsmParser where

import Ebpf.Asm
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Int (Int64)

import Text.Parsec
import qualified Text.Parsec.String as PS

type Parser a = PS.Parser a

asciiAlphaNum :: Parser Char
asciiAlphaNum = satisfy (\c -> C.isAscii c && C.isAlphaNum c)

ignore :: Parser a -> Parser ()
ignore p = p >> return ()

comment = ignore $ char ';' >> manyTill anyChar (try endOfLine)

sc = skipMany (comment <|> ignore space)
lexeme :: Parser a -> Parser a
lexeme p = p <* sc

symbol s = lexeme $ string s

operator = lexeme $ many1 asciiAlphaNum

reg :: Parser Reg
reg = Reg . read <$> lexeme (char 'r' >> many1 digit) <?> "register"

imm :: Parser Int64
imm = lexeme number <?> "immediate constant"
  where
    number = sign <*> unsigned
    unsigned = (char '0' >> (hex <|> decimal <|> return 0))
               <|> decimal
    hex = read . ("0x" ++) <$> (oneOf "Xx" >> many1 hexDigit)
    decimal = read <$> many1 digit
    sign = (char '-' >> return negate)
           <|> (optional (char '+') >> return id)

regimm = (Left <$> reg) <|> (Right <$> imm)

labelName = lexeme (many1 asciiAlphaNum) <?> "label"

jmpTarget = (Left <$> labelName) <|> (Right <$> imm)

ocomma = lexeme . optional $ char ','

lowercase opr = map C.toLower $ show opr

binAlus = do
  alu <- [Add .. Arsh]
  (post, sz) <- [("32", B32), ("", B64)]
  let name = lowercase alu ++ post
  return (name, Binary sz alu <$> reg <* ocomma <*> regimm)

unAlus = do
 alu <- [Neg .. Be]
 (post, sz) <- case alu of
                 Neg -> [("32", B32), ("", B64)]
                 _ -> [("16", B16), ("32", B32), ("64", B64)]
 let name = lowercase alu ++ post
 return (name, Unary sz alu <$> reg)


memref = between (symbol "[") (symbol "]")
         ((,) <$> reg <*> (optionMaybe (symbol "+" *> imm)))

stores = do
  (mem_sz, bsz) <- [("b", B8), ("h", B16), ("w", B32), ("dw", B64)]
  (x, r_or_i) <- [("x", Left <$> reg), ("", Right <$> imm)]
  let name = "st" ++ x ++ mem_sz
  return (name, do (r, off) <- memref
                   ocomma
                   Store bsz r off <$> r_or_i)

loads = do
  (mem_sz, bsz) <- [("b", B8), ("h", B16), ("w", B32), ("dw", B64)]
  let name = "ldx" ++ mem_sz
  return (name, do dst <- reg
                   ocomma
                   (src, off) <- memref
                   return $ Load bsz dst src off)

conditionals = do
  jmp <- [Jeq .. Jsle]
  let name = lowercase jmp
  return(name, JCond jmp <$> reg <* ocomma <*> regimm <* ocomma <*> jmpTarget)


instruction = do
  opr <- operator
  case M.lookup opr operators of
    Just argP -> argP
    _ -> fail $ "Unknown operator: " ++ opr
  where
    operators = M.fromList $ binAlus ++
                             unAlus ++
                             stores ++
                             loads ++ [ ("lddw", LoadImm <$> reg <* ocomma <*> imm)] ++
                             conditionals ++
                             [ ("ja", Jmp <$> jmpTarget),
                               ("call", Call <$> imm),
                               ("exit", pure Exit)]

instrLabel = do
  name <- labelName
  lexeme $ char ':'
  inst <- instruction
  return $ Label name inst

program = sc >> many1 (try instrLabel <|> instruction) <* eof

parseFromFile :: FilePath -> IO(Either String Program)
parseFromFile filename =
  do result <- PS.parseFromFile program filename
     return $ case result of
                Left err   -> Left $ show err
                Right prog -> Right prog
