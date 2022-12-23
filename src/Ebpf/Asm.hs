module Ebpf.Asm where

import Data.Int (Int64)
import Data.Foldable (asum)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.List (foldl')

data BinAlu = Add | Sub | Mul | Div | Or | And | Lsh | Rsh | Mod | Xor
  | Mov | Arsh
  deriving (Eq, Show, Ord, Enum)

data UnAlu = Neg | Le | Be
  deriving (Eq, Show, Ord, Enum)

data BSize = B8 | B16 | B32 | B64
  deriving (Eq, Show, Ord, Enum)

data Jcmp = Jeq | Jgt | Jge | Jlt | Jle | Jset | Jne | Jsgt | Jsge | Jslt | Jsle
  deriving (Eq, Show, Ord, Enum)

--data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
newtype Reg = Reg Int deriving (Eq, Show, Ord)
type Imm = Int64
type RegImm = Either Reg Imm
type Offset = Int64
type Label = String
type JmpTarget = Either Label Offset

-- TODO support atomic operations
-- TODO support absolute and indirect loads
-- TODO support tail calls

data Instruction =
    Binary BSize BinAlu Reg RegImm
  | Unary BSize UnAlu Reg
  | Store BSize Reg (Maybe Offset) RegImm
  | Load BSize Reg Reg (Maybe Offset)
  | LoadImm Reg Imm
  | LoadMapFd Reg Imm
  | Label Label
  | JCond Jcmp Reg RegImm JmpTarget
  | Jmp JmpTarget
  | Call Imm
  | Exit
  deriving (Eq, Show, Ord)

type Program = [Instruction]

wellformed :: Program -> Maybe String
wellformed instrs = asum $ fmap wfInst instrs
  where
    wfReg (Reg n) | 0 <= n && n < 11 = Nothing
                  | otherwise = Just $ "Invalid register: r"++show n
    wfRegImm (Left r) = wfReg r
    wfRegImm (Right imm) | -2^31 <= imm && imm < 2^31 = Nothing
                         | otherwise = Just $ "Invalid immediate: "++show imm
    wfOffset n | -2^15 <= n && n < 2^15 = Nothing
               | otherwise = Just $ "Invalid immediate: "++show n
    wfInst inst =
      case inst of
        Binary bs opr _ _ | bs == B8 || bs == B16 ->
                            return $ concat ["Invalid byte size '", show bs
                                            , "' for operation: ", show opr]
        Binary _ _ r ri -> asum [wfReg r, wfRegImm ri]
        Unary bs@B8 opr _ -> return $ concat ["Invalid byte size '", show bs
                                            , "' for operation: ", show opr]
        Unary B16 Neg _ -> return "Invalid byte size 'B16' for operation: Neg"
        Unary _ _ r -> wfReg r
        Store _ dst off src -> asum [wfReg dst, off >>= wfOffset, wfRegImm src]
        Load _ dst src off -> asum [wfReg dst, off >>= wfOffset, wfReg src]
        LoadImm dst _ -> wfReg dst
        LoadMapFd dst _ -> wfReg dst
        JCond _ lhs rhs (Right off) -> asum [wfReg lhs, wfRegImm rhs, wfOffset off]
        Jmp (Right off) -> wfOffset off
        _ -> Nothing
  -- | Call Imm
  -- | Exit

resolveLabels :: Program -> Maybe Program
resolveLabels instrs =
  let labels = collectLabels instrs in
  let realInstrs = filter notLabel instrs in
  sequenceA $ zipWith (resolveInstr labels) [0..] realInstrs
  where notLabel (Label _) = False
        notLabel _ = True

resolveInstr :: M.Map Label Imm -> Imm -> Instruction -> Maybe Instruction
resolveInstr labels pos instr =
    case instr of
        JCond c r1 r2 (Left l) -> do
            target <- M.lookup l labels
            let offset = target - pos - 1
            return $ JCond c r1 r2 (Right offset)
        Jmp (Left l) -> do
            target <- M.lookup l labels
            let offset = target - pos - 1
            return $ Jmp (Right offset)
        Label l -> Nothing
        instr -> Just instr

collectLabels :: Program -> M.Map Label Imm
collectLabels prog = snd $ foldl' collect (0, M.empty) prog

collect ::  (Int64, M.Map Label Imm) -> Instruction -> (Int64, M.Map Label Imm)
collect (n, labels) instr =
    case instr of
        Label label -> (n, M.insert label n labels)
        _ -> (n + 1, labels)

