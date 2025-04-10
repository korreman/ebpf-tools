module Ebpf.Helpers where

import Ebpf.Asm
import Data.Int (Int16, Int32)

type Imm32 = Int32
type Offset16 = Int16

r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10 :: Reg
[r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10] = map Reg [0 .. 10]

add64_i :: Reg -> Imm32 -> Instruction
add64_i dst imm = Binary B64 Add dst (Right $ fromIntegral imm)
add64_r :: Reg -> Reg -> Instruction
add64_r dst src = Binary B64 Add dst (Left src)
sub64_i :: Reg -> Imm32 -> Instruction
sub64_i dst imm = Binary B64 Sub dst (Right $ fromIntegral imm)
sub64_r :: Reg -> Reg -> Instruction
sub64_r dst src = Binary B64 Sub dst (Left src)
mul64_i :: Reg -> Imm32 -> Instruction
mul64_i dst imm = Binary B64 Mul dst (Right $ fromIntegral imm)
mul64_r :: Reg -> Reg -> Instruction
mul64_r dst src = Binary B64 Mul dst (Left src)
div64_i :: Reg -> Imm32 -> Instruction
div64_i dst imm = Binary B64 Div dst (Right $ fromIntegral imm)
div64_r :: Reg -> Reg -> Instruction
div64_r dst src = Binary B64 Div dst (Left src)
or64_i :: Reg -> Imm32 -> Instruction
or64_i dst imm = Binary B64 Or dst (Right $ fromIntegral imm)
or64_r :: Reg -> Reg -> Instruction
or64_r dst src = Binary B64 Or dst (Left src)
and64_i :: Reg -> Imm32 -> Instruction
and64_i dst imm = Binary B64 And dst (Right $ fromIntegral imm)
and64_r :: Reg -> Reg -> Instruction
and64_r dst src = Binary B64 And dst (Left src)
lsh64_i :: Reg -> Imm32 -> Instruction
lsh64_i dst imm = Binary B64 Lsh dst (Right $ fromIntegral imm)
lsh64_r :: Reg -> Reg -> Instruction
lsh64_r dst src = Binary B64 Lsh dst (Left src)
rsh64_i :: Reg -> Imm32 -> Instruction
rsh64_i dst imm = Binary B64 Rsh dst (Right $ fromIntegral imm)
rsh64_r :: Reg -> Reg -> Instruction
rsh64_r dst src = Binary B64 Rsh dst (Left src)
mod64_i :: Reg -> Imm32 -> Instruction
mod64_i dst imm = Binary B64 Mod dst (Right $ fromIntegral imm)
mod64_r :: Reg -> Reg -> Instruction
mod64_r dst src = Binary B64 Mod dst (Left src)
xor64_i :: Reg -> Imm32 -> Instruction
xor64_i dst imm = Binary B64 Xor dst (Right $ fromIntegral imm)
xor64_r :: Reg -> Reg -> Instruction
xor64_r dst src = Binary B64 Xor dst (Left src)
mov64_i :: Reg -> Imm32 -> Instruction
mov64_i dst imm = Binary B64 Mov dst (Right $ fromIntegral imm)
mov64_r :: Reg -> Reg -> Instruction
mov64_r dst src = Binary B64 Mov dst (Left src)
arsh64_i :: Reg -> Imm32 -> Instruction
arsh64_i dst imm = Binary B64 Arsh dst (Right $ fromIntegral imm)
arsh64_r :: Reg -> Reg -> Instruction
arsh64_r dst src = Binary B64 Arsh dst (Left src)

add32_i :: Reg -> Imm32 -> Instruction
add32_i dst imm = Binary B32 Add dst (Right $ fromIntegral imm)
add32_r :: Reg -> Reg -> Instruction
add32_r dst src = Binary B32 Add dst (Left src)
sub32_i :: Reg -> Imm32 -> Instruction
sub32_i dst imm = Binary B32 Sub dst (Right $ fromIntegral imm)
sub32_r :: Reg -> Reg -> Instruction
sub32_r dst src = Binary B32 Sub dst (Left src)
mul32_i :: Reg -> Imm32 -> Instruction
mul32_i dst imm = Binary B32 Mul dst (Right $ fromIntegral imm)
mul32_r :: Reg -> Reg -> Instruction
mul32_r dst src = Binary B32 Mul dst (Left src)
div32_i :: Reg -> Imm32 -> Instruction
div32_i dst imm = Binary B32 Div dst (Right $ fromIntegral imm)
div32_r :: Reg -> Reg -> Instruction
div32_r dst src = Binary B32 Div dst (Left src)
or32_i :: Reg -> Imm32 -> Instruction
or32_i dst imm = Binary B32 Or dst (Right $ fromIntegral imm)
or32_r :: Reg -> Reg -> Instruction
or32_r dst src = Binary B32 Or dst (Left src)
and32_i :: Reg -> Imm32 -> Instruction
and32_i dst imm = Binary B32 And dst (Right $ fromIntegral imm)
and32_r :: Reg -> Reg -> Instruction
and32_r dst src = Binary B32 And dst (Left src)
lsh32_i :: Reg -> Imm32 -> Instruction
lsh32_i dst imm = Binary B32 Lsh dst (Right $ fromIntegral imm)
lsh32_r :: Reg -> Reg -> Instruction
lsh32_r dst src = Binary B32 Lsh dst (Left src)
rsh32_i :: Reg -> Imm32 -> Instruction
rsh32_i dst imm = Binary B32 Rsh dst (Right $ fromIntegral imm)
rsh32_r :: Reg -> Reg -> Instruction
rsh32_r dst src = Binary B32 Rsh dst (Left src)
mod32_i :: Reg -> Imm32 -> Instruction
mod32_i dst imm = Binary B32 Mod dst (Right $ fromIntegral imm)
mod32_r :: Reg -> Reg -> Instruction
mod32_r dst src = Binary B32 Mod dst (Left src)
xor32_i :: Reg -> Imm32 -> Instruction
xor32_i dst imm = Binary B32 Xor dst (Right $ fromIntegral imm)
xor32_r :: Reg -> Reg -> Instruction
xor32_r dst src = Binary B32 Xor dst (Left src)
mov32_i :: Reg -> Imm32 -> Instruction
mov32_i dst imm = Binary B32 Mov dst (Right $ fromIntegral imm)
mov32_r :: Reg -> Reg -> Instruction
mov32_r dst src = Binary B32 Mov dst (Left src)
arsh32_i :: Reg -> Imm32 -> Instruction
arsh32_i dst imm = Binary B32 Arsh dst (Right $ fromIntegral imm)
arsh32_r :: Reg -> Reg -> Instruction
arsh32_r dst src = Binary B32 Arsh dst (Left src)

neg64 :: Reg -> Instruction
neg64 dst = Unary B64 Neg dst
neg32 :: Reg -> Instruction
neg32 dst = Unary B32 Neg dst
le16 :: Reg -> Instruction
le16 dst  = Unary B16 Le dst
le32 :: Reg -> Instruction
le32 dst  = Unary B32 Le dst
le64 :: Reg -> Instruction
le64 dst  = Unary B64 Le dst
be16 :: Reg -> Instruction
be16 dst  = Unary B16 Be dst
be32 :: Reg -> Instruction
be32 dst  = Unary B32 Be dst
be64 :: Reg -> Instruction
be64 dst  = Unary B64 Be dst


lddw :: Reg -> Imm -> Instruction
lddw dst imm = LoadImm dst imm
loadMapFd :: Reg -> Imm -> Instruction
loadMapFd dst imm = LoadMapFd dst imm

ldxw :: Reg -> Reg -> Offset16 -> Instruction
ldxw dst src off = Load B32 dst src (Just $ fromIntegral off)
ldxh :: Reg -> Reg -> Offset16 -> Instruction
ldxh dst src off = Load B16 dst src (Just $ fromIntegral off)
ldxb :: Reg -> Reg -> Offset16 -> Instruction
ldxb dst src off = Load B8 dst src (Just $ fromIntegral off)
ldxdw :: Reg -> Reg -> Offset16 -> Instruction
ldxdw dst src off = Load B64 dst src (Just $ fromIntegral off)

stw :: Reg -> Offset16 -> Imm32 -> Instruction
stw dst off imm = Store B32 dst (Just $ fromIntegral off) (Right $ fromIntegral imm)
sth :: Reg -> Offset16 -> Imm32 -> Instruction
sth dst off imm = Store B16 dst (Just $ fromIntegral off) (Right $ fromIntegral imm)
stb :: Reg -> Offset16 -> Imm32 -> Instruction
stb dst off imm = Store B8 dst (Just $ fromIntegral off) (Right $ fromIntegral imm)
stdw :: Reg -> Offset16 -> Imm32 -> Instruction
stdw dst off imm = Store B64 dst (Just $ fromIntegral off) (Right $ fromIntegral imm)

stxw :: Reg -> Offset16 -> Reg -> Instruction
stxw dst off src = Store B32 dst (Just $ fromIntegral off) (Left src)
stxh :: Reg -> Offset16 -> Reg -> Instruction
stxh dst off src = Store B16 dst (Just $ fromIntegral off) (Left src)
stxb :: Reg -> Offset16 -> Reg -> Instruction
stxb dst off src = Store B8 dst (Just $ fromIntegral off) (Left src)
stxdw :: Reg -> Offset16 -> Reg -> Instruction
stxdw dst off src = Store B64 dst (Just $ fromIntegral off) (Left src)

ja :: Offset16 -> Instruction
ja off = Jmp (Right $ fromIntegral off)
jmp :: Offset16 -> Instruction
jmp off = Jmp (Right $ fromIntegral off)

jeq_i :: Reg -> Imm32 -> Offset16 -> Instruction
jeq_i dst imm off = JCond Jeq dst (Right $ fromIntegral imm) (Right $ fromIntegral off)
jeq_r :: Reg -> Reg -> Offset16 -> Instruction
jeq_r dst src off = JCond Jeq dst (Left src) (Right $ fromIntegral off)
jgt_i :: Reg -> Imm32 -> Offset16 -> Instruction
jgt_i dst imm off = JCond Jgt dst (Right $ fromIntegral imm) (Right $ fromIntegral off)
jgt_r :: Reg -> Reg -> Offset16 -> Instruction
jgt_r dst src off = JCond Jgt dst (Left src) (Right $ fromIntegral off)
jge_i :: Reg -> Imm32 -> Offset16 -> Instruction
jge_i dst imm off = JCond Jge dst (Right $ fromIntegral imm) (Right $ fromIntegral off)
jge_r :: Reg -> Reg -> Offset16 -> Instruction
jge_r dst src off = JCond Jge dst (Left src) (Right $ fromIntegral off)
jlt_i :: Reg -> Imm32 -> Offset16 -> Instruction
jlt_i dst imm off = JCond Jlt dst (Right $ fromIntegral imm) (Right $ fromIntegral off)
jlt_r :: Reg -> Reg -> Offset16 -> Instruction
jlt_r dst src off = JCond Jlt dst (Left src) (Right $ fromIntegral off)
jle_i :: Reg -> Imm32 -> Offset16 -> Instruction
jle_i dst imm off = JCond Jle dst (Right $ fromIntegral imm) (Right $ fromIntegral off)
jle_r :: Reg -> Reg -> Offset16 -> Instruction
jle_r dst src off = JCond Jle dst (Left src) (Right $ fromIntegral off)
jset_i :: Reg -> Imm32 -> Offset16 -> Instruction
jset_i dst imm off = JCond Jset dst (Right $ fromIntegral imm) (Right $ fromIntegral off)
jset_r :: Reg -> Reg -> Offset16 -> Instruction
jset_r dst src off = JCond Jset dst (Left src) (Right $ fromIntegral off)
jne_i :: Reg -> Imm32 -> Offset16 -> Instruction
jne_i dst imm off = JCond Jne dst (Right $ fromIntegral imm) (Right $ fromIntegral off)
jne_r :: Reg -> Reg -> Offset16 -> Instruction
jne_r dst src off = JCond Jne dst (Left src) (Right $ fromIntegral off)
jsgt_i :: Reg -> Imm32 -> Offset16 -> Instruction
jsgt_i dst imm off = JCond Jsgt dst (Right $ fromIntegral imm) (Right $ fromIntegral off)
jsgt_r :: Reg -> Reg -> Offset16 -> Instruction
jsgt_r dst src off = JCond Jsgt dst (Left src) (Right $ fromIntegral off)
jsge_i :: Reg -> Imm32 -> Offset16 -> Instruction
jsge_i dst imm off = JCond Jsge dst (Right $ fromIntegral imm) (Right $ fromIntegral off)
jsge_r :: Reg -> Reg -> Offset16 -> Instruction
jsge_r dst src off = JCond Jsge dst (Left src) (Right $ fromIntegral off)
jslt_i :: Reg -> Imm32 -> Offset16 -> Instruction
jslt_i dst imm off = JCond Jslt dst (Right $ fromIntegral imm) (Right $ fromIntegral off)
jslt_r :: Reg -> Reg -> Offset16 -> Instruction
jslt_r dst src off = JCond Jslt dst (Left src) (Right $ fromIntegral off)
jsle_i :: Reg -> Imm32 -> Offset16 -> Instruction
jsle_i dst imm off = JCond Jsle dst (Right $ fromIntegral imm) (Right $ fromIntegral off)
jsle_r :: Reg -> Reg -> Offset16 -> Instruction
jsle_r dst src off = JCond Jsle dst (Left src) (Right $ fromIntegral off)

call :: Imm -> Instruction
call f = Call f
exit :: Instruction
exit = Exit
