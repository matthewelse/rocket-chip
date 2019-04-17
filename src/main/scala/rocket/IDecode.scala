// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.util._
import freechips.rocketchip.scie.SCIE
import Instructions._
import ALU._

abstract trait DecodeConstants extends HasCoreParameters
{
  val table: Array[(BitPat, List[BitPat])]
}

class IntCtrlSigs extends Bundle {
  val legal = Bool()
  val fp = Bool()
  val rocc = Bool()
  val branch = Bool()
  val jal = Bool()
  val jalr = Bool()
  val rxs2 = Bool()
  val rxs1 = Bool()
  val scie = Bool()
  val sel_alu2 = Bits(width = A2_X.getWidth)
  val sel_alu1 = Bits(width = A1_X.getWidth)
  val sel_imm = Bits(width = IMM_X.getWidth)
  val alu_dw = Bool()
  val alu_fn = Bits(width = FN_X.getWidth)
  val mem = Bool()
  val mem_cmd = Bits(width = M_SZ)
  val rfs1 = Bool()
  val rfs2 = Bool()
  val rfs3 = Bool()
  val wfd = Bool()
  val mul = Bool()
  val div = Bool()
  val wxd = Bool()
  val csr = Bits(width = CSR.SZ)
  val fence_i = Bool()
  val fence = Bool()
  val amo = Bool()
  val dp = Bool()
  val fuse_clear = Bool()

  def default: List[BitPat] =
                //           jal                                                             renf1               fence.i
                //   val     | jalr                                                          | renf2             |
                //   | fp_val| | renx2                                                       | | renf3           |
                //   | | rocc| | | renx1       s_alu1                          mem_val       | | | wfd           |
                //   | | | br| | | |   s_alu2  |       imm    dw     alu       | mem_cmd     | | | | mul         |       fuse
                //   | | | | | | | |   |       |       |      |      |         | |           | | | | | div       | fence | 
                //   | | | | | | | |   |       |       |      |      |         | |           | | | | | | wxd     | | amo |
                //   | | | | | | | | scie      |       |      |      |         | |           | | | | | | |       | | | dp|
                List(N,X,X,X,X,X,X,X,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        X,X,X,X,X,X,X,CSR.X,X,X,X,X,X)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, scie, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd,
                   rfs1, rfs2, rfs3, wfd, mul, div, wxd, csr, fence_i, fence, amo, dp)
    sigs zip decoder map {case(s,d) => s := d}
    fuse_clear := false.B
    this
  }
}

class IDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BNE->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    BEQ->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    BLT->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    BLTU->      List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    BGE->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    BGEU->      List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),

    JAL->       List(Y,N,N,N,Y,N,N,N,N,A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    JALR->      List(Y,N,N,N,N,Y,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    AUIPC->     List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),

    LB->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    LH->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    LW->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    LBU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    LHU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SB->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    SH->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    SW->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),

    LUI->       List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    ADDI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SLTI ->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SLTIU->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    ANDI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    ORI->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    XORI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    ADD->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SUB->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SLT->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SLTU->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    AND->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    OR->        List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    XOR->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SLL->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SRL->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SRA->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),

    FENCE->     List(Y,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.N,N,Y,N,N,X),

    SCALL->     List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,X),
    SBREAK->    List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,X),
    MRET->      List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,X),
    WFI->       List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,X),
    CEASE->     List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,X),
    CSRRW->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N,X),
    CSRRS->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N,X),
    CSRRC->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N,X),
    CSRRWI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N,X),
    CSRRSI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N,X),
    CSRRCI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N,X))
}

class FenceIDecode(flushDCache: Boolean)(implicit val p: Parameters) extends DecodeConstants
{
  private val (v, cmd) = if (flushDCache) (Y, BitPat(M_FLUSH_ALL)) else (N, M_X)

  val table: Array[(BitPat, List[BitPat])] = Array(
    FENCE_I->   List(Y,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     v,cmd,        N,N,N,N,N,N,N,CSR.N,Y,Y,N,N,X))
}

class CFlushDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CFLUSH_D_L1->
                List(Y,N,N,N,N,N,N,Y,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     Y,M_FLUSH_ALL,N,N,N,N,N,N,N,CSR.I,N,N,N,N,X))
}

class SDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SFENCE_VMA->List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_SFENCE,   N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    SRET->      List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,X))
}

class DebugDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    DRET->      List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N,X))
}

class I32Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SLLI_RV32-> List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SRLI_RV32-> List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SRAI_RV32-> List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X))
}

class I64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    LD->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    LWU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SD->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),

    SLLI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SRLI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SRAI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),

    ADDIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SLLIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SRLIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SRAIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    ADDW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SUBW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SLLW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SRLW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    SRAW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X))
}

class MDecode(pipelinedMul: Boolean)(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MUL->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,X),
    MULH->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULH,  N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,X),
    MULHU->     List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHU, N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,X),
    MULHSU->    List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHSU,N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,X),

    DIV->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,X),
    DIVU->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,X),
    REM->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,X),
    REMU->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,X))
}

class M64Decode(pipelinedMul: Boolean)(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MULW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N,X),

    DIVW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,X),
    DIVUW->     List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,X),
    REMW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,X),
    REMUW->     List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N,X))
}

class ADecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOXOR_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOSWAP_W-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOAND_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOOR_W->   List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOMIN_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOMINU_W-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOMAX_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOMAXU_W-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),

    LR_W->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    SC_W->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X))
}

class A64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOSWAP_D-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOXOR_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOAND_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOOR_D->   List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOMIN_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOMINU_D-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOMAX_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    AMOMAXU_D-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),

    LR_D->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X),
    SC_D->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N,X))
}

class FDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FSGNJ_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FSGNJX_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FSGNJN_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FMIN_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FMAX_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FADD_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FSUB_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FMUL_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FMADD_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,X),
    FMSUB_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,X),
    FNMADD_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,X),
    FNMSUB_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N,X),
    FCLASS_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    FMV_X_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    FCVT_W_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    FCVT_WU_S-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    FEQ_S->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    FLT_S->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    FLE_S->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    FMV_S_X->   List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FCVT_S_W->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FCVT_S_WU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FLW->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FSW->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,N,X),
    FDIV_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FSQRT_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N,X))
}

class DDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_S_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FCVT_D_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FSGNJ_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FSGNJX_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FSGNJN_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FMIN_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FMAX_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FADD_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FSUB_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FMUL_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FMADD_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FMSUB_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FNMADD_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FNMSUB_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FCLASS_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,X),
    FCVT_W_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,X),
    FCVT_WU_D-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,X),
    FEQ_D->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y,X),
    FLT_D->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y,X),
    FLE_D->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y,X),
    FCVT_D_W->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FCVT_D_WU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FLD->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FSD->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,Y,X),
    FDIV_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FSQRT_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y,X))
}

class F64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_L_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    FCVT_LU_S-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    FCVT_S_L->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,X),
    FCVT_S_LU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N,X))
}

class D64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FMV_X_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,X),
    FCVT_L_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,X),
    FCVT_LU_D-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y,X),
    FMV_D_X->   List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FCVT_D_L->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,X),
    FCVT_D_LU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y,X))
}

class SCIEDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SCIE.opcode->       List(Y,N,N,N,N,N,Y,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_X,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X))
}

class RoCCDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CUSTOM0->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM0_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM0_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM0_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    //CUSTOM0_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    // CUSTOM0_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    // TODO: change this to FN_SHIFT_ADD
    CUSTOM0_RD        ->List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    CUSTOM0_RD_RS1    ->List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    CUSTOM0_RD_RS1_RS2->List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    CUSTOM1->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM1_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM1_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM1_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    CUSTOM1_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    CUSTOM1_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    CUSTOM2->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM2_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM2_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM2_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    CUSTOM2_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    CUSTOM2_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    CUSTOM3->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM3_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM3_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N,X),
    CUSTOM3_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    CUSTOM3_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X),
    CUSTOM3_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N,X))
}
