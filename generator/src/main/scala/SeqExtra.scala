package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqExtra(use: EnabledInstructions, xregs: HWRegPool, fregs_s: HWRegPool, fregs_d: HWRegPool, mem: Mem) extends InstSeq
{
  override val seqname = "extra"

  val candidates = new ArrayBuffer[() => insts.type]

  def seq_c_loadfn(op: Opcode, addrfn: (Int) => Int, fregpool: HWRegPool, scaling: Int) = () =>
  {
    val reg_addr = reg_write_c_xreg_hidden(xregs)
    val reg_dest = reg_write_c_freg_visible(fregpool)
    val addr = addrfn(mem.size)
    // INFO: offset is 6 Bits and scaled by 2 or 3 bits (FLW vs FLD)
    // TODO: check how to generate valid negative offsets!
    val imm = (rand_imm() & 0x1F) * scaling
    insts += LA(reg_addr, BaseImm(mem.toString, addr-imm))
    insts += op(reg_dest, RegImm(reg_addr, imm))
  }

  def seq_c_storefn(op: Opcode, addrfn: (Int) => Int, fregpool: HWRegPool, scaling: Int) = () =>
  {
    val reg_addr = reg_write_c_xreg_hidden(xregs)
    val reg_src = reg_read_c_freg_visible(fregpool)
    val addr = addrfn(mem.size)
    // INFO: offset is 6 Bits and scaled by 2 or 3 bits (FLW vs FLD)
    // TODO: check how to generate valid negative offsets!
    val imm = (rand_imm() & 0x1F) * scaling
    insts += LA(reg_addr, BaseImm(mem.toString, addr-imm))
    insts += op(reg_src, RegImm(reg_addr, imm))
  }

  def seq_c_loadspfn(op: Opcode, addrfn: (Int) => Int, fregpool: HWRegPool, scaling: Int) = () =>
  {
    val reg_sp = reg_write_sp_hidden(xregs)
    val reg_dest = reg_write_c_freg_visible(fregpool)
    val addr = addrfn(mem.size)
    // INFO: offset is 6 Bits and scaled by 2 or 3 bits (FLW vs FLD)
    // TODO: check how to generate valid negative offsets!
    val imm = (rand_imm() & 0x1F) * scaling
    insts += LA(reg_sp, BaseImm(mem.toString, addr-imm))
    insts += op(reg_dest, RegImm(reg_sp, imm))
  }

  def seq_c_storespfn(op: Opcode, addrfn: (Int) => Int, fregpool: HWRegPool, scaling: Int) = () =>
  {
    val reg_sp = reg_write_sp_hidden(xregs)
    val reg_src = reg_read_c_freg_visible(fregpool)
    val addr = addrfn(mem.size)
    // INFO: offset is 6 Bits and scaled by 2 or 3 bits (FLW vs FLD)
    // TODO: check how to generate valid negative offsets!
    val imm = (rand_imm() & 0x1F) * scaling
    insts += LA(reg_sp, BaseImm(mem.toString, addr-imm))
    insts += op(reg_src, RegImm(reg_sp, imm))
  }

  def seq_ecall() = () =>
  {
    //    LA t2, 1f
    //    ECALL
    // 1:
    val reg_t2 = reg_write_t2_visible(xregs)
    insts += LA(reg_t2, Label("1f"))
    insts += ECALL()
  }

  if(use.fps)
  {
    candidates += seq_c_loadfn(C_FLW, rand_addr_w, fregs_s, 4)
    candidates += seq_c_storefn(C_FSW, rand_addr_w, fregs_s, 4)

    candidates += seq_c_loadspfn(C_FLWSP, rand_addr_w, fregs_s, 4)
    candidates += seq_c_storespfn(C_FSWSP, rand_addr_w, fregs_s, 4)
  }

  if(use.fpd)
  {
    candidates += seq_c_loadfn(C_FLD, rand_addr_d, fregs_d, 8)
    candidates += seq_c_storefn(C_FSD, rand_addr_d, fregs_d, 8)

    candidates += seq_c_loadspfn(C_FLDSP, rand_addr_d, fregs_d, 8)
    candidates += seq_c_storespfn(C_FSDSP, rand_addr_d, fregs_d, 8)
  }

  candidates += seq_ecall()

  rand_pick(candidates)()
}
