package vexriscv.plugin

import spinal.core._
import spinal.lib._
import vexriscv._
import vexriscv.Riscv._

class BitManipPlugin extends Plugin[VexRiscv] {
    val ANDN  = M"0100000----------111-----0110011"
    val ORN   = M"0100000----------110-----0110011"
    val XNOR  = M"0100000----------100-----0110011"
    val ROL   = M"0110000----------001-----0110011"
    val ROR   = M"0110000----------101-----0110011"
    val RORI  = M"0110000----------101-----0010011"
    val PACK  = M"0000100----------100-----0110011"
    val PACKH = M"0000100----------111-----0110011"
    val BREV8 = M"011010000111-----101-----0010011"
    val REV8  = M"011010011000-----101-----0010011"
    val ZIP   = M"000010001111-----001-----0010011"
    val UNZIP = M"000010001111-----101-----0010011"

    object BITMANIP_CTRL extends Stageable(Bits(4 bits))
    object BITMANIP_ENABLE extends Stageable(Bool())

    object Ctrl extends SpinalEnum {
      val OP_ANDN, OP_ORN, OP_XNOR, OP_ROL, OP_ROR, OP_RORI, OP_PACK, OP_PACKH, OP_REV8, OP_BREV8, OP_ZIP, OP_UNZIP = newElement()
    }

    override def setup(pipeline: VexRiscv): Unit = {
      import pipeline.config._

      val decoderService = pipeline.service(classOf[DecoderService])

      decoderService.addDefault(BITMANIP_ENABLE, False)

      def add(pattern: MaskedLiteral, ctrl: Ctrl.E): Unit = {
        decoderService.add(
          pattern, 
          List(
            BITMANIP_ENABLE          -> True,
            BITMANIP_CTRL            -> B(ctrl.position, 4 bits),
            REGFILE_WRITE_VALID      -> True, //Enable the register file write
            BYPASSABLE_EXECUTE_STAGE -> True, //Notify the hazard management unit that the instruction result is already accessible in the EXECUTE stage (Bypass ready)
            BYPASSABLE_MEMORY_STAGE  -> True, //Same as above but for the memory stage
            RS1_USE                  -> True, //Notify the hazard management unit that this instruction use the RS1 value
            RS2_USE                  -> True  //Same than above but for RS2.
          )
        )
      }

      add(ANDN,  Ctrl.OP_ANDN)
      add(ORN,   Ctrl.OP_ORN)
      add(XNOR,  Ctrl.OP_XNOR)
      add(ROL,   Ctrl.OP_ROL)
      add(ROR,   Ctrl.OP_ROR)
      add(RORI,  Ctrl.OP_RORI)
      add(PACK,  Ctrl.OP_PACK)
      add(PACKH, Ctrl.OP_PACKH)
      add(REV8,  Ctrl.OP_REV8)
      add(BREV8, Ctrl.OP_BREV8)
      add(ZIP,   Ctrl.OP_ZIP)
      add(UNZIP, Ctrl.OP_UNZIP)
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._

    execute plug new Area {
      import execute._

      val rs1 = input(RS1).asUInt
      val rs2 = input(RS2).asUInt
      val shamtImm = input(INSTRUCTION)(24 downto 20).asUInt 
      val shamtRs2 = rs2(4 downto 0)

      val logicRes = UInt(32 bits)
      switch(input(BITMANIP_CTRL)) {
        is(B(Ctrl.OP_ANDN.position, 4 bits)) { logicRes := rs1 & ~rs2 }
        is(B(Ctrl.OP_ORN.position, 4 bits))  { logicRes := rs1 | ~rs2 }
        is(B(Ctrl.OP_XNOR.position, 4 bits)) { logicRes := rs1 ^ ~rs2 }
        default                              { logicRes := 0 }
      }

      val rotateRes = UInt(32 bits)
      switch(input(BITMANIP_CTRL)) {
        is(B(Ctrl.OP_ROL.position, 4 bits))  { rotateRes := rs1.rotateLeft(shamtRs2) }
        is(B(Ctrl.OP_ROR.position, 4 bits))  { rotateRes := rs1.rotateRight(shamtRs2) }
        is(B(Ctrl.OP_RORI.position, 4 bits)) { rotateRes := rs1.rotateRight(shamtImm) }
        default                              { rotateRes := 0 }
      }

      val packRes = UInt(32 bits)
      switch(input(BITMANIP_CTRL)) {
        is(B(Ctrl.OP_PACK.position, 4 bits)) { 
           packRes := rs2(15 downto 0) @@ rs1(15 downto 0)
        }
        is(B(Ctrl.OP_PACKH.position, 4 bits)) { 
           packRes := U(0, 16 bits) @@ rs2(7 downto 0) @@ rs1(7 downto 0)
        }
        default { packRes := 0 }
      }

      val revRes = UInt(32 bits)
      switch(input(BITMANIP_CTRL)) {
        is(B(Ctrl.OP_REV8.position, 4 bits)) {
          revRes := rs1(7 downto 0) @@ rs1(15 downto 8) @@ rs1(23 downto 16) @@ rs1(31 downto 24)
        }
        is(B(Ctrl.OP_BREV8.position, 4 bits)) {
          val b0_rev = Cat(rs1(0), rs1(1), rs1(2), rs1(3), rs1(4), rs1(5), rs1(6), rs1(7))
          val b1_rev = Cat(rs1(8), rs1(9), rs1(10), rs1(11), rs1(12), rs1(13), rs1(14), rs1(15))
          val b2_rev = Cat(rs1(16), rs1(17), rs1(18), rs1(19), rs1(20), rs1(21), rs1(22), rs1(23))
          val b3_rev = Cat(rs1(24), rs1(25), rs1(26), rs1(27), rs1(28), rs1(29), rs1(30), rs1(31))

          revRes := Cat(b3_rev, b2_rev, b1_rev, b0_rev).asUInt
        }
        default { revRes := 0 }
      }

      val permRes = UInt(32 bits)
      val zipCalc = UInt(32 bits)
      val unzipCalc = UInt(32 bits)
      
      for(i <- 0 until 16) {
         zipCalc(2*i)     := rs1(i)
         zipCalc(2*i + 1) := rs1(i + 16)
      }
      for(i <- 0 until 16) {
         unzipCalc(i)      := rs1(2*i)
         unzipCalc(i + 16) := rs1(2*i + 1)
      }

      switch(input(BITMANIP_CTRL)) {
        is(B(Ctrl.OP_ZIP.position, 4 bits))   { permRes := zipCalc }
        is(B(Ctrl.OP_UNZIP.position, 4 bits)) { permRes := unzipCalc }
        default { permRes := 0 }
      }

      when(input(BITMANIP_ENABLE)) {
        switch(input(BITMANIP_CTRL)) {
           is(B(Ctrl.OP_ANDN.position, 4 bits), B(Ctrl.OP_ORN.position, 4 bits), B(Ctrl.OP_XNOR.position, 4 bits)) {
             output(REGFILE_WRITE_DATA) := logicRes.asBits
           }
           is(B(Ctrl.OP_ROL.position, 4 bits), B(Ctrl.OP_ROR.position, 4 bits), B(Ctrl.OP_RORI.position, 4 bits)) {
             output(REGFILE_WRITE_DATA) := rotateRes.asBits
           }
           is(B(Ctrl.OP_PACK.position, 4 bits), B(Ctrl.OP_PACKH.position, 4 bits)) {
             output(REGFILE_WRITE_DATA) := packRes.asBits
           }
           is(B(Ctrl.OP_REV8.position, 4 bits), B(Ctrl.OP_BREV8.position, 4 bits)) {
             output(REGFILE_WRITE_DATA) := revRes.asBits
           }
           is(B(Ctrl.OP_ZIP.position, 4 bits), B(Ctrl.OP_UNZIP.position, 4 bits)) {
             output(REGFILE_WRITE_DATA) := permRes.asBits
           }
        }
      }
    }
  }
}