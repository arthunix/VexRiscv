package vexriscv.plugin

import spinal.core._
import spinal.lib._
import vexriscv.{DecoderService, Stageable, VexRiscv}

class BitManipPlugin extends Plugin[VexRiscv] {
  // --- Pipeline Signals ---
  object ZBK_OP_CODE    extends Stageable(Bits(5 bits))
  object ZBK_INSN_VALID extends Stageable(Bool())

  object Ctrl extends SpinalEnum {
    val OP_ANDN, OP_ORN, OP_XNOR,          // Zbkb
        OP_ROL, OP_ROR, OP_RORI,           // Zbkb
        OP_PACK, OP_PACKH,                 // Zbkb
        OP_REV8, OP_BREV8,                 // Zbkb
        OP_ZIP, OP_UNZIP,                  // Zbkb
        OP_CLMUL, OP_CLMULH,               // Zbkc (Carry-less Multiply)
        OP_XPERM8, OP_XPERM4               // Zbkx (Crossbar Permutation)
        = newElement()
  }

  override def setup(pipeline: VexRiscv): Unit = {
    import pipeline.config._

    val decoderService = pipeline.service(classOf[DecoderService])

    decoderService.addDefault(ZBK_INSN_VALID, False)
    
    def add(pattern: MaskedLiteral, ctrl: Ctrl.E): Unit = {
      decoderService.add(
        pattern,
        List(
          ZBK_INSN_VALID           -> True,
          ZBK_OP_CODE              -> B(ctrl.position, 5 bits),
          REGFILE_WRITE_VALID      -> True,
          BYPASSABLE_EXECUTE_STAGE -> True,
          BYPASSABLE_MEMORY_STAGE  -> True,
          RS1_USE                  -> True,
          RS2_USE                  -> True
        )
      )
    }

    // --- Zbb / Zbkb (Bitmanipulation) Encodings ---
    add(M"0100000----------111-----0110011", Ctrl.OP_ANDN)
    add(M"0100000----------110-----0110011", Ctrl.OP_ORN)
    add(M"0100000----------100-----0110011", Ctrl.OP_XNOR)
    
    add(M"0110000----------001-----0110011", Ctrl.OP_ROL)
    add(M"0110000----------101-----0110011", Ctrl.OP_ROR)
    add(M"0110000----------101-----0010011", Ctrl.OP_RORI)
    
    add(M"0000100----------100-----0110011", Ctrl.OP_PACK)
    add(M"0000100----------111-----0110011", Ctrl.OP_PACKH)
    
    add(M"011010011000-----101-----0010011", Ctrl.OP_REV8)
    add(M"011010000111-----101-----0010011", Ctrl.OP_BREV8)
    
    add(M"000010001111-----001-----0010011", Ctrl.OP_ZIP)
    add(M"000010001111-----101-----0010011", Ctrl.OP_UNZIP)

    // --- Zbkc (Carry-less Multiply) Encodings ---
    // Funct7 = 0000101 for all CLMUL variants
    add(M"0000101----------001-----0110011", Ctrl.OP_CLMUL)
    add(M"0000101----------011-----0110011", Ctrl.OP_CLMULH)

    // --- Zbkx (Crossbar Permutation) Encodings ---
    // Funct7 = 0010100 for all XPERM variants
    add(M"0010100----------100-----0110011", Ctrl.OP_XPERM8)
    add(M"0010100----------010-----0110011", Ctrl.OP_XPERM4)
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
      switch(input(ZBK_OP_CODE)) {
        is(B(Ctrl.OP_ANDN.position, 5 bits)) { logicRes := rs1 & ~rs2 }
        is(B(Ctrl.OP_ORN.position, 5 bits))  { logicRes := rs1 | ~rs2 }
        is(B(Ctrl.OP_XNOR.position, 5 bits)) { logicRes := rs1 ^ ~rs2 }
        default                              { logicRes := 0 }
      }

      val rotateRes = UInt(32 bits)
      switch(input(ZBK_OP_CODE)) {
        is(B(Ctrl.OP_ROL.position, 5 bits))  { rotateRes := rs1.rotateLeft(shamtRs2) }
        is(B(Ctrl.OP_ROR.position, 5 bits))  { rotateRes := rs1.rotateRight(shamtRs2) }
        is(B(Ctrl.OP_RORI.position, 5 bits)) { rotateRes := rs1.rotateRight(shamtImm) }
        default                              { rotateRes := 0 }
      }

      val packRes = UInt(32 bits)
      switch(input(ZBK_OP_CODE)) {
        is(B(Ctrl.OP_PACK.position, 5 bits))  { packRes := rs2(15 downto 0) @@ rs1(15 downto 0) }
        is(B(Ctrl.OP_PACKH.position, 5 bits)) { packRes := U(0, 16 bits) @@ rs2(7 downto 0) @@ rs1(7 downto 0) }
        default                               { packRes := 0 }
      }

      val revRes = UInt(32 bits)
      switch(input(ZBK_OP_CODE)) {
        is(B(Ctrl.OP_REV8.position, 5 bits)) {
           revRes := rs1(7 downto 0) @@ rs1(15 downto 8) @@ rs1(23 downto 16) @@ rs1(31 downto 24)
        }
        is(B(Ctrl.OP_BREV8.position, 5 bits)) {
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
         unzipCalc(i)      := rs1(2*i)
         unzipCalc(i + 16) := rs1(2*i + 1)
      }
      switch(input(ZBK_OP_CODE)) {
        is(B(Ctrl.OP_ZIP.position, 5 bits))   { permRes := zipCalc }
        is(B(Ctrl.OP_UNZIP.position, 5 bits)) { permRes := unzipCalc }
        default                               { permRes := 0 }
      }
      
      val clmulRes = UInt(32 bits)
      val clmulRows = Vec(Bits(64 bits), 32)
      val rs1Bits = rs1.asBits.resize(64)
      
      for(i <- 0 until 32) {
          val shifted = rs1Bits << i 
          val shifted64 = shifted.resize(64) 
          clmulRows(i) := (rs2(i) ? shifted64 | B(0, 64 bits))
      }
      
      val clmulFull = clmulRows.reduce(_ ^ _)

      switch(input(ZBK_OP_CODE)) {
         is(B(Ctrl.OP_CLMUL.position, 5 bits))  { clmulRes := clmulFull(31 downto 0).asUInt }
         is(B(Ctrl.OP_CLMULH.position, 5 bits)) { clmulRes := clmulFull(63 downto 32).asUInt }
         default                                { clmulRes := 0 }
      }

      val xpermRes = UInt(32 bits)

      // XPERM8: Byte-wise lookup
      val xperm8Res = Bits(32 bits)
      val rs1Bytes = rs1.asBits.subdivideIn(8 bits)
      val rs2Bytes = rs2.asBits.subdivideIn(8 bits)
      
      for(i <- 0 until 4) {
          val idx = rs1Bytes(i).asUInt
          val outOfBounds = idx(7 downto 2) =/= 0
          val lookupIdx   = idx(1 downto 0) 

          when(outOfBounds) {
              xperm8Res(i*8 + 7 downto i*8) := 0
          } otherwise {
              xperm8Res(i*8 + 7 downto i*8) := rs2Bytes(lookupIdx)
          }
      }

      // XPERM4: Nibble-wise lookup
      val xperm4Res = Bits(32 bits)
      val rs1Nibbles = rs1.asBits.subdivideIn(4 bits)
      val rs2Nibbles = rs2.asBits.subdivideIn(4 bits)
      
      for(i <- 0 until 8) {
          val idx = rs1Nibbles(i).asUInt
          val outOfBounds = idx(3) 
          val lookupIdx   = idx(2 downto 0)

          when(outOfBounds) {
              xperm4Res(i*4 + 3 downto i*4) := 0
          } otherwise {
              xperm4Res(i*4 + 3 downto i*4) := rs2Nibbles(lookupIdx)
          }
      }

      switch(input(ZBK_OP_CODE)) {
          is(B(Ctrl.OP_XPERM8.position, 5 bits)) { xpermRes := xperm8Res.asUInt }
          is(B(Ctrl.OP_XPERM4.position, 5 bits)) { xpermRes := xperm4Res.asUInt }
          default                                { xpermRes := 0 }
      }

      when(input(ZBK_INSN_VALID)) {
        switch(input(ZBK_OP_CODE)) {
          is(B(Ctrl.OP_ANDN.position, 5 bits), B(Ctrl.OP_ORN.position, 5 bits), B(Ctrl.OP_XNOR.position, 5 bits)) {
            output(REGFILE_WRITE_DATA) := logicRes.asBits
          }
          is(B(Ctrl.OP_ROL.position, 5 bits), B(Ctrl.OP_ROR.position, 5 bits), B(Ctrl.OP_RORI.position, 5 bits)) {
            output(REGFILE_WRITE_DATA) := rotateRes.asBits
          }
          is(B(Ctrl.OP_PACK.position, 5 bits), B(Ctrl.OP_PACKH.position, 5 bits)) {
            output(REGFILE_WRITE_DATA) := packRes.asBits
          }
          is(B(Ctrl.OP_REV8.position, 5 bits), B(Ctrl.OP_BREV8.position, 5 bits)) {
            output(REGFILE_WRITE_DATA) := revRes.asBits
          }
          is(B(Ctrl.OP_ZIP.position, 5 bits), B(Ctrl.OP_UNZIP.position, 5 bits)) {
            output(REGFILE_WRITE_DATA) := permRes.asBits
          }
          is(B(Ctrl.OP_CLMUL.position, 5 bits), B(Ctrl.OP_CLMULH.position, 5 bits)) {
            output(REGFILE_WRITE_DATA) := clmulRes.asBits
          }
          is(B(Ctrl.OP_XPERM8.position, 5 bits), B(Ctrl.OP_XPERM4.position, 5 bits)) {
            output(REGFILE_WRITE_DATA) := xpermRes.asBits
          }
        }
      }
    }
  }
}