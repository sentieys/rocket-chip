// See LICENSE.SiFive for license details.

package uncore.devices

import Chisel._
import junctions._
import util._
import regmapper._
import tile.XLen
import uncore.tilelink2._
import config._


/** Constant values used by both Debug Bus Response & Request
  */

object DbBusConsts{

  def dbDataSize = 32

  def dbOpSize = 2
  def db_OP_NONE            = UInt("b00")
  def db_OP_READ            = UInt("b01")
  def db_OP_WRITE           = UInt("b10")

  def dbRespSize = 2
  def db_RESP_SUCCESS     = UInt("b00")
  def db_RESP_FAILURE     = UInt("b01")
  def db_RESP_HW_FAILURE  = UInt("b10")
  // This is used outside this block
  // to indicate 'busy'.
  def db_RESP_RESERVED    = UInt("b11")

}

object DsbBusConsts {

  def sbAddrWidth = 12
  def sbIdWidth   = 10 

}

object DsbRegAddrs{

  // These may need to move around to be used by the serial interface.

  // These are used by the ROM.
  def HALTED       = 0x100
  def GOING        = 0x10C
  def RESUMING     = 0x110
  def EXCEPTION    = 0x114

  def GO           = 0x400

  def ROMBASE      = 0x800

  def WHERETO      = 0x900
  def ABSTRACT     = 0x910
  def PROGBUF      = 0xA00

  // This shows up in HartInfo
  def DATA         = 0xB00

  //Not implemented: Serial.
 
}

/** Enumerations used both in the hardware
  * and in the configuration specification.
  */

object DebugModuleAccessType extends scala.Enumeration {
  type DebugModuleAccessType = Value
  val Access8Bit, Access16Bit, Access32Bit, Access64Bit, Access128Bit = Value
}
import DebugModuleAccessType._

object DebugModuleHartStatus extends scala.Enumeration {
  type DebugModuleHartStatus = Value
  val Halted, Running, Unavailable, NonExistent = Value
}
import DebugModuleHartStatus._


/** Parameters exposed to the top-level design, set based on
  * external requirements, etc.
  *
  *  This object checks that the parameters conform to the 
  *  full specification. The implementation which receives this
  *  object can perform more checks on what that implementation
  *  actually supports.
  *  nComponents : The number of components to support debugging.
  *  nDebugBusAddrSize : Size of the Debug Bus Address
  *  nAbstractDataWords: 
  *  nProgamBufferWords: 
  *  hasBusMaster: Whethr or not a bus master should be included
  *    The size of the accesses supported by the Bus Master. 
  *  nSerialPorts : Number of serial ports to instantiate
  *  authType : The Authorization Type
  *  Number of cycles to assert ndreset when pulsed. 
  **/


case class DebugModuleConfig (
  nComponents        : Int,
  nDebugBusAddrSize  : Int,
  nProgBufferWords   : Int,
  nAbstractDataWords : Int,
  hasBusMaster : Boolean,
  hasAccess128 : Boolean,
  hasAccess64  : Boolean,
  hasAccess32  : Boolean,
  hasAccess16  : Boolean,
  hasAccess8   : Boolean,
  nSerialPorts : Int,
  supportQuickAccess : Boolean,
  nNDResetCycles : Int
) {

  if (hasBusMaster == false){
    require (hasAccess128 == false)
    require (hasAccess64  == false)
    require (hasAccess32  == false)
    require (hasAccess16  == false)
    require (hasAccess8   == false)
  }

  require (nSerialPorts <= 8)

  require ((nDebugBusAddrSize >= 5) && (nDebugBusAddrSize <= 7))

  //TODO: Revisit these.
  private val maxComponents = nDebugBusAddrSize match {
    case 5 => (32*4)
    case 6 => (32*32)
    case 7 => (32*32)
  }
  require (nComponents > 0 && nComponents <= maxComponents)

  //TODO: Revisit.
  private val maxRam = nDebugBusAddrSize match {
    case 5 => (4 * 16)
    case 6 => (4 * 16)
    case 7 => (4 * 64)
  }

  require (nNDResetCycles > 0)

  // TODO: Check that quick access requirements are met.
}

class DefaultDebugModuleConfig (val ncomponents : Int, val xlen:Int)
    extends DebugModuleConfig(
      nComponents = ncomponents,
      nDebugBusAddrSize = 5,
      //TODO: what should these values be.
      nProgBufferWords   =  8,
      nAbstractDataWords = xlen match{
        case 32  => 1 
        case 64  => 2
        case 128 => 4
      },
      hasBusMaster = false,
      hasAccess128 = false, 
      hasAccess64 = false, 
      hasAccess32 = false, 
      hasAccess16 = false, 
      hasAccess8 = false, 
      nSerialPorts = 0,
      nNDResetCycles = 1,
      supportQuickAccess = false
    )

case object DMKey extends Field[DebugModuleConfig]

// *****************************************
// Module Interfaces
// 
// *****************************************


/** Structure to define the contents of a Debug Bus Request
  */

class DebugBusReq(addrBits : Int) extends Bundle {
  val addr = UInt(width = addrBits)
  val data = UInt(width = DbBusConsts.dbDataSize)
  val op   = UInt(width = DbBusConsts.dbOpSize)

  override def cloneType = new DebugBusReq(addrBits).asInstanceOf[this.type]
}

/** Structure to define the contents of a Debug Bus Response
  */
class DebugBusResp( ) extends Bundle {
  val data = UInt(width = DbBusConsts.dbDataSize)
  val resp = UInt(width = DbBusConsts.dbRespSize)

}

/** Structure to define the top-level DebugBus interface 
  *  of DebugModule.
  *  DebugModule is the consumer of this interface.
  *  Therefore it has the 'flipped' version of this.
  */

class DebugBusIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val req = new  DecoupledIO(new DebugBusReq(p(DMKey).nDebugBusAddrSize))
  val resp = new DecoupledIO(new DebugBusResp).flip()
}

class AsyncDebugBusIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val req  = new AsyncBundle(1, new DebugBusReq(p(DMKey).nDebugBusAddrSize))
  val resp = new AsyncBundle(1, new DebugBusResp).flip
}

object FromAsyncDebugBus
{
  def apply(x: AsyncDebugBusIO) = {
    val out = Wire(new DebugBusIO()(x.p))
    out.req <> FromAsyncBundle(x.req)
    x.resp <> ToAsyncBundle(out.resp, 1)
    out
  }
}

object ToAsyncDebugBus
{
  def apply(x: DebugBusIO) = {
    val out = Wire(new AsyncDebugBusIO()(x.p))
    out.req <> ToAsyncBundle(x.req, 1)
    x.resp <> FromAsyncBundle(out.resp)
    out
  }
}

trait HasDebugModuleParameters {
  implicit val p: Parameters
  val cfg = p(DMKey)
}

/** Debug Module I/O, with the exclusion of the RegisterRouter
  *  Access interface.
  */

trait DebugModuleBundle extends Bundle with HasDebugModuleParameters {
  val db = new DebugBusIO()(p).flip()
  val debugInterrupts = Vec(cfg.nComponents, Bool()).asOutput
  val debugUnavail    = Vec(cfg.nComponents, Bool()).asInput
  val ndreset         = Bool(OUTPUT)
}

// *****************************************
// The Module 
// 
// *****************************************

/** Parameterized version of the Debug Module defined in the
  *  RISC-V Debug Specification 
  *  
  *  DebugModule is a slave to two masters:
  *    The Debug Bus (DMI) -- implemented as a generic Decoupled IO with request
  *                           and response channels
  *    The System Bus -- implemented as generic RegisterRouter
  *  
  *  DebugModule is responsible for holding registers, RAM, and ROM
  *      to support debug interactions, as well as driving interrupts
  *      to a configurable number of components in the system.
  *      It must also maintain a state machine to track the state of harts
  *      being debugged.
  *      It is also responsible for some reset lines.
  */

trait DebugModule extends Module with HasDebugModuleParameters with HasRegMap {

  val io: DebugModuleBundle

  //--------------------------------------------------------------
  // Import constants for shorter variable names
  //--------------------------------------------------------------

  import DMI_RegAddrs._
  import DsbRegAddrs._
  import DsbBusConsts._
  import DbBusConsts._

  //--------------------------------------------------------------
  // Sanity Check Configuration For this implementation.
  //--------------------------------------------------------------

  //TODO: is this requiredment still true.
  require (cfg.nComponents <= 128)
  require (cfg.nSerialPorts == 0)
  require (cfg.hasBusMaster == false)
  // ??? require((DbBusConsts.dbRamWordBits % 8) == 0)

  //--------------------------------------------------------------
  // Register & Wire Declarations
  //--------------------------------------------------------------

  val debugIntRegs = Reg(init=Vec.fill(cfg.nComponents){Bool(false)})
  val haltedRegs  = Reg(init=Vec.fill(cfg.nComponents){Bool(false)})

  val numHaltedStatus = ((cfg.nComponents - 1) / 32) + 1

  val haltedStatus   = Wire(Vec(numHaltedStatus, Bits(width = 32)))
  val rdHaltedStatus = Wire(Bits(width = 32))

  val haltedSummary = Cat(haltedStatus.map(_.orR).reverse)

  //--------------------------------------------------------------
  // DMI Registers 
  //--------------------------------------------------------------

  //----DMCONTROL

  val DMCONTROLReset = (new DMI_DMCONTROLFields()).fromBits(0.U)
  DMCONTROLReset.authenticated := true.B // Not implemented
  DMCONTROLReset.version       := 1.U
  val DMCONTROLReg = Reg(init = DMCONTROLReset)

  val DMCONTROLRdData = DMCONTROLReg
  when (DMCONTROLReg.hartsel >== UInt(cfg.nComponents)) {
    DMCONTROLRdData.hartstatus := DebugModuleHartStatus.Nonexistent
  } .elsewhen (haltRegs(DMCONTROLReg.hartsel)) {
    DMCONTROLRdData.hartstatus := DebugModuleHartStatus.Halted
  } .elsewhen(io.debugUnavail(DMCONTROLReg.hartsel)) {
    DMCONTROLRdData.hartstatus := DebugModuleHartStatus.Unavailable
  } .otherwise {
    DMCONTROLRdData.hartstatus := DebugModuleHartStatus.Running
  }

  val ndresetCtrReg = Reg(UInt(cfg.nNDResetCycles), init = 0.U)

  val DMCONTROLWrData = (new DMCONTROLFields()).fromBits(dbReq.data)
  val DMCONTROLWrEn   = dbWrEn & (dbReq.addr === DMI_DMCONTROL)

  when (~DMCONTROLReg.dmactive) {
    DMCONTROLReg := DMCONTROLReset.reset
  } .otherwise {
    DMCONTROLReg.reset     := DMCONTROLWrData.reset
    DMCONTROLReg.haltreq   := DMCONTROLWrData.haltreq
    DMCONTROLReg.resumereq := DMCONTROLWrData.resumereq
  }

  // Put this last to override its own effects.
  when (DMCTONROLWrEn) {
    DMCONTROLReg.dmactive := DMCONTROLWrData.dmactive
  }

  //----HARTINFO

  val DMHARTINFORdData = (new DMHARTINFOFields()).fromBits(0.U)
  HARTINFORdData.dataaccess  := true.B
  HARTINFORdData.datasize    := UInt(cfg.nAbstractDataWords)
  HARTINFORdData.dataaddr    := UInt(0x400)//TODO--where.

  //----HALTSUM

  val HALTSUMRdData = (new DMI_HALTSUMFields()).fromBits(haltSummary)

  //----ABSTRACTCS
  val ABSTRACTCSReset = (new ABSTRACTCSFields()).fromBits(0.U)
  ABSTRACTCSReset.datacount := UInt(cfg.nAbstractDataWords)

  val ABSTRACTCSReg = RegInit(ABSTRACTCSReset)
  val ABSTRACTCSWrData = (new ABSTRACTCSFields()).fromBits(dbReq.data)
  val ABSTRACTCSRdData = ABSTRACTCSReg

  when(~DMCONTROL.dmactive){
    ABSTRACTCSReg := ABSTRACTCSReset
  }.otherwise {

    when (ABSTRACTCSWrEn){
      // TODO -- check to make sure it is legal to write this register now.
      when (ABSTRACTCSWrData.cmderr === 0.U) {
        ABSTRACTCSReg.cmderr := 0.U
      }
      if (cfg.nAbstractData > 0) ABSTRACTCSReg.autoexec0 := ABSTRACTCSWrData.autoexec0
      if (cfg.nAbstractData > 1) ABSTRACTCSReg.autoexec1 := ABSTRACTCSWrData.autoexec1
      if (cfg.nAbstractData > 2) ABSTRACTCSReg.autoexec2 := ABSTRACTCSWrData.autoexec2
      if (cfg.nAbstractData > 3) ABSTRACTCSReg.autoexec3 := ABSTRACTCSWrData.autoexec3
      if (cfg.nAbstractData > 4) ABSTRACTCSReg.autoexec4 := ABSTRACTCSWrData.autoexec4
      if (cfg.nAbstractData > 5) ABSTRACTCSReg.autoexec5 := ABSTRACTCSWrData.autoexec5
      if (cfg.nAbstractData > 6) ABSTRACTCSReg.autoexec6 := ABSTRACTCSWrData.autoexec6
      if (cfg.nAbstractData > 7) ABSTRACTCSReg.autoexec7 := ABSTRACTCSWrData.autoexec7
    }
  }

  // TODO: ABSTRACTCSReg.busy
  // TODO: ABSTRACTCSReg.cmderr

  //---- COMMAND

  val COMMANDReset = (new COMMANDFields()).fromBits(0.U)
  val COMMANDReg = RegInit(COMMANDReset)

  val COMMANDWrData = (new COMMANDFields()).fromBits(dbReq.data)

  when (~DMCONTROL.dmactive) {
    COMMANDReg := COMMANDReset
  }.otherwise {
    when (COMMANDWrEn) {
      //TODO : Check that it is legal to write this register right now.
      COMMANDReg := COMMANDWrData
    }
  }
  // --- System Bus Registers

  val hartHaltedWrEn       = Wire(Bool())
  val hartHaltedId         = Wire(UInt(width = sbIdWidth))
  val hartGoingWrEn        = Wire(Bool())
  val hartGoingId          = Wire(UInt(width = sbIdWidth))
  val hartResumingWrEn     = Wire(Bool())
  val hartResumingId       = Wire(UInt(width = sbIdWidth))
  val hartExceptionWrEn    = Wire(Bool())
  val hartExceptionId      = Wire(UInt(width = sbIdWidth))

  // --- Abstract Data

  val abstractDataWidth     = 32
  val abstractDataAddrWidth = log2Up(cfg.nAbstractDataWords)

  // These are byte addressible, s.t. the Processor can use
  // byte-addressible instructions to store to them.
  val abstractDataMem       = Reg(init = Vec.fill(cfg.nAbstractDataWords*4){UInt(0, width = 8)})

  val dbAbstractDataIdx        = Wire(UInt(width=abstractDataAddrWidth))
  val dbAbstractDataIdxValid   = Wire(Bool())
  val dbAbstractDataRdData      = Wire (UInt(32.W))
  val dbAbstractDataWrData      = Wire(UInt(32.W))
  val dbAbstractDataWrEn        = Wire(Bool())
  val dbAbstractDataRdEn        = Wire(Bool())
  val dbAbstractDataWrEnFinal   = Wire(Bool())
  val dbAbstractDataRdEnFinal   = Wire(Bool())

  val dbAbstractDataOffset = log2Up(dbAbstractDataWidth/8)

  // --- Program Buffer

  val PROGBUFRdData = (new DMI_PROGBUFCSFields).fromBits(0.U)
  PROGBUFRdData.progsize := UInt(cfg.nProgramBufferWords)

  val programBufferDataWidth  = 32
  val programBufferAddrWidth = log2Up(cfg.nProgramBufferWords)

  val programBufferMem    = Reg(init = Vec.fill(cfg.nProgramBufferWords*4){UInt(0, width = 8)})

  val dbProgramBufferIdx   = Wire(UInt(width=programBufferAddrWidth))
  val dbProgramBufferIdxValid   = Wire(Bool())
  val dbProgramBufferRdData = Wire (UInt(32.W))
  val dbProgramBufferWrData = Wire(UInt(32.W))
  val dbProgramBufferWrEn   = Wire(Bool())
  val dbProgramBufferRdEn   = Wire(Bool())
  val dbProgramBufferWrEnFinal   = Wire(Bool())
  val dbProgramBufferRdEnFinal   = Wire(Bool())

  val dbProgramBufferOffset = log2Up(dbProgramBufferWidth/8)

// --- Debug Bus Accesses

  val dbRdEn   = Wire(Bool())
  val dbWrEn   = Wire(Bool())
  val dbRdData = Wire(UInt(width = DbBusConsts.dbDataSize))

  val s_DB_READY :: s_DB_RESP :: Nil = Enum(Bits(), 2)
  val dbStateReg = Reg(init = s_DB_READY)

  val dbResult  = Wire(io.db.resp.bits)

  val dbReq     = Wire(io.db.req.bits)
  val dbRespReg = Reg(io.db.resp.bits) 

  //--------------------------------------------------------------
  // Interrupt Registers
  //--------------------------------------------------------------
  
  for (component <- 0 until cfg.nComponents) {
    io.debugInterrupts(component) := debugIntRegs(component)
  }

  // Halt request registers are written by write to DMCONTROL.haltreq
  // and cleared by writes to DMCONTROL.resumereq.
  
  for (component <- 0 until cfg.nComponents) {
    when (~DMCONTROL.dmactive) {
      debugIntRegs(component) := false.B
    }. otherwise {
      when (DMCONTROLWrEn) {
        when (DMCONTROLWrData.hartid === UInt(component)) {
          debugIntRegs(component) := (debugIntRegs(component) | DMCONTROLWrData.haltreq) &
          ~(DMCONTROLWrData.resumereq)
        }
      }
    }
  }

  //--------------------------------------------------------------
  // These bits are implementation-specific bits set
  // by harts executing code.
  //--------------------------------------------------------------

  for (component <- 0 until cfg.nComponents) {
    when (~DMCONTROL.dmactive) {
      haltedRegs(component) := false.B
    }.otherwise {
      when (hartHaltedWrEn) {
        when (hartHaltedWrId === UInt(component, width = sbIdWidth)) {
          haltedRegs(component) := true.B
        }
      }.elsewhen (hartResumingWrEn) {
        when (hartResumingWrId == UInt(component, width=sbIdWidth)) {
          haltedRegs(component) := false.B
        }
      }
    }
  }

  for (ii <- 0 until numHaltedStatus) {
    haltedStatus(ii) := Cat(haltedRegs.slice(ii * 32, (ii + 1) * 32).reverse)
  }

 
  //--------------------------------------------------------------
  // Abstract Data Access (Debug Bus ... System Bus can override)
  //--------------------------------------------------------------

  //TODO: Make this more efficient with the final addresses to use masking
  // instead of actual comparisons.
  dbReq := io.db.req.bits
  dbAbstractDataIdx       := dbReq.addr - UInt(DMI_DATA0)
  dbAbstractDataIdxValid := (dbReq.addr >== UInt(DMI_DATA0)) && (dbReq.addr <== (UInt(DMI_DATA0) + UInt(cfg.nAbstractDataWords)))

  val dbAbstractDataFields = List.tabulate(cfg.nAbstractDataWords) { ii =>
    val slice = abstractDataMem.slice(ii * 4, (ii+1)*4)
    slice.reduce[UInt]{ case (x: UInt, y: UInt) => Cat(y, x) }
  }

  when (dbWrEn & dbAbstractDataIdxValid) {
    for (ii <- 0 until 4) {
      abstractDataMem((dbAbstractDataIdx << 2) + UInt(ii)) := dbReq.data((8*(ii+1)-1), (8*ii))
    }
  }

  dbAbstractDataRdData := dbAbstractDataFields(dbAbstractDataIdx)

  //--------------------------------------------------------------
  // Program Buffer Access (Debug Bus ... System Bus can override)
  //--------------------------------------------------------------

  //TODO: Make this more efficient with the final addresses to use masking
  // instead of actual comparisons.

  dbProgramBufferIdx       := dbReq.addr - UInt(DMI_PROGBUF0)
  dbProgramBufferIdxValid := (dbReq.addr >= UInt(DMI_PROGBUF0)) && dbReq.addr <= (UInt(DMI_PROGBUF0) + UInt(cfg.nProgramBufferWords))

  val dbProgramBufferFields = List.tabulate(cfg.nProgramBufferWords) { ii =>
    val slice = programBufferMem.slice(ii * 4, (ii+1)*4)
    slice.reduce[UInt]{ case (x: UInt, y: UInt) => Cat(y, x)}
  }

  dbProgramBufferRdData := dbProgramBufferFields(dbProgramBufferIdx)

  when (dbWrEn & dbProgramBufferDataIdxValid) {
    for (ii <- 0 until 4) {
      programBufferMem((dbProgramBufferIdx << 2) + UInt(ii)) := dbReq.data((8*(ii+1)-1), (8*ii))
    }
  }



  //--------------------------------------------------------------
  // DMI Access
  //--------------------------------------------------------------

  // -----------------------------------------
  // DB Access Write Decoder
  
  DMCONTROLWrEn  := dbWrEn & (dbWrAddr == DMI_DMCONTROL)
  HARTINFOWrEn   := dbWrEn & (dbWrAddr == DMI_HARTINFO)
  ABSTRACTCSWrEn := dbWrEn & (dbWrAddr == DMI_ABSTRACTCS)
  COMMANDWrEn    := dbWrEn & (dbWrAddr == DMI_COMMAND)
  //TODO: use the fact that the addresses are aligned.
  abstractDataWrEn = Wire(Vec(cfg.nAbstractDataWords, Bool()))
  abstractDataWrEn.zipwithindex.foreach {
    case (wen, ii) => wen := dbWrEn & (dbWrAddr == (DMI_DATA0 + ii))
  }
  PROGBUFCSWrEn := dbWrEn & (dbWrAddr == DMI_PROGBUFCS)
  //TODO: use the fact that the addresses are aligned.

  programBufferWrEn = Wire(Vec(cfg.nProgramBufferWords, Bool()))
  programBufferWrEn.zipWithIndex.foreach {
    case (wen, ii) => wen := dbWrEn & (dbWrAddr == (DMI_PROGBUF0 + ii))
  }

    elsewhen (dbReq.addr === DMCONTROL) {
    DMCONTROLWrEn  := dbWrEn
  }.otherwise {
    //Other registers/RAM are Not Implemented.
  }

  // -----------------------------------------
  // DB Access Read Mux
  when     (dbRdAddr == UInt(DMI_DMCONTROL))  {dbRdData := DMCONTROLRdData}
    .elsewhen (dbRdAddr == UInt(DMI_HARTINFO))   {dbRdData := HARTINFORdData}
    .elsewhen (dbRdAddr == UInt(DMI_HALTSUM))    {dbRdData := HALTSUMRdData}
    .elsewhen (dbRdAddr == UInt(DMI_ABSTRACTCS)) {dbRdData := ABSTRACTCSRdData}
    .elsewhen (dbRdAddr == UInt(DMI_COMMAND))    {dbRdData := COMMANDRdData}
    .elsewhen (dbAbstractDataIdxValid)           {dbRdData := dbAbstractDataRdData}
    .elsewhen (dbProgBufferDataIdxValid)         {dbRdData := dbProgramBufferRdData}
    .otherwise {dbRdData := 0.U}

  // There is no way to return failure without SB or Serial, which are not
  // implemented yet.
  dbResult.resp := db_RESP_SUCCESS
  dbResult.data := dbRdData

  // -----------------------------------------
  // DB Access State Machine Decode (Combo)
  io.db.req.ready := (dbStateReg === s_DB_READY) ||
  (dbStateReg === s_DB_RESP && io.db.resp.fire())

  io.db.resp.valid := (dbStateReg === s_DB_RESP)
  io.db.resp.bits  := dbRespReg

  dbRdEn := io.db.req.fire()
  dbWrEn := (io.db.req.op == db_OP_WRITE) && io.db.req.fire()

  // -----------------------------------------
  // DB Access State Machine Update (Seq)

  when (dbStateReg === s_DB_READY){
    when (io.db.req.fire()){
      dbStateReg := s_DB_RESP
      dbRespReg := dbResult
    }
  } .elsewhen (dbStateReg === s_DB_RESP){
    when (io.db.req.fire()){
      dbStateReg := s_DB_RESP
      dbRespReg := dbResult
    }.elsewhen (io.db.resp.fire()){
      dbStateReg := s_DB_READY
    }
  }

  //--------------------------------------------------------------
  // Debug ROM
  //--------------------------------------------------------------

  // See the debug directory for contents and scripts to generate this.

  val debugRomContents : Array[Byte] = Array (
    0x6f, 0x00, 0xc0, 0x00, 0x6f, 0x00, 0x80, 0x02, 0x6f, 0x00, 0x80, 0x03,
    0x0f, 0x00, 0xf0, 0x0f, 0x73, 0x10, 0x24, 0x7b, 0x73, 0x24, 0x40, 0xf1,
    0x23, 0x20, 0x80, 0x10, 0x03, 0x04, 0x04, 0x40, 0x63, 0x1a, 0x80, 0x00,
    0x73, 0x24, 0x40, 0xf1, 0x6f, 0xf0, 0x5f, 0xff, 0x23, 0x26, 0x00, 0x10,
    0x73, 0x00, 0x10, 0x00, 0x73, 0x24, 0x20, 0x7b, 0x23, 0x22, 0x00, 0x10,
    0x67, 0x00, 0x00, 0x90, 0x23, 0x24, 0x00, 0x10, 0x73, 0x00, 0x20, 0x7b
  )

  val romRegFields  =  debugRomContents.get.map( x => RegField.r(8, UInt(x.toInt & 0xFF)))

  //--------------------------------------------------------------
  // "Variable" ROM
  //--------------------------------------------------------------
  // f09ff06f                j       808 <resume>
  // 00c0006f                j       910 <abstract>
  // 0f80006f                j       a00 <prog_buffer>

  val whereToReg = Reg(UInt(32.W))
  //TODO: whereToReg logic.

  val goBits = RegInit(Vec(cfg.nComponents, false.B))

  //TODO: goBits logic.

  // TODO: abstractGeneratedInstrucitonLogic.
  val abstractGeneratedInstruction = Reg(32.W)
  val ebreakInstruction = 0x00100073.U

  //--------------------------------------------------------------
  // System Bus Access
  //--------------------------------------------------------------

  // Local reg mapper function : Notify when written, but give the value.
  def wValue (n: Int, value: UInt, set: Bool) : RegField = {
    RegField(n, value, RegWriteFn((valid, data) => {set := valid ; value := data; Bool(true)}))
  }

  regmap(
    HALTED      -> Seq(wValue(sbIdWidth, HALTEDWrData, HALTEDWrEn)),
    GOING       -> Seq(wValue(sbIdWidth, GOINGWrData,  GOINGWrEn)),
    RESUMING    -> Seq(wValue(sbIdWidth, RESUMINGWrData,  RESUMINGWrEn)),
    EXCEPTION   -> Seq(wValue(sbIdWidth, EXCEPTIONWrData,  EXCEPTIONWrEn)),

    DATA        -> abstractDataMem.map(x => RegField(8, x)),
    ROMBASE     -> romRegFields,

    GO          -> goBits.map(x => RegField.r(8, x))    
    WHERETO     -> Seq(RegField.r(32, whereToReg)),
    ABSTRACT    -> Seq(RegField.r(32, abstractGeneratedInstruction), RegField.r(32, ebreakInstruction)),
    PROGBUF     -> programBufferMem.map(x => RegFields(8, x))

  )

  //--------------------------------------------------------------
  // Misc. Outputs
  //--------------------------------------------------------------

  // TODO
  io.ndreset   := false.B

}

/** Create a concrete TL2 Slave for the DebugModule RegMapper interface.
  *  
  */
class TLDebugModule(address: BigInt = 0)(implicit p: Parameters)
  extends TLRegisterRouter(address, beatBytes=p(XLen)/8, executable=true)(
  new TLRegBundle((), _ )    with DebugModuleBundle)(
  new TLRegModule((), _, _)  with DebugModule)


/** Synchronizers for DebugBus
  *  
  */


object AsyncDebugBusCrossing {
  // takes from_source from the 'from' clock domain to the 'to' clock domain
  def apply(from_clock: Clock, from_reset: Bool, from_source: DebugBusIO, to_clock: Clock, to_reset: Bool, depth: Int = 1, sync: Int = 3) = {
    val to_sink = Wire(new DebugBusIO()(from_source.p))
    to_sink.req <> AsyncDecoupledCrossing(from_clock, from_reset, from_source.req, to_clock, to_reset, depth, sync)
    from_source.resp <> AsyncDecoupledCrossing(to_clock, to_reset, to_sink.resp, from_clock, from_reset, depth, sync)
    to_sink // is now to_source
  }
}

object AsyncDebugBusFrom { // OutsideClockDomain
  // takes from_source from the 'from' clock domain and puts it into your clock domain
  def apply(from_clock: Clock, from_reset: Bool, from_source: DebugBusIO, depth: Int = 1, sync: Int = 3): DebugBusIO = {
    val scope = AsyncScope()
    AsyncDebugBusCrossing(from_clock, from_reset, from_source, scope.clock, scope.reset, depth, sync)
  }
}

object AsyncDebugBusTo { // OutsideClockDomain
  // takes source from your clock domain and puts it into the 'to' clock domain
  def apply(to_clock: Clock, to_reset: Bool, source: DebugBusIO, depth: Int = 1, sync: Int = 3): DebugBusIO = {
    val scope = AsyncScope()
    AsyncDebugBusCrossing(scope.clock, scope.reset, source, to_clock, to_reset, depth, sync)
  }
}
