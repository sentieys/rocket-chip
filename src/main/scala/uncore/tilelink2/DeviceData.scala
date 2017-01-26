package uncore.tilelink2

import config.Parameters
import coreplex.CoreplexParameters
import diplomacy.LazyModule
import tile.XLen
import uncore.devices.{CoreplexLocalInterrupter, TLPLIC}


/** JSON-like data structure that only supports dictionaries ("items") or
 *  primitives as values.
 *
 *  Should roughly correspond to config string data.
 */
sealed trait DeviceData
case class DeviceDataString(val string: String) extends DeviceData
case class DeviceDataInt(val int: BigInt) extends DeviceData
case class DeviceDataHex(val hex: BigInt) extends DeviceData
case class DeviceDataItems(val items: Seq[(String, DeviceData)]) extends DeviceData {
  def ++(other: DeviceDataItems): DeviceDataItems = {
    DeviceDataItems(items ++ other.items)
  }
}
object DeviceDataItems {
  // DummyImplicit is required for this to have a different type signature than
  // the default constructor because Seq[Foo] and Foo* have the same type
  // signature after erasure.
  // http://stackoverflow.com/a/9229677
  def apply(seq: (String, DeviceData)*)(implicit x: DummyImplicit): DeviceDataItems = new DeviceDataItems(seq)
}


/** Indicates the top of a TileLink device and has the ability to extract
 *  DeviceData information when given a set of TileLink node parameters.
 */
trait TLDeviceTop {
  this: LazyModule =>
  /** Extract device data out of TileLink node parameters.
   *
   *  By default will label all interrupts with intbase and intsize and all
   *  addresses with addr and size, duplicating keys if there are more than
   *  IntSourceNode or TLManagerNode under this device top.
   *
   *  Can be overridden to provide a custom information or more specific labels.
   *
   *  @param interruptParams a Seq of IntSourceParameters under this module
   *  @param addressParams a Seq of TLManagerParameters under this module
   *  @return a single-item DeviceDataItems object mapping the name of the
   *          device to a second-level DeviceDataItems object containing other
   *          parameters.
   */
  def getDeviceData(
      interruptParams: Seq[IntSourceParameters],
      addressParams: Seq[TLManagerParameters]): DeviceDataItems = {
    DeviceDataItems(
      name -> DeviceDataItems(
        interruptParams.map { param =>
          Seq(
            "intbase" -> DeviceDataInt(param.range.start),
            "intsize" -> DeviceDataInt(param.range.size)
          )
        }.flatten ++
        addressParams.map { param =>
          param.address.map { address =>
            Seq(
              "addr" -> DeviceDataHex(address.base),
              "size" -> DeviceDataHex(address.mask + 1)
            )
          }.flatten
        }.flatten
      )
    )
  }
}


object DeviceDataUtils {
  /** Get DeviceData from TileLink interrupt and manager node parameters.
   *
   *  First group IntSourceParameters and TLManagerParameters by TLDeviceTop,
   *  then ask each TLDeviceTop to extract the DeviceData from their parameters.
   */
  def getDeviceData(
      interruptParams: Seq[IntSourceParameters],
      addressParams: Seq[TLManagerParameters]
    )(implicit p: Parameters): DeviceData = {

    type ParamsMap = Map[TLDeviceTop, (Seq[IntSourceParameters], Seq[TLManagerParameters])]

    // Gather all interrupt and address parameters by device top.
    val interruptParamsByDeviceTop: ParamsMap =
      interruptParams
        .groupBy{params => locateDeviceTop(params.nodePath.last.lazyModule)}
        .mapValues{params => (params, Seq[TLManagerParameters]())}
    val addressParamsByDeviceTop: ParamsMap =
      addressParams
        .groupBy{params => locateDeviceTop(params.nodePath.last.lazyModule)}
        .mapValues{params => (Seq[IntSourceParameters](), params)}
    // Combine the previous two maps
    val paramsByDeviceTop: ParamsMap = {
      (interruptParamsByDeviceTop.toSeq ++ addressParamsByDeviceTop.toSeq)
        .groupBy(_._1)
        .mapValues {
          case paramsSeq => paramsSeq.foldLeft((Seq[IntSourceParameters](), Seq[TLManagerParameters]())) {
            case ((intAcc, addressAcc), (_, (intParams, addressParams))) => (intAcc ++ intParams, addressAcc ++ addressParams)
          }
        }
    }
    val peripheralData = paramsByDeviceTop.map {
      case (top, (interruptParameters, addressParameters)) =>
        top.getDeviceData(interruptParameters, addressParameters)
    }.reduce(_++_)

    // Get device tops in order to compute per-hart core data.
    val clint = {
      val matches = paramsByDeviceTop.keys.filter(_.isInstanceOf[CoreplexLocalInterrupter]).toSeq
      require(matches.length == 1, s"There must be exactly one CoreplexLocalInterrupter in the design. Found: ${matches.length}")
      matches(0).asInstanceOf[CoreplexLocalInterrupter]
    }
    val tlplic = {
      val matches = paramsByDeviceTop.keys.filter(_.isInstanceOf[TLPLIC]).toSeq
      require(matches.length == 1, s"There must be exactly one TLPLIC in the design. Found: ${matches.length}")
      matches(0).asInstanceOf[TLPLIC]
    }
    val coreData = getCoreDeviceData(clint, tlplic)

    peripheralData ++ coreData
  }

  /** Format deviceData as a config string.
   *
   *  DeviceDataHexs are formatted in hex with leading 0x, e.g. 0xdeadb33f.
   *  The top-level DeviceDataItem lacks curly braces and its keys are not
   *  indented.
   */
  def formatConfigString(dd: DeviceData): String = {
    def sort(dd: DeviceData): DeviceData = {
      dd match {
        case DeviceDataItems(items) => DeviceDataItems(items.sortWith(_._1 < _._1).map {
          case (key, value) => (key, sort(value))
        })
        case _ => dd
      }
    }

    def formatHelper(dd: DeviceData, indentationLevel: Int = 0): String = {
      dd match {
        case DeviceDataString(s) => s ++ ";\n"
        case DeviceDataInt(i) => s"$i;\n"
        case DeviceDataHex(h) => s"0x${h.toString(16)};\n"
        case DeviceDataItems(seq) =>
          // Unlike JSON, do not surround the top-most dictionary with curly braces.
          val open = if (indentationLevel > 0) "{\n" else ""
          val body = seq.map { case (key, value) =>
            "  " * (indentationLevel) + key + " " + formatHelper(value, indentationLevel + 1)
          }.mkString
          val close = if (indentationLevel > 0) "  " * (indentationLevel - 1) + "};\n" else ""
          open ++ body ++ close
      }
    }

    formatHelper(sort(dd)) + '\u0000'
  }

  /** Follow the parent lazy modules until we find a TLDeviceTop. */
  private def locateDeviceTop(lazyModule: LazyModule): TLDeviceTop = {
    def locateHelper(lazyModule: LazyModule): Option[TLDeviceTop] = {
      lazyModule match {
        case deviceTop: TLDeviceTop => Some(deviceTop)
        case _: LazyModule => lazyModule.parent match {
          case Some(parent) => locateHelper(parent)
          case None => None
        }
      }
    }
    locateHelper(lazyModule).getOrElse(
      throw new RuntimeException(s"${lazyModule.name} not contained within a TLDeviceTop")
    )
  }

  private def getCoreDeviceData(clint: CoreplexLocalInterrupter, tlplic: TLPLIC)(implicit p: Parameters): DeviceDataItems = {
    val c = CoreplexParameters()(p)

    DeviceDataItems("core" -> c.tilesParams.zipWithIndex.map { case (t, i) =>
      val isa = {
        val m = if (t.core.mulDiv.nonEmpty) "m" else ""
        val a = if (t.core.useAtomics) "a" else ""
        val f = if (t.core.fpu.nonEmpty) "f" else ""
        val d = if (t.core.fpu.nonEmpty && p(XLen) > 32) "d" else ""
        val c = if (t.core.useCompressed) "c" else ""
        val s = if (t.core.useVM) "s" else ""
        s"rv${p(XLen)}i$m$a$f$d$c$s"
      }
      DeviceDataItems(
        i.toString -> DeviceDataItems(
          "0" -> (
            DeviceDataItems("isa" -> DeviceDataString(isa)) ++
            clint.getHartDeviceData(i) ++
            tlplic.getHartDeviceData(i)
          )
        )
      )
    }.reduce(_++_))
  }
}
