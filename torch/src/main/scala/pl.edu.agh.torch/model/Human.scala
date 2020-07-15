package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.model.GridPart.SmellMap
import pl.edu.agh.xinuk.model.{EmptyCell, GridPart}

final case class HumanCell(signal: SmellMap, crowd: List[HumanCell], speed: Int)(implicit config: TorchConfig) extends GridPart {

  override type Self = HumanCell

  override def withSmell(smell: SmellMap): HumanCell = copy(signal = smell)
}

trait HumanAccessible[+T <: GridPart] {
  def withHuman(crowd: List[HumanCell], speed: Int): T
}

object HumanAccessible {

  def unapply(arg: GridPart)(implicit config: TorchConfig): Option[HumanAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: EscapeCell => Some(unapply(cell))
    case _ => None
  }

  def unapply(arg: EmptyCell)(implicit config: TorchConfig): HumanAccessible[HumanCell] =
    new HumanAccessible[HumanCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): HumanCell = HumanCell(arg.signalWith(config.humanInitialSignal), crowd, speed)
    }

  def unapply(arg: EscapeCell): HumanAccessible[EscapeCell] =
    new HumanAccessible[EscapeCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): EscapeCell = EscapeCell(arg.signal)
    }
}
