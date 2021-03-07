package pl.edu.agh.urban.algorithm

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.urban.algorithm.UrbanUpdate._
import pl.edu.agh.urban.config.{Serialization, TargetType, TileType, TileTypeId, UrbanConfig}
import pl.edu.agh.urban.model._
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridDirection}

final case class UrbanPlanCreator() extends PlanCreator[UrbanConfig] with LazyLogging {
  private val noop: (Plans, UrbanMetrics) = (Plans.empty, UrbanMetrics.empty)

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourContents: Map[Direction, CellContents])
                          (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    if (config.pathCreation != "None") {
      noop
    } else {
      move(iteration, cellId.asInstanceOf[GridCellId], cellState.contents.asInstanceOf[UrbanCell], neighbourContents.asInstanceOf[Map[Direction, UrbanCell]])
    }
  }

  private def move(iteration: Long, cellId: GridCellId, contents: UrbanCell, neighbourContents: Map[Direction, UrbanCell])
                  (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    val timeState = config.getTimeState(iteration)

    var allMoves: Seq[(Plans, UrbanMetrics)] = Seq()

    if (timeState.markerPurgeIteration) {
      allMoves = allMoves :+ checkAndPurgeMarkers(cellId, contents, timeState.markerRound)
    }
    if (timeState.personMovementIteration) {
      allMoves = allMoves ++ contents.occupants.map(person => handlePerson(cellId, person, timeState.time, timeState.markerRound, contents.markers, neighbourContents))
      allMoves = allMoves ++ contents.entrances.map(entrance => handleEntrance(cellId, entrance, timeState.time, timeState.markerRound))
    }
    allMoves = allMoves ++ contents.markers.map(marker => handleMarker(marker, timeState.markerRound, neighbourContents))

    allMoves.reduceOption[(Plans, UrbanMetrics)] {
      case ((firstPlans, firstMetrics), (secondPlans, secondMetrics)) =>
        (firstPlans ++ secondPlans, firstMetrics + secondMetrics)
    }.getOrElse(noop)
  }

  private def randomFrom[A](list: Seq[A])(implicit config: UrbanConfig): A = {
    list(config.random.nextInt(list.size))
  }

  private def randomTargetByType(targetType: TargetType)(implicit config: UrbanConfig): Option[(String, TravelMode)] = {
    Some(randomFrom(config.targetTypeToTargets(targetType)).id, TravelMode.Travel)
  }

  private def nearestTargetByType(cellId: GridCellId, targetType: TargetType)
                                 (implicit config: UrbanConfig): Option[(String, TravelMode)] = {
    val targetInfo = config.targetTypeToTargets(targetType)
      .minBy(targetInfo => GridCellId.distance(cellId, targetInfo.center.gridId))
    Some(targetInfo.id, TravelMode.Travel)
  }

  private def findCellByTargetId(targetId: String)(implicit config: UrbanConfig): Option[GridCellId]= {
    config.targets.find(_.id == targetId).map(info => config.getRandomElement(info.entrances))
      .map(_.gridId)
  }

  private def chooseTarget(cellId: GridCellId, targetType: TargetType)
                          (implicit config: UrbanConfig): Option[(String, TravelMode)] = {
    targetType match {
      case TargetType.Parking =>
        nearestTargetByType(cellId, targetType)
      case TargetType.Bike =>
        None // bikers are not treated as pedestrians
      case TargetType.Bus =>
        randomTargetByType(targetType)
      case TargetType.Outside =>
        randomTargetByType(targetType)
      case TargetType.Playground =>
        nearestTargetByType(cellId, targetType)
      case TargetType.Wander =>
        Some(randomFrom(config.targets).id, TravelMode.Wander)
      case TargetType.Residential =>
        randomTargetByType(targetType)
      case TargetType.Service =>
        randomTargetByType(targetType)
      case TargetType.Social =>
        randomTargetByType(targetType)
      case _ =>
        None
    }
  }

  private def handleEntrance(cellId: GridCellId, entrance: Entrance, time: Double, markerRound: Long)
                            (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    val spawningPlans: (Plans, UrbanMetrics) = if (entrance.targetTypes.contains(TargetType.Residential)) {
      config.getTimeOfDay(time).map { currentTimeOfDay =>
        val personSpawnProbability = config.getPersonSpawnProbability(currentTimeOfDay, entrance.population)
        if (config.random.nextDouble() <= personSpawnProbability) {
          val targetDistribution = config.personBehavior.spawnRoutine(currentTimeOfDay).targets
          var targetRand = config.random.nextDouble()
          val targetType = targetDistribution.find { case (_, probability) =>
            targetRand -= probability / 100d
            targetRand <= 0
          }.map(_._1).getOrElse(config.getRandomElement(targetDistribution.keySet.toSeq))

          if (config.random.nextDouble() < config.personBehavior.restrictionFactors(targetType)) {

            if (targetType == TargetType.Wander) {
              chooseTarget(cellId, targetType).map { case (target, travelMode) =>
                val person = travelMode match {
                  case TravelMode.Travel =>
                    throw new RuntimeException("Attempted to generate travel person for TargetType.Wander")
                  case TravelMode.Wander =>
                    Person.wandering(entrance.targetId, List(target), time)
                  case _ =>
                    throw new RuntimeException("Attempted to generate returning person")
                }
                (Plans(None -> Plan(CreatePerson(person, markerRound))), UrbanMetrics.empty)
              }.getOrElse(noop)
            } else {
              val numberOfTargets = config.random.between(1, 4) // it could be in configuration.
              val targets = createTravelTarget(cellId, time, numberOfTargets)
              if (targets.isEmpty) {
                noop
              } else {
                val person = Person.travelling(entrance.targetId, targets)
                (Plans(None -> Plan(CreatePerson(person, markerRound))), UrbanMetrics.empty)
              }
            }
          } else {
            // spawning restriction triggered
            noop
          }
        } else {
          // random spawning not triggered
          noop
        }
      }.getOrElse(noop) // time of day is not spawning interval
    } else {
      // entrance is not residential type and does not have spawning capabilities
      noop
    }

    val leavingPlans = entrance.visitors.map {
      case Visitor(person, returnTime) if time >= returnTime =>
        val returningPerson = person.leaving()
        (Plans(None -> Plan(CreatePerson(returningPerson, markerRound), RemoveVisitor(entrance.targetId, person.id))), UrbanMetrics.empty)
      case _ =>
        noop
    }

    (leavingPlans :+ spawningPlans).reduceOption[(Plans, UrbanMetrics)] {
      case ((firstPlans, firstMetrics), (secondPlans, secondMetrics)) =>
        (firstPlans ++ secondPlans, firstMetrics + secondMetrics)
    }.getOrElse(noop)
  }

  private def createTravelTarget(cellId: GridCellId, time: Double, n: Int)(implicit config: UrbanConfig): List[String] = {
    if (n > 0) {
      val targetOpt = createTravelTarget(cellId, time)
      targetOpt.map(target => {
        target :: findCellByTargetId(target).map(createTravelTarget(_,time, n -1)).getOrElse(Nil)
      }).getOrElse(Nil)
    } else {
      Nil
    }
  }

  private def createTravelTarget(cellId: GridCellId, time: Double)
                                (implicit config: UrbanConfig): Option[String] = {
    val target = config.getTimeOfDay(time).flatMap { currentTimeOfDay =>
      val targetDistribution = config.personBehavior.spawnRoutine(currentTimeOfDay).targets.removed(TargetType.Wander)
      var targetRand = config.random.nextDouble()
      val targetType = targetDistribution.find { case (_, probability) =>
        targetRand -= probability / 100d
        targetRand <= 0
      }.map(_._1).getOrElse(config.getRandomElement(targetDistribution.keySet.toSeq))
      chooseTarget(cellId, targetType).map(_._1)
    }
    target
  }

  private def signalToPercents(signalMap: SignalMap): Map[Direction, Double] = {
    val signalTotal = signalMap.map(_._2).reduceOption(_ + _).map(_.value).getOrElse(0d)
    signalMap.map { case (d, s) => (d, if (signalTotal > 0) s.value / signalTotal else 0d) }
  }

  private val weightedDirections: Map[GridDirection, Map[GridDirection, Double]] = {
    val step = 0.15
    val clockwiseBias = 0.01

    GridDirection.values.map { direction =>
      direction -> Map(direction -> 1d, direction.clockwise -> (1d - step), direction.counterClockwise -> (1d - step - clockwiseBias))
    }.toMap
  }

  private def directionsWeighted(startingDirection: GridDirection): Map[GridDirection, Double] = {
    weightedDirections(startingDirection)
  }

  private def chooseDirection(optimalDirection: Direction, personId: String, markers: Seq[PersonMarker], allowedCells: Map[Direction, UrbanCell])
                             (implicit config: UrbanConfig): Option[Direction] = {
    val allowedDirections = allowedCells.keySet
    val markerDistancePenalty: PartialFunction[Double, Double] = {
      case distance => 0.6 - distance * 0.1
    }
    val groupingFactor: PartialFunction[Direction, Double] = {
      case direction => allowedCells.get(direction).map(it => groupingFactorForCell(it)).getOrElse(1.0)
    }

    val weighted = directionsWeighted(optimalDirection.asInstanceOf[GridDirection])
      .filter { case (d, _) => allowedDirections.contains(d) }

    if (weighted.isEmpty) {
      None
    } else if (!config.avoidMarkers) {
      // best direction
      Some(weighted.maxBy(_._2)._1)
    } else {
      val markerSourceDirections = markers
        .filterNot(_.personId == personId)
        .flatMap(marker => marker.sourceDirections.map(direction => (direction,
          markerDistancePenalty(marker.distance) * groupingFactor(direction))))
        .filter { case (direction, _) => allowedDirections.contains(direction) }
        .toMap

      val finalScoring = weighted.keySet.map(d => d -> (weighted.getOrElse(d, 0d) - markerSourceDirections.getOrElse(d, 0d)))
      Some(finalScoring.maxBy(_._2)._1)
    }
  }
  //decreasing factor of marker based on tileType
  private def groupingFactorForCell(urbanCell: UrbanCell) : Double = {
    urbanCell.tileType.id match {
      case TileTypeId.Crossing | TileTypeId.Stop => 0.8 //maybe it should be configured in config
      case _ => 1.0
    }
  }

  private def movePlans(cellId: CellId, direction: Direction, person: Person, markerRound: Long)
                       (implicit config: UrbanConfig): Plans = {
    Plans(Some(direction) -> Plan(
      AddPerson(person.withAddedDecision(cellId, direction), markerRound),
      RemovePerson(person.id),
      KeepPerson(person, markerRound)
    ))
  }

  private def handlePerson(cellId: GridCellId, person: Person, time: Double, markerRound: Long, markers: Seq[PersonMarker],
                           neighbourContents: Map[Direction, UrbanCell])
                          (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    val (updatedPerson, metrics) = person.travelMode match {
      case TravelMode.Wander =>
        if (time < person.wanderingSegmentEndTime) {
          // stay with current wandering target
          (person, UrbanMetrics.empty)
        } else {
          if (person.wanderingSegmentsRemaining > 0) {
            // change wandering target
            (person.withNewWanderTarget(randomFrom(config.targets).id, time), UrbanMetrics.empty)
          } else {
            // return to source
            (person.leaving(), UrbanMetrics.wanderEnd + UrbanMetrics.returnBeginning)
          }
        }
      case _ =>
        (person, UrbanMetrics.empty)
    }

    val historicalDirections = person.decisionHistory.filter(_._1 == cellId).map(_._2)
    val allowedNeighbours = neighbourContents.filter(_._2.asInstanceOf[UrbanCell].isWalkable)// remove directions to unwalkable cells
    val preferredNeighbours = allowedNeighbours.filterNot(it => historicalDirections.contains(it._1))// remove previously taken decisions
    val staticDirectionOpt = updatedPerson.targets.headOption.map(config.staticPaths).flatMap(_.get(cellId))

    val plans = staticDirectionOpt.map { staticDirection =>
      val chosenDirectionOpt = chooseDirection(staticDirection, updatedPerson.id, markers, preferredNeighbours)
      chosenDirectionOpt.map { chosenDirection =>
        movePlans(cellId, chosenDirection, updatedPerson, markerRound)
      }.getOrElse {
        val altChosenDirectionOpt = chooseDirection(staticDirection, updatedPerson.id, markers, allowedNeighbours)
        altChosenDirectionOpt.map { chosenDirection =>
          logger.warn(s"Forced to repeat historic move from $cellId to ${updatedPerson.targets}: $chosenDirection")
          movePlans(cellId, chosenDirection, updatedPerson, markerRound)
        }.getOrElse {
          logger.warn(s"Could not choose direction from $cellId to ${updatedPerson.targets}")
          Plans(None -> Plan(KeepPerson(updatedPerson, markerRound)))
        }
      }
    }.getOrElse {
      logger.warn(s"No static direction available from $cellId to ${updatedPerson.targets}")
      Plans(None -> Plan(KeepPerson(updatedPerson, markerRound)))
    }

    (plans, metrics)
  }

  private def checkAndPurgeMarkers(cellId: GridCellId, contents: UrbanCell, markerRound: Long)
                                  (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    val plans = Plans(None -> Plan(PurgeMarkers(markerRound)))
    val metrics = contents.occupants.map {
      person =>
        val violatedMarkers = contents.markers.filter(_.personId != person.id)
        if (violatedMarkers.isEmpty) {
          UrbanMetrics.empty
        } else if (violatedMarkers.exists(_.distance <= config.closeViolationThreshold)) {
          UrbanMetrics.closeViolation(cellId)
        } else if (violatedMarkers.exists(_.distance <= config.personalSpaceDetection)) {
          UrbanMetrics.farViolation(cellId)
        } else {
          UrbanMetrics.empty
        }
    }.reduceOption(_ + _).getOrElse(UrbanMetrics.empty)

    (plans, metrics)
  }

  private def handleMarker(marker: PersonMarker, markerRound: Long, neighbourContents: Map[Direction, CellContents])
                          (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    // move only markers that are not at max distance and no older than previous round
    if (marker.round >= markerRound - config.markerMaxAge) {
      val spreadPlans = neighbourContents.keys.flatMap { direction =>
        val spread = marker.spread(direction)
        if (spread.distance <= config.markerMaxDistance) {
          Some(Some(direction) -> Plan(AddMarker(spread)))
        } else {
          None
        }
      }.toSeq
      (Plans(spreadPlans), UrbanMetrics.empty)
    } else {
      noop
    }
  }

  override def finalize(worldShard: WorldShard)(implicit config: UrbanConfig): Unit = {
    if (config.pathCreation != "None") {
      val signal: Map[GridCellId, SignalMap] = worldShard
        .localCellIds
        .map(worldShard.cells(_))
        .map(cell => (cell.id.asInstanceOf[GridCellId], cell.state.signalMap))
        .toMap
      Serialization.dumpStaticSignal(signal, config.pathCreation, worldShard.workerId)
    }
  }
}
