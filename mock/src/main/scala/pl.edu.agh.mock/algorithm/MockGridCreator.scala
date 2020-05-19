package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.MockCell
import pl.edu.agh.xinuk.algorithm.GridCreator
import pl.edu.agh.xinuk.model.{Direction, Grid, NonPlanarConnection, NonPlanarConnections}

final class MockGridCreator(implicit config: MockConfig) extends GridCreator {

  override def initialGrid: (Grid, NonPlanarConnections) = {
    val grid = Grid.empty()
    grid.cells(config.gridSize / 4)(config.gridSize / 4) = MockCell.create(config.mockInitialSignal)
    grid.cells(3 * config.gridSize / 4)(config.gridSize / 4) = MockCell.create(config.mockInitialSignal)
    grid.cells(config.gridSize / 4)(3 * config.gridSize / 4) = MockCell.create(config.mockInitialSignal)
    grid.cells(3 * config.gridSize / 4)(3 * config.gridSize / 4) = MockCell.create(config.mockInitialSignal)
    val nonPlanarConnection = NonPlanarConnection((1, config.gridSize - 1), Direction.Right, (1, 0))
    (grid, NonPlanarConnections(Set(nonPlanarConnection, nonPlanarConnection.reversed)))
  }
}

object MockGridCreator {
  def apply(implicit config: MockConfig): MockGridCreator = new MockGridCreator
}