using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Slitherlink {
    using GridCell = GridPoint;

    partial class GameController {
        private class GameVerifier {
            private GameController controller;
            private IDictionary<GridCell, int> cellEdgesAroundCounter;
            //private ISet<GridPoint> openLineEnds;
            private int numbersUnsatisfiedCounter;

            // although this dictionary can be easily generated on the basis of cellEdgesAroundCounter,
            // it is needed to prevent the time-consuming generation at every repainting
            private IDictionary<GridCell, bool> cellNumbersSatisfaction;

            public GameVerifier(GameController controller) {
                this.controller = controller;

                this.cellEdgesAroundCounter = new Dictionary<GridCell, int>();
                this.cellNumbersSatisfaction = new Dictionary<GridCell, bool>();

                calculateEdgesAroundCounter();
                calculateCellNumbersSatisfaction();
                //this.openLineEnds = new HashSet<GridPoint>(); /**/
            }

            // Section of various getters

            public int NumbersUnsatisfiedCount() {
                return numbersUnsatisfiedCounter;
            }

            public IDictionary<GridCell, bool> NumbersSatisfaction() {
                return cellNumbersSatisfaction;
            }

            // Section of class modifying methods
            public void Clear() {
                calculateEdgesAroundCounter();
                calculateCellNumbersSatisfaction();
            }

            // should be called always when an edge is being added
            public void OnEdgeAdded(Edge edge) {
                onEdgeEvent(edge, true);
                //updateOpenLineEnds(edge, true);
            }

            // should be called always when an edge is being removed
            public void OnEdgeRemoved(Edge edge) {
                //updateOpenLineEnds(edge, false);
                onEdgeEvent(edge, false);
            }

            // Sections with target functionality of this class

            public bool IsEdgeCorrect(Edge edge) {
                bool isNumberSatisfied = true;
                List<GridCell> cells = controller.adjacentGridCells(edge);
                foreach (GridCell cell in cells) {
                    isNumberSatisfied &= !isCellNumberViolated(cell);
                }
                return isNumberSatisfied && !isSelfIntersection(edge) && (!isLineClosed(edge) || numbersUnsatisfiedCounter == 0);
            }

            private bool isCellNumberViolated(GridCell cell) {
                return controller.cellNumbers[cell] >= 0 && cellEdgesAroundCounter[cell] > controller.cellNumbers[cell];
            }

            private bool isCellNumberSatisfied(GridCell cell) {
                return controller.cellNumbers[cell] < 0 || cellEdgesAroundCounter[cell] == controller.cellNumbers[cell];
            }

            private bool isSelfIntersection(Edge edge) {
                return false;
            }

            private bool isLineClosed(Edge edge) {
                return false;
            }

            // Section of initializers for private fields

            private void calculateEdgesAroundCounter() {
                foreach (GridCell cell in controller.GridCells()) {
                    cellEdgesAroundCounter[cell] = 0;
                }

                foreach (KeyValuePair<Edge, EdgeInfo> pair in controller.edgeInfos) {
                    Edge edge = pair.Key;
                    EdgeInfo edgeInfo = pair.Value;
                    if (edgeInfo.isActive) {
                        List<GridCell> adjacentCells = controller.adjacentGridCells(edge);
                        foreach (GridPoint cell in adjacentCells) {
                            cellEdgesAroundCounter[cell] += 1;
                        }
                    }
                }
            }

            private void calculateCellNumbersSatisfaction() {
                numbersUnsatisfiedCounter = 0;
                foreach (GridCell cell in controller.GridCells()) {
                    cellNumbersSatisfaction[cell] = isCellNumberSatisfied(cell);
                    if (!cellNumbersSatisfaction[cell]) {
                        ++numbersUnsatisfiedCounter;
                    }
                }
            }

            // Section of a few auxilliary functions

            private void updateCellSatisfaction(GridCell cell) {
                bool wasNumberSatisfied = cellNumbersSatisfaction[cell];
                cellNumbersSatisfaction[cell] = isCellNumberSatisfied(cell);
                if (wasNumberSatisfied && !cellNumbersSatisfaction[cell]) {
                    ++numbersUnsatisfiedCounter;
                } else if (!wasNumberSatisfied && cellNumbersSatisfaction[cell]) {
                    --numbersUnsatisfiedCounter;
                }
            }

            private void onEdgeEvent(Edge edge, bool isAdded) {
                List<GridCell> cells = controller.adjacentGridCells(edge);
                foreach (GridCell cell in cells) {
                    cellEdgesAroundCounter[cell] += isAdded ? 1 : -1;
                    updateCellSatisfaction(cell);
                }
            }

            //private bool isEdgeWrong(Edge edge) {
            //    // check number satisfaction
            //    List<GridPoint> cells = adjacentGridCells(edge);
            //    bool isMistake = false;
            //    foreach (GridPoint cell in cells) {
            //        isMistake = isMistake || (cellNumbers[cell] >= 0 &&
            //            cellNumbers[cell] < cellEdgesAroundCounter[cell]);
            //    }

            //    // check self-intersections
            //    isMistake = isMistake || isSelfIntersection(edge.From) || isSelfIntersection(edge.To);
            //    // check early closure of line
            //    isMistake = isMistake || (isLinesClosed() && numbersUnsatisfiedCounter > 0);
            //    return isMistake;
            //}

            //private bool tryValidateEdge(Edge edge) {
            //    if (!edgeInfos[edge].isWrong) {
            //        return true;
            //    }
            //    bool success = isEdgeWrong(edge);
            //    if (success) {
            //        EdgeInfo edgeInfo = edgeInfos[edge];
            //        edgeInfo.isActive = true;
            //        edgeInfos[edge] = edgeInfo;
            //    }
            //    return success;
            //}

            //private bool isSelfIntersection(GridPoint point) {
            //    List<Edge> edges = incomingEdges(point);
            //    edges = edges.Where(
            //        edge => edgeInfos[edge].isActive
            //    ).ToList();
            //    return edges.Count > 2;
            //}

            //private bool isLinesClosed() {
            //    return openLineEnds.Count == 0;
            //}

            //private List<Edge> incomingEdges(GridPoint point) {
            //    List<Edge> edges = surroundingEdges(point);
            //    edges.AddRange(surroundingEdges(new GridPoint(point.Row - 1, point.Col - 1)));
            //    edges = edges.Where(edge => edgeInfos.ContainsKey(edge) && (edge.From.Equals(point) || edge.To.Equals(point))).ToList();
            //    return edges;
            //}

            //private List<Edge> surroundingEdges(GridPoint point) {
            //    List<Edge> edges = new List<Edge>() {
            //        new Edge(point, new GridPoint(point.Row, point.Col + 1)), // up  , horizontal
            //        new Edge(point, new GridPoint(point.Row + 1, point.Col)), // left, vertical
            //        new Edge(new GridPoint(point.Row + 1, point.Col), new GridPoint(point.Row + 1, point.Col + 1)), // down , horizontal
            //        new Edge(new GridPoint(point.Row, point.Col + 1), new GridPoint(point.Row + 1, point.Col + 1))  // right, vertical
            //    };
            //    return edges;
            //}

            //private void updateOpenLineEnds(Edge edge, bool isAdded) {
            //    List<GridPoint> openEnds = new List<GridPoint>();
            //    if (openLineEnds.Contains(edge.From)) {
            //        openEnds.Add(edge.From);
            //    }
            //    if (openLineEnds.Contains(edge.To)) {
            //        openEnds.Add(edge.To);
            //    }

            //    if (openEnds.Count == 0) {
            //        openLineEnds.Add(edge.From);
            //        openLineEnds.Add(edge.To);
            //    } else if (openEnds.Count == 1) {
            //        openLineEnds.Remove(openEnds[0]);
            //        openLineEnds.Add(edge.From.Equals(openEnds[0]) ? edge.To : edge.From);
            //    } else {
            //        openLineEnds.Remove(edge.From);
            //        openLineEnds.Remove(edge.To);
            //    }
            //}
        }
    }
}
