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
                return isEdgeCorrectSimple(edge) && (!isLineClosed(edge) || numbersUnsatisfiedCounter == 0);
            }

            private bool isEdgeCorrectSimple(Edge edge) {
                bool isNumberNotViolated = true;
                List<GridCell> cells = controller.adjacentGridCells(edge);
                foreach (GridCell cell in cells) {
                    isNumberNotViolated &= !isCellNumberViolated(cell);
                }
                return isNumberNotViolated && !isSelfIntersection(edge);
            }

            private bool isCellNumberViolated(GridCell cell) {
                return controller.cellNumbers[cell] >= 0 && cellEdgesAroundCounter[cell] > controller.cellNumbers[cell];
            }

            private bool isCellNumberSatisfied(GridCell cell) {
                return controller.cellNumbers[cell] < 0 || cellEdgesAroundCounter[cell] == controller.cellNumbers[cell];
            }

            private bool isSelfIntersection(Edge edge) {
                return isSelfIntersection(edge.From) || isSelfIntersection(edge.To);
            }

            private bool isSelfIntersection(GridPoint point) {
                IList<Edge> edges = incomingEdgesActive(point);
                return edges.Count > 2;
            }

            private bool isLineClosed(Edge edge) {
                IDictionary<GridPoint, bool> isVisited = new Dictionary<GridPoint, bool>();
                return dfsLineClosed(edge.From, edge.From, isVisited);
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

            private List<Edge> incomingEdges(GridPoint point) {
                List<Edge> edges = controller.surroundingEdges(point);
                edges.AddRange(controller.surroundingEdges(new GridPoint(point.Row - 1, point.Col - 1)));
                edges = edges.Where(edge =>
                    controller.edgeInfos.ContainsKey(edge) && (edge.From.Equals(point) || edge.To.Equals(point))
                ).ToList();
                return edges;
            }

            private List<Edge> incomingEdgesActive(GridPoint point) {
                List<Edge> edges = incomingEdges(point);
                edges = edges.Where(edge => controller.edgeInfos[edge].isActive).ToList();
                return edges;
            }

            private bool dfsLineClosed(GridPoint point, GridPoint parent, IDictionary<GridPoint, bool> isVisited) {
                isVisited[point] = true;
                bool lineClosed = false;
                foreach (Edge edge in incomingEdgesActive(point)) {
                    if (!edge.Equals(new Edge(point, parent))) {
                        GridPoint pointTo = edge.From.Equals(point) ? edge.To : edge.From;
                        lineClosed = lineClosed || isVisited.ContainsKey(pointTo) && isVisited[pointTo];
                        if (lineClosed) {
                            break;
                        } else {
                            lineClosed = dfsLineClosed(pointTo, point, isVisited);
                            EdgeInfo edgeInfo = controller.EdgeInfoByEdge(edge);
                            if (!lineClosed && isEdgeCorrectSimple(edge) && edgeInfo.isWrong) {
                                edgeInfo.isWrong = false;
                                controller.edgeInfos[edge] = edgeInfo;
                            }
                        }
                    }
                }

                return lineClosed;
            }

        }
    }
}
