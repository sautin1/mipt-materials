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
                onEdgeEvent(edge, null, true);
                //updateOpenLineEnds(edge, true);
            }

            // should be called always when an edge is being removed
            public void OnEdgeRemoved(Edge edge, IDictionary<Edge, EdgeInfo> edgeInfos) {
                //updateOpenLineEnds(edge, false);
                onEdgeEvent(edge, edgeInfos, false);
            }

            // Sections with target functionality of this class

            public bool IsEdgeCorrect(Edge edge) {
                bool isNumberSatisfied = true;
                List<GridCell> cells = adjacentGridCells(edge);
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
                return isSelfIntersection(edge.From) || isSelfIntersection(edge.To);
            }

            private bool isSelfIntersection(GridPoint point) {
                IList<Edge> edges = incomingEdgesActiveNonCrossed(point);
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
                        List<GridCell> adjacentCells = adjacentGridCells(edge);
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

            private void onEdgeEvent(Edge edge, IDictionary<Edge, EdgeInfo> edgeInfos, bool isAdded) {
                List<GridCell> cells = adjacentGridCells(edge);
                foreach (GridCell cell in cells) {
                    cellEdgesAroundCounter[cell] += isAdded ? 1 : -1;
                    updateCellSatisfaction(cell);
                }
                if (!isAdded && edgeInfos != null) {
                    foreach (GridCell cell in adjacentGridCells(edge)) {
                        updateSurroundingEdges(cell, edgeInfos);
                    }
                    updateReachableEdges(edge, edgeInfos);
                }
            }

            private List<GridPoint> adjacentGridCells(Edge edge) {
                List<GridCell> cells = new List<GridCell>();
                if (edge.IsHorizontal() && edge.From.Row < controller.RowCount ||
                    !edge.IsHorizontal() && edge.From.Col < controller.ColCount) {
                    cells.Add(edge.From);
                }
                if (edge.IsHorizontal() && edge.From.Row > 0) {
                    cells.Add(new GridCell(edge.From.Row - 1, edge.From.Col));
                } else if (!edge.IsHorizontal() && edge.To.Col > 0) {
                    cells.Add(new GridCell(edge.From.Row, edge.From.Col - 1));
                }
                return cells;
            }

            private List<Edge> surroundingEdges(GridCell cell) {
                List<Edge> edges = new List<Edge>() {
                    new Edge(cell, new GridCell(cell.Row, cell.Col + 1)), // up  , horizontal
                    new Edge(cell, new GridCell(cell.Row + 1, cell.Col)), // left, vertical
                    new Edge(new GridCell(cell.Row + 1, cell.Col), new GridCell(cell.Row + 1, cell.Col + 1)), // down , horizontal
                    new Edge(new GridCell(cell.Row, cell.Col + 1), new GridCell(cell.Row + 1, cell.Col + 1))  // right, vertical
                };
                return edges;
            }

            private List<Edge> incomingEdges(GridPoint point) {
                List<Edge> edges = surroundingEdges(point);
                edges.AddRange(surroundingEdges(new GridPoint(point.Row - 1, point.Col - 1)));
                edges = edges.Where(edge => 
                    controller.edgeInfos.ContainsKey(edge) && (edge.From.Equals(point) || edge.To.Equals(point))
                ).ToList();
                return edges;
            }

            private List<Edge> incomingEdgesActiveNonCrossed(GridPoint point) {
                List<Edge> edges = incomingEdges(point);
                edges = edges.Where(
                    edge => controller.edgeInfos[edge].isActive && !controller.edgeInfos[edge].isCrossed
                ).ToList();
                return edges;
            }

            private void tryValidateEdge(Edge edge, IDictionary<Edge, EdgeInfo> edgeInfos) {
                EdgeInfo edgeInfo = controller.edgeInfos[edge];
                if (edgeInfo.isWrong && IsEdgeCorrect(edge)) {
                    edgeInfo.isWrong = false;
                    edgeInfos[edge] = edgeInfo;
                }
            }

            private void updateSurroundingEdges(GridCell cell, IDictionary<Edge, EdgeInfo> edgeInfos) {
                List<Edge> edges = surroundingEdges(cell);
                foreach (Edge edge in edges) {
                    tryValidateEdge(edge, edgeInfos);
                }
            }

            private void updateReachableEdges(Edge edge, IDictionary<Edge, EdgeInfo> edgeInfos) {
                IDictionary<GridPoint, bool> isVisited = new Dictionary<GridPoint, bool>();
                dfsVerifyEdges(edge.From, edge.From, isVisited, edgeInfos);
            }

            private bool dfsVerifyEdges(GridPoint point, GridPoint parent, 
                IDictionary<GridPoint, bool> isVisited, 
                IDictionary<Edge, EdgeInfo> edgeInfos) {

                isVisited[point] = true;
                bool lineClosed = false;
                foreach (Edge edge in incomingEdgesActiveNonCrossed(point)) {
                    if (!edge.Equals(new Edge(point, parent))) {
                        GridPoint pointTo = edge.From.Equals(point) ? edge.To : edge.From;
                        if (isVisited.ContainsKey(pointTo) && isVisited[pointTo]) {
                            break;
                        } else {
                            lineClosed = dfsVerifyEdges(pointTo, point, isVisited, edgeInfos);
                            EdgeInfo edgeInfo = edgeInfos[edge];
                            if (edgeInfo.isWrong != lineClosed) {
                                edgeInfo.isWrong = lineClosed;
                                edgeInfos[edge] = edgeInfo;
                            }
                        }
                    }
                }
                return lineClosed;
            }

            private bool dfsLineClosed(GridPoint point, GridPoint parent, IDictionary<GridPoint, bool> isVisited) {
                isVisited[point] = true;
                bool lineClosed = false;
                foreach (Edge edge in incomingEdgesActiveNonCrossed(point)) {
                    if (!edge.Equals(new Edge(point, parent))) {
                        GridPoint pointTo = edge.From.Equals(point) ? edge.To : edge.From;
                        lineClosed = lineClosed || isVisited.ContainsKey(pointTo) && isVisited[pointTo];
                        if (lineClosed) {
                            break;
                        } else {
                            lineClosed = dfsLineClosed(pointTo, point, isVisited);
                        }
                    }
                }

                return lineClosed;
            }

        }
    }
}
