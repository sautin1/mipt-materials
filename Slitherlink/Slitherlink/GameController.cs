using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;

namespace Slitherlink {
    using GridCell = GridPoint;

    public struct GridPoint : IComparable<GridPoint> {
        private int row, col;

        public int Row {
            get { return row; }
            set { row = value; }
        }
        public int Col {
            get { return col; }
            set { col = value; }
        }

        public GridPoint(int row, int col) {
            this.row = row;
            this.col = col;
        }

        public int CompareTo(GridPoint other) {
            if (row == other.row && col == other.col) {
                return 0;
            } else if (row < other.row || row == other.row && col < other.col) {
                return -1;
            } else {
                return 1;
            }
        }
    }

    public struct Edge {
        GridPoint from;
        GridPoint to;

        public GridPoint From {
            get { return from; }
        }

        public GridPoint To {
            get { return to; }
        }

        public Edge(GridPoint from, GridPoint to) {
            if (from.CompareTo(to) > 0) {
                GridPoint tmp = from;
                from = to;
                to = tmp;
            }

            this.from = from;
            this.to = to;
        }

        public bool IsHorizontal() {
            return from.Row == to.Row;
        }
    }

    public struct EdgeInfo {
        public bool isActive;
        public bool isCrossed;
        public bool isWrong;

        public EdgeInfo(bool isActive = false, bool isCrossed = false, bool isWrong = false) {
            this.isActive = isActive;
            this.isCrossed = isCrossed;
            this.isWrong = isWrong;
        }

        public int ToInt() {
            return (Convert.ToInt32(isActive) << 2) | (Convert.ToInt32(isCrossed) << 1) | Convert.ToInt32(isWrong);
        }

        public static EdgeInfo FromInt(int x) {
            return new EdgeInfo(Convert.ToBoolean((x >> 2) & 1), Convert.ToBoolean((x >> 1) & 1), Convert.ToBoolean(x & 1));
        }
    }

    class GameController {
        private IDictionary<Edge, EdgeInfo> edgeInfos;
        private IDictionary<GridCell, int> cellNumbers;
        private IDictionary<GridCell, int> cellEdgesAroundCounter;
        private IDictionary<GridCell, bool> cellNumbersSatisfaction;
        private ISet<GridPoint> openLineEnds;
        private int numbersUnsatisfiedCounter;
        private bool isGameFinished;

        private int rowCount;
        private int colCount;

        // memoization
        private IList<GridCell> gridCells;

        public int RowCount {
            get { return rowCount; }
        }

        public int ColCount {
            get { return colCount; }
        }

        public GameController(int rowCount, int colCount, IDictionary<GridPoint, int> cellNumbers, IDictionary<Edge, EdgeInfo> edgeInfos) {
            this.rowCount = rowCount;
            this.colCount = colCount;
            this.cellNumbers = cellNumbers;
            this.edgeInfos = edgeInfos;
            this.isGameFinished = false;
            this.openLineEnds = new HashSet<GridPoint>(); /**/

            clearEdgesAroundCounter();
            foreach (KeyValuePair<Edge, EdgeInfo> pair in edgeInfos) {
                Edge edge = pair.Key;
                EdgeInfo edgeInfo = pair.Value;
                if (edgeInfo.isActive || edgeInfo.isWrong) {
                    List<GridPoint> adjacentPoints = adjacentGridCells(edge);
                    foreach (GridPoint point in adjacentPoints) {
                        cellEdgesAroundCounter[new GridCell(point.Row, point.Col)] += 1;
                    }
                }
            }
            clearNumberSatisfaction();
            recountNumbersSatisfaction();
        }

        public void ToggleEdgeState(Edge edge, bool isToggleCross) {
            if (isToggleCross) {
                toggleCross(edge);
            } else {
                toggleNonCross(edge);
            }

            checkIsGameFinished(edge);
        }

        public IDictionary<Edge, EdgeInfo> GetEdgeInfos() {
            return edgeInfos;
        }

        public IList<Edge> GetEdgesByInfo(EdgeInfo edgeInfo) {
            return edgeInfos.Where(pair => pair.Value.Equals(edgeInfo)).Select(pair => pair.Key).ToList();
        }

        public IList<Edge> GetEdgesByCrossed(bool isCrossed) {
            return edgeInfos.Where(pair => pair.Value.isCrossed == isCrossed).Select(pair => pair.Key).ToList();
        }

        public IList<Edge> GetEdgesByActive(bool isActive) {
            return edgeInfos.Where(pair => pair.Value.isActive == isActive).Select(pair => pair.Key).ToList();
        }

        public IList<Edge> GetEdgesByWrong(bool isWrong) {
            return edgeInfos.Where(pair => pair.Value.isWrong == isWrong).Select(pair => pair.Key).ToList();
        }

        public IDictionary<GridCell, int> GetNumbers() {
            return cellNumbers;
        }

        public IDictionary<GridCell, bool> GetNumbersSatisfaction() {
            return cellNumbersSatisfaction;
        }

        public IList<GridCell> GetGridCells() {
            if (gridCells == null) {
                gridCells = new List<GridCell>(rowCount * colCount);
                for (int row = 0; row < rowCount; ++row) {
                    for (int col = 0; col < colCount; ++col) {
                        gridCells.Add(new GridCell(row, col));
                    }
                }
            }
            return gridCells;
        }

        public bool IsGameFinished() {
            return isGameFinished;
        }

        public void ClearGame() {
            clearEdgeStates();
            clearNumberSatisfaction();
            clearEdgesAroundCounter();
        }

        //______________________________________________________________________________________________________

        private void clearEdgeStates() {
            foreach (Edge key in edgeInfos.Keys.ToList()) {
                edgeInfos[key] = new EdgeInfo(false, false, false);
            }
        }

        private void clearNumberSatisfaction() {
            cellNumbersSatisfaction = new Dictionary<GridCell, bool>();
            foreach (GridCell cell in GetGridCells()) {
                cellNumbersSatisfaction[cell] = false;
            }
        }

        private void clearEdgesAroundCounter() {
            cellEdgesAroundCounter = new Dictionary<GridCell, int>();
            foreach (GridCell cell in GetGridCells()) {
                cellEdgesAroundCounter[cell] = 0;
            }
        }

        private bool isNumberSatisfied(GridPoint point) {
            return cellNumbers[point] < 0 || cellEdgesAroundCounter[point] == cellNumbers[point];
        }

        private bool isSelfIntersection(GridPoint point) {
            List<Edge> edges = incomingEdges(point);
            edges = edges.Where(
                edge => edgeInfos[edge].isActive
            ).ToList();
            return edges.Count > 2;
        }

        private bool isLinesClosed() {
            return openLineEnds.Count == 0;
        }

        private void checkIsGameFinished(Edge edgeLast) {
            isGameFinished = numbersUnsatisfiedCounter == 0 && !edgeInfos[edgeLast].isWrong;
        }

        private void toggleCross(Edge edge) {
            EdgeInfo edgeInfo = edgeInfos[edge];
            if (edgeInfo.isCrossed) {
                edgeInfo.isCrossed = false;
            } else {
                if (edgeInfo.isActive || edgeInfo.isWrong) {
                    afterEdgeRemoved(edge);
                }
                edgeInfo.isCrossed = true;
            }
            edgeInfo.isWrong = false;
            edgeInfo.isActive = false;
            edgeInfos[edge] = edgeInfo;
        }

        private void toggleNonCross(Edge edge) {
            EdgeInfo edgeInfo = edgeInfos[edge];
            if (edgeInfo.isActive) {
                // active or wrong
                edgeInfo.isActive = false;
                edgeInfo.isWrong = false;
                edgeInfos[edge] = edgeInfo;
                afterEdgeRemoved(edge);
            } else if (edgeInfo.isCrossed) {
                // crossed
                edgeInfo.isCrossed = false;
                edgeInfos[edge] = edgeInfo;
            } else {
                // passive
                edgeInfo.isActive = true;
                edgeInfos[edge] = edgeInfo;
                afterEdgeAdded(edge);
            }
        }

        private bool isEdgeWrong(Edge edge) {
            // check number satisfaction
            List<GridPoint> cells = adjacentGridCells(edge);
            bool isMistake = false;
            foreach (GridPoint cell in cells) {
                isMistake = isMistake || (cellNumbers[cell] >= 0 &&
                    cellNumbers[cell] < cellEdgesAroundCounter[cell]);
            }

            // check self-intersections
            isMistake = isMistake || isSelfIntersection(edge.From) || isSelfIntersection(edge.To);
            // check early closure of line
            isMistake = isMistake || (isLinesClosed() && numbersUnsatisfiedCounter > 0);
            return isMistake;
        }

        private bool tryValidateEdge(Edge edge) {
            if (!edgeInfos[edge].isWrong) {
                return true;
            }
            bool success = isEdgeWrong(edge);
            if (success) {
                EdgeInfo edgeInfo = edgeInfos[edge];
                edgeInfo.isActive = true;
                edgeInfos[edge] = edgeInfo;
            }
            return success;
        }

        private List<GridPoint> adjacentGridCells(Edge edge) {
            List<GridPoint> points = new List<GridPoint>();
            if (edge.IsHorizontal() && edge.From.Row < rowCount || !edge.IsHorizontal() && edge.From.Col < colCount) {
                points.Add(edge.From);
            }
            if (edge.IsHorizontal() && edge.From.Row > 0) {
                points.Add(new GridPoint(edge.From.Row - 1, edge.From.Col));
            } else if (!edge.IsHorizontal() && edge.To.Col > 0) {
                points.Add(new GridPoint(edge.From.Row, edge.From.Col - 1));
            }
            return points;
        }

        private List<Edge> incomingEdges(GridPoint point) {
            List<Edge> edges = surroundingEdges(point);
            edges.AddRange(surroundingEdges(new GridPoint(point.Row - 1, point.Col - 1)));
            edges = edges.Where(edge => edgeInfos.ContainsKey(edge) && (edge.From.Equals(point) || edge.To.Equals(point))).ToList();
            return edges;
        }

        private List<Edge> surroundingEdges(GridPoint point) {
            List<Edge> edges = new List<Edge>() {
                new Edge(point, new GridPoint(point.Row, point.Col + 1)), // up  , horizontal
                new Edge(point, new GridPoint(point.Row + 1, point.Col)), // left, vertical
                new Edge(new GridPoint(point.Row + 1, point.Col), new GridPoint(point.Row + 1, point.Col + 1)), // down , horizontal
                new Edge(new GridPoint(point.Row, point.Col + 1), new GridPoint(point.Row + 1, point.Col + 1))  // right, vertical
            };
            return edges;
        }

        private void updateSurroundingEdges(GridCell cell) {
            List<Edge> edges = surroundingEdges(cell);
            foreach (Edge edge in edges) {
                if (edgeInfos[edge].isWrong && !isEdgeWrong(edge)) {
                    EdgeInfo edgeInfo = edgeInfos[edge];
                    edgeInfo.isWrong = false;
                    edgeInfos[edge] = edgeInfo;
                }
            }
        }

        private void updateOpenLineEnds(Edge edge, bool isAdded) {
            List<GridPoint> openEnds = new List<GridPoint>();
            if (openLineEnds.Contains(edge.From)) {
                openEnds.Add(edge.From);
            }
            if (openLineEnds.Contains(edge.To)) {
                openEnds.Add(edge.To);
            }

            if (openEnds.Count == 0) {
                openLineEnds.Add(edge.From);
                openLineEnds.Add(edge.To);
            } else if (openEnds.Count == 1) {
                openLineEnds.Remove(openEnds[0]);
                openLineEnds.Add(edge.From.Equals(openEnds[0]) ? edge.To : edge.From);
            } else {
                openLineEnds.Remove(edge.From);
                openLineEnds.Remove(edge.To);
            }
        }

        // should be called always after an edge has been added
        private void afterEdgeAdded(Edge edge) {
            List<GridPoint> cells = adjacentGridCells(edge);
            foreach (GridPoint cell in cells) {
                ++cellEdgesAroundCounter[cell];
                bool wasNumberSatisfied = cellNumbersSatisfaction[cell];
                cellNumbersSatisfaction[cell] = isNumberSatisfied(cell);
                if (wasNumberSatisfied && !cellNumbersSatisfaction[cell]) {
                    ++numbersUnsatisfiedCounter;
                } else if (!wasNumberSatisfied && cellNumbersSatisfaction[cell]) {
                    --numbersUnsatisfiedCounter;
                }
            }
            updateOpenLineEnds(edge, true);
            EdgeInfo edgeInfo = edgeInfos[edge];
            if (isEdgeWrong(edge)) {
                edgeInfo.isWrong = true;
            }
            edgeInfos[edge] = edgeInfo;
        }

        // should be called always after an edge has been removed
        private void afterEdgeRemoved(Edge edge) {
            updateOpenLineEnds(edge, false);
            List<GridPoint> points = adjacentGridCells(edge);
            foreach (GridPoint point in points) {
                --cellEdgesAroundCounter[point];
                bool wasNumberSatisfied = cellNumbersSatisfaction[point];
                cellNumbersSatisfaction[point] = isNumberSatisfied(point);
                if (wasNumberSatisfied && !cellNumbersSatisfaction[point]) {
                    ++numbersUnsatisfiedCounter;
                } else if (!wasNumberSatisfied && cellNumbersSatisfaction[point]) {
                    --numbersUnsatisfiedCounter;
                }
                updateSurroundingEdges(point);
            }
        }

        private void recountNumbersSatisfaction() {
            foreach (GridCell cell in GetGridCells()) {
                cellNumbersSatisfaction[cell] = isNumberSatisfied(cell);
                if (!cellNumbersSatisfaction[cell]) {
                    ++numbersUnsatisfiedCounter;
                }
            }
        }
    }
}
