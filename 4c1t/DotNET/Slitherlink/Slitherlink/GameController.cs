﻿using System;
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

    partial class GameController {
        private IDictionary<Edge, EdgeInfo> edgeInfos;
        private IDictionary<GridCell, int> cellNumbers;

        private GameVerifier verifier;
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
            this.verifier = new GameVerifier(this);
        }

        // Section of various getters

        public IDictionary<Edge, EdgeInfo> EdgeInfos() {
            return edgeInfos;
        }

        public EdgeInfo EdgeInfoByEdge(Edge edge) {
            return edgeInfos[edge];
        }

        public IList<Edge> EdgesByInfo(EdgeInfo edgeInfo) {
            return edgeInfos.Where(pair => pair.Value.Equals(edgeInfo)).Select(pair => pair.Key).ToList();
        }

        public IList<Edge> EdgesByCrossed(bool isCrossed) {
            return edgeInfos.Where(pair => pair.Value.isCrossed == isCrossed).Select(pair => pair.Key).ToList();
        }

        public IList<Edge> EdgesByActive(bool isActive) {
            return edgeInfos.Where(pair => pair.Value.isActive == isActive).Select(pair => pair.Key).ToList();
        }

        public IList<Edge> EdgesByWrong(bool isWrong) {
            return edgeInfos.Where(pair => pair.Value.isWrong == isWrong).Select(pair => pair.Key).ToList();
        }

        public IDictionary<GridCell, int> Numbers() {
            return cellNumbers;
        }

        public IList<GridCell> GridCells() {
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

        public IDictionary<GridCell, bool> NumbersSatisfaction() {
            return verifier.NumbersSatisfaction();
        }

        public bool IsGameFinished() {
            return isGameFinished;
        }

        // Modifications to the game state

        public void ClearGame() {
            clearEdgeInfos();
            verifier.Clear();
            //isGameFinished = checkIsGameFinished();
        }

        public void ToggleEdgeState(Edge edge, bool isToggleCross) {
            if (isToggleCross) {
                toggleCross(edge);
            } else {
                toggleNonCross(edge);
            }

            checkIsGameFinished(edge);
        }

        //__________________________________________________________________________________________

        private void clearEdgeInfos() {
            foreach (Edge key in edgeInfos.Keys.ToList()) {
                edgeInfos[key] = new EdgeInfo(false, false, false);
            }
        }

        private int countEdgeWrong() {
            int edgeWrongCounter = 0;
            foreach (KeyValuePair<Edge, EdgeInfo> pair in edgeInfos) {
                if (pair.Value.isWrong) {
                    ++edgeWrongCounter;
                }
            }
            return edgeWrongCounter;
        }

        private void checkIsGameFinished(Edge edgeLast) {
            isGameFinished = verifier.NumbersUnsatisfiedCount() == 0 && countEdgeWrong() == 0 && verifier.IsLineClosed();
        }

        private void toggleCross(Edge edge) {
            EdgeInfo edgeInfo = edgeInfos[edge];
            bool isRemoved = false;
            if (edgeInfo.isCrossed) {
                edgeInfo.isCrossed = false;
            } else {
                if (edgeInfo.isActive) {
                    // active or wrong
                    isRemoved = true;
                    verifier.OnEdgeRemoved(edge);
                }
                edgeInfo.isCrossed = true;
            }
            edgeInfo.isActive = false;
            edgeInfo.isWrong = false;
            edgeInfos[edge] = edgeInfo;
            if (isRemoved) {
                foreach (GridCell cell in adjacentGridCells(edge)) {
                    updateSurroundingEdges(cell);
                }
            }
        }

        private void toggleNonCross(Edge edge) {
            EdgeInfo edgeInfo = edgeInfos[edge];
            bool isRemoved = false;
            if (edgeInfo.isActive) {
                // active or wrong
                isRemoved = true;
                verifier.OnEdgeRemoved(edge);
                edgeInfo.isActive = false;
                edgeInfo.isWrong = false;
                edgeInfos[edge] = edgeInfo;
            } else if (edgeInfo.isCrossed) {
                // crossed
                edgeInfo.isCrossed = false;
                edgeInfos[edge] = edgeInfo;
            } else {
                // passive
                verifier.OnEdgeAdded(edge);
                edgeInfo.isActive = true;
                edgeInfos[edge] = edgeInfo;
                bool isWrong = !verifier.IsEdgeCorrect(edge);
                if (edgeInfo.isWrong != isWrong) {
                    edgeInfo.isWrong = isWrong;
                    edgeInfos[edge] = edgeInfo;
                }
            }
            if (isRemoved) {
                foreach (GridCell cell in adjacentGridCells(edge)) {
                    updateSurroundingEdges(cell);
                }
            }
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

        private List<GridPoint> adjacentGridCells(Edge edge) {
            List<GridCell> cells = new List<GridCell>();
            if (edge.IsHorizontal() && edge.From.Row < rowCount ||
                !edge.IsHorizontal() && edge.From.Col < colCount) {
                cells.Add(edge.From);
            }
            if (edge.IsHorizontal() && edge.From.Row > 0) {
                cells.Add(new GridCell(edge.From.Row - 1, edge.From.Col));
            } else if (!edge.IsHorizontal() && edge.To.Col > 0) {
                cells.Add(new GridCell(edge.From.Row, edge.From.Col - 1));
            }
            return cells;
        }

        private void updateSurroundingEdges(GridCell cell) {
            List<Edge> edges = surroundingEdges(cell);
            foreach (Edge edge in edges) {
                tryValidateEdge(edge);
            }
        }

        private void tryValidateEdge(Edge edge) {
            if (verifier.IsEdgeCorrect(edge)) {
                EdgeInfo edgeInfo = edgeInfos[edge];
                edgeInfo.isWrong = false;
                edgeInfos[edge] = edgeInfo;
            }
        }
    }
}
