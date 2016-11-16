using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Slitherlink {
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
        public enum EdgeState {
            Passive, Active, Crossed, Wrong
        }

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

    class GameController {
        private IDictionary<Edge, Edge.EdgeState> edgeStates;
        private List<List<int>> numbers;
        private List<List<int>> edgesAroundCounter;
        private List<List<bool>> numbersSatisfaction;
        private int rowCount;
        private int colCount;

        public int RowCount {
            get { return rowCount; }
        }

        public int ColCount {
            get { return colCount; }
        }

        public GameController(int rowCount, int colCount, List<List<int>> numbers, IDictionary<Edge, Edge.EdgeState> edgeStates) {
            this.rowCount = rowCount;
            this.colCount = colCount;
            this.numbers = numbers;
            this.edgeStates = edgeStates;

            edgesAroundCounter = new List<List<int>>(rowCount);
            for (int rowIdx = 0; rowIdx < rowCount; ++rowIdx) {
                List<int> row = Enumerable.Repeat(0, colCount).ToList();
                edgesAroundCounter.Add(row);
            }
            foreach (KeyValuePair<Edge, Edge.EdgeState> pair in edgeStates) {
                Edge edge = pair.Key;
                Edge.EdgeState state = pair.Value;
                if (state == Edge.EdgeState.Active || state == Edge.EdgeState.Wrong) {
                    List<GridPoint> adjacentPoints = adjacentGridPoints(edge);
                    foreach (GridPoint point in adjacentPoints) {
                        edgesAroundCounter[point.Row][point.Col] += 1;
                    }
                }
            }
            numbersSatisfaction = new List<List<bool>>(rowCount);
            for (int row = 0; row < rowCount; ++row) {
                numbersSatisfaction.Add(Enumerable.Repeat(false, colCount).ToList());
            }
            recountNumbersSatisfaction();
        }

        public void ToggleEdgeState(Edge edge, bool isToggleCross) {
            Edge.EdgeState stateCur;
            if (edgeStates.TryGetValue(edge, out stateCur)) {
                if (isToggleCross) {
                    toggleCross(edge, stateCur);
                } else {
                    toggleNonCross(edge, stateCur);
                }
            }
        }

        public IList<Edge> GetEdgesByState(Edge.EdgeState state) {
            return edgeStates.Where(pair => pair.Value == state).Select(pair => pair.Key).ToList();
        }

        public List<List<int>> GetNumbers() {
            return numbers;
        }

        public List<List<bool>> GetNumbersSatisfaction() {
            // TODO: implement GetGridPoints(), refactor satisfaction, numbers, edgeAroundCounter to be hash tables
            return numbersSatisfaction;
        }

        private bool isNumberSatisfied(GridPoint point) {
            return edgesAroundCounter[point.Row][point.Col] == numbers[point.Row][point.Col];
        }

        private bool isWrongEdge(Edge edge) {
            List<GridPoint> points = adjacentGridPoints(edge);
            bool isMistake = false;
            foreach (GridPoint point in points) {
                isMistake = isMistake || (numbers[point.Row][point.Col] >= 0 && 
                    numbers[point.Row][point.Col] < edgesAroundCounter[point.Row][point.Col]);
            }
            return isMistake;
        }

        private void toggleCross(Edge edge, Edge.EdgeState stateCur) {
            switch (stateCur) {
                case Edge.EdgeState.Crossed:
                    edgeStates[edge] = Edge.EdgeState.Passive;
                    break;
                case Edge.EdgeState.Active:
                    onEdgeRemoved(edge);
                    goto case Edge.EdgeState.Passive;
                case Edge.EdgeState.Passive:
                case Edge.EdgeState.Wrong:
                    edgeStates[edge] = Edge.EdgeState.Crossed;
                    break;
                default:
                    throw new KeyNotFoundException("Found unexpected edge state");
            }
        }

        private void toggleNonCross(Edge edge, Edge.EdgeState stateCur) {
            switch (stateCur) {
                case Edge.EdgeState.Passive:
                    edgeStates[edge] = onEdgeAdded(edge);
                    break;
                case Edge.EdgeState.Active:
                case Edge.EdgeState.Wrong:
                    onEdgeRemoved(edge);
                    goto case Edge.EdgeState.Crossed;
                case Edge.EdgeState.Crossed:
                    edgeStates[edge] = Edge.EdgeState.Passive;
                    break;
                default:
                    throw new KeyNotFoundException("Found unexpected edge state");
            }
        }

        private List<GridPoint> adjacentGridPoints(Edge edge) {
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

        private List<Edge> surroundingEdges(GridPoint point) {
            List<Edge> edges = new List<Edge>() {
                new Edge(point, new GridPoint(point.Row, point.Col + 1)), // up  , horizontal
                new Edge(point, new GridPoint(point.Row + 1, point.Col)), // left, vertical
                new Edge(new GridPoint(point.Row + 1, point.Col), new GridPoint(point.Row + 1, point.Col + 1)), // down , horizontal
                new Edge(new GridPoint(point.Row, point.Col + 1), new GridPoint(point.Row + 1, point.Col + 1))  // right, vertical
            };
            return edges;
        }

        private void correctSurroundingEdges(GridPoint point) {
            List<Edge> edges = surroundingEdges(point);
            foreach (Edge edge in edges) {
                if (edgeStates[edge] == Edge.EdgeState.Wrong) {
                    edgeStates[edge] = Edge.EdgeState.Active;
                }
            }
        }

        // should be called every time an edge is added
        // returns the state of the added edge (Active or Wrong)
        private Edge.EdgeState onEdgeAdded(Edge edge) {
            List<GridPoint> points = adjacentGridPoints(edge);
            foreach (GridPoint point in points) {
                ++edgesAroundCounter[point.Row][point.Col];
                numbersSatisfaction[point.Row][point.Col] = isNumberSatisfied(point);
            }
            return isWrongEdge(edge) ? Edge.EdgeState.Wrong : Edge.EdgeState.Active;
        }

        // should be called every time an edge is removed
        private void onEdgeRemoved(Edge edge) {
            List<GridPoint> points = adjacentGridPoints(edge);
            foreach (GridPoint point in points) {
                --edgesAroundCounter[point.Row][point.Col];
                bool wasNumberSatisfied = numbersSatisfaction[point.Row][point.Col];
                numbersSatisfaction[point.Row][point.Col] = isNumberSatisfied(point);
                if (!wasNumberSatisfied && numbersSatisfaction[point.Row][point.Col]) {
                    correctSurroundingEdges(point);
                }
            }
        }

        private void recountNumbersSatisfaction() {
            for (int row = 0; row < rowCount; ++row) {
                for (int col = 0; col < colCount; ++col) {
                    numbersSatisfaction[row][col] = isNumberSatisfied(new GridPoint(row, col));
                }
            }
        }
    }
}
