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
            foreach (KeyValuePair<Edge, Edge.EdgeState> pair in edgeStates ) {
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

        public void ToggleEdgeState(Edge edge, bool toggleCross) {
            Edge.EdgeState stateCur;
            if (edgeStates.TryGetValue(edge, out stateCur)) {
                if (toggleCross) {
                    if (stateCur == Edge.EdgeState.Crossed) {
                        edgeStates[edge] = Edge.EdgeState.Passive;
                    } else {
                        edgeStates[edge] = Edge.EdgeState.Crossed;
                        onRemoveEdge(edge);
                    }
                } else {
                    if (stateCur == Edge.EdgeState.Active) {
                        edgeStates[edge] = Edge.EdgeState.Passive;
                        onRemoveEdge(edge);
                    } else {
                        edgeStates[edge] = Edge.EdgeState.Active;
                        onAddEdge(edge);
                    }
                }
            } else {
                throw new ArgumentException("Wrong edge provided");
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

        public bool IsNumberSatisfied(GridPoint point) {
            return edgesAroundCounter[point.Row][point.Col] == numbers[point.Row][point.Col];
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

        private void onAddEdge(Edge edge) {
            List<GridPoint> points = adjacentGridPoints(edge);
            foreach (GridPoint point in points) {
                ++edgesAroundCounter[point.Row][point.Col];
                numbersSatisfaction[point.Row][point.Col] = IsNumberSatisfied(point);
            }
        }

        private void onRemoveEdge(Edge edge) {
            List<GridPoint> points = adjacentGridPoints(edge);
            foreach (GridPoint point in points) {
                --edgesAroundCounter[point.Row][point.Col];
                numbersSatisfaction[point.Row][point.Col] = IsNumberSatisfied(point);
            }
        }

        private void recountNumbersSatisfaction() {
            for (int row = 0; row < rowCount; ++row) {
                for (int col = 0; col < colCount; ++col) {
                    numbersSatisfaction[row][col] = IsNumberSatisfied(new GridPoint(row, col));
                }
            }
        }
    }
}
