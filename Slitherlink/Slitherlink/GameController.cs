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
    }

    class GameController {
        private IDictionary<Edge, Edge.EdgeState> edgeStates;
        private List<List<int>> numbers;
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
        }

        public void ToggleEdgeState(Edge edge, bool toggleCross) {
            Edge.EdgeState stateCur;
            if (edgeStates.TryGetValue(edge, out stateCur)) {
                if (toggleCross) {
                    edgeStates[edge] = stateCur == Edge.EdgeState.Crossed ? Edge.EdgeState.Passive : Edge.EdgeState.Crossed;
                } else {
                    edgeStates[edge] = stateCur == Edge.EdgeState.Active ? Edge.EdgeState.Passive : Edge.EdgeState.Active;
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
    }
}
