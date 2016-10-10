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
        private int rowCount;
        private int colCount;

        public int RowCount {
            get { return rowCount; }
        }

        public int ColCount {
            get { return colCount; }
        }

        // temporary
        public GameController() {
            rowCount = 3;
            colCount = 3;
            edgeStates = new Dictionary<Edge, Edge.EdgeState>();
            edgeStates.Add(new Edge(new GridPoint(0, 0), new GridPoint(0, 1)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(0, 1), new GridPoint(0, 2)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(0, 2), new GridPoint(0, 3)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(1, 0), new GridPoint(1, 1)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(1, 1), new GridPoint(1, 2)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(1, 2), new GridPoint(1, 3)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(2, 0), new GridPoint(2, 1)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(2, 1), new GridPoint(2, 2)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(2, 2), new GridPoint(2, 3)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(3, 0), new GridPoint(3, 1)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(3, 1), new GridPoint(3, 2)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(3, 2), new GridPoint(3, 3)), Edge.EdgeState.Passive);

            edgeStates.Add(new Edge(new GridPoint(0, 0), new GridPoint(1, 0)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(1, 0), new GridPoint(2, 0)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(2, 0), new GridPoint(3, 0)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(0, 1), new GridPoint(1, 1)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(1, 1), new GridPoint(2, 1)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(2, 1), new GridPoint(3, 1)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(0, 2), new GridPoint(1, 2)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(1, 2), new GridPoint(2, 2)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(2, 2), new GridPoint(3, 2)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(0, 3), new GridPoint(1, 3)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(1, 3), new GridPoint(2, 3)), Edge.EdgeState.Passive);
            edgeStates.Add(new Edge(new GridPoint(2, 3), new GridPoint(3, 3)), Edge.EdgeState.Passive);
        }

        public GameController(int rowCount, int colCount, IDictionary<Edge, Edge.EdgeState> edgeStates) {
            this.edgeStates = edgeStates;
            this.rowCount = rowCount;
            this.colCount = colCount;
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
    }
}
