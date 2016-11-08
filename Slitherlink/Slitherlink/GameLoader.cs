using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace Slitherlink {
    interface IGameLoader {
        GameController LoadGame();
    }

    class FileGameLoader : IGameLoader {
        string path;

        public FileGameLoader(string path) {
            this.path = path;
        }

        public GameController LoadGame() {
            string fileContents = FileUtils.LoadFileContents(path);
            List<string> fileTokens = new List<string>(fileContents.Split(new char[] { }, StringSplitOptions.RemoveEmptyEntries));

            int offset = 0;
            int height = int.Parse(fileTokens[offset++]);
            int width = int.Parse(fileTokens[offset++]);

            List<List<int>> numbers = parseMatrix(fileTokens.GetRange(offset, height * width), height, width);
            offset += height * width;

            List<List<int>> edgesHorizontal = parseMatrix(fileTokens.GetRange(offset, (height + 1) * width), height + 1, width);
            offset += (height + 1) * width;
            IDictionary<Edge, Edge.EdgeState> edgeStatesHorizontal = edgeMatrixToDict(edgesHorizontal, true);
            List<List<int>> edgesVertical = parseMatrix(fileTokens.GetRange(offset, height * (width + 1)), height, width + 1);
            IDictionary<Edge, Edge.EdgeState> edgeStatesVertical = edgeMatrixToDict(edgesVertical, false);
            IDictionary<Edge, Edge.EdgeState> edgeStates = edgeStatesHorizontal
                                                            .Concat(edgeStatesVertical)
                                                            .ToDictionary(x => x.Key, x => x.Value);
            return new GameController(height, width, numbers, edgeStates);
        }

        private IDictionary<Edge, Edge.EdgeState> edgeMatrixToDict(List<List<int>> matr, bool isHorizontal) {
            IDictionary<Edge, Edge.EdgeState> states = new Dictionary<Edge, Edge.EdgeState>();
            for (int row = 0; row < matr.Count; ++row) {
                for (int col = 0; col < matr[0].Count; ++col) {
                    Edge.EdgeState state = (Edge.EdgeState)(matr[row][col]);
                    Edge edge;
                    if (isHorizontal) {
                        edge = new Edge(new GridPoint(row, col), new GridPoint(row, col + 1));
                    } else {
                        edge = new Edge(new GridPoint(row, col), new GridPoint(row + 1, col));
                    }
                    states.Add(edge, state);
                }
            }
            return states;
        }

        private List<List<int>> parseMatrix(List<string> tokens, int height, int width) {
            List<List<int>> matr = new List<List<int>>(height);
            for (int i = 0; i < height; ++i) {
                List<int> row = new List<int>(tokens.GetRange(i * width, width).Select(str => int.Parse(str)));
                matr.Add(row);
            }
            return matr;
        }
    }

    class SimpleGameLoader : IGameLoader {
        public GameController LoadGame() {
            int rowCount = 3;
            int colCount = 3;

            List<List<int>> numbers = new List<List<int>> {
                new List<int> {  3,  1, -1 },
                new List<int> {  3,  2, -1 },
                new List<int> { -1, -1,  3 }
            };
            IDictionary<Edge, Edge.EdgeState> edgeStates = new Dictionary<Edge, Edge.EdgeState>();
            // horizontal edges
            for (int i = 0; i < rowCount + 1; ++i) {
                for (int j = 0; j < colCount; ++j) {
                    edgeStates.Add(new Edge(new GridPoint(i, j), new GridPoint(i, j + 1)), Edge.EdgeState.Passive);
                }
            }
            // vertical edges
            for (int i = 0; i < rowCount; ++i) {
                for (int j = 0; j < colCount + 1; ++j) {
                    edgeStates.Add(new Edge(new GridPoint(i, j), new GridPoint(i + 1, j)), Edge.EdgeState.Passive);
                }
            }
            return new GameController(rowCount, colCount, numbers, edgeStates);
        }
    }
}
