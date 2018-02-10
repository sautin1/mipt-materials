using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace Slitherlink {
    using GridCell = GridPoint;

    interface IGameLoader {
        GameController LoadGame();
    }

    class FileGameLoader : IGameLoader {
        private string path;

        public FileGameLoader(string path) {
            this.path = path;
        }

        public GameController LoadGame() {
            string fileContents = FileUtils.LoadFileContents(path);
            List<string> fileTokens = new List<string>(fileContents.Split(new char[] { }, StringSplitOptions.RemoveEmptyEntries));

            int offset = 0;
            int height = int.Parse(fileTokens[offset++]);
            int width = int.Parse(fileTokens[offset++]);

            List<List<int>> numbersMatr = parseMatrix(fileTokens.GetRange(offset, height * width), height, width);
            IDictionary<GridCell, int> numbers = new Dictionary<GridCell, int>(height * width);
            for (int row = 0; row < height; ++row) {
                for (int col = 0; col < width; ++col) {
                    numbers.Add(new GridCell(row, col), numbersMatr[row][col]);
                }
            }
            offset += height * width;

            List<List<int>> edgesHorizontal = parseMatrix(fileTokens.GetRange(offset, (height + 1) * width), height + 1, width);
            offset += (height + 1) * width;
            IDictionary<Edge, EdgeInfo> edgeStatesHorizontal = edgeMatrixToDict(edgesHorizontal, true);
            List<List<int>> edgesVertical = parseMatrix(fileTokens.GetRange(offset, height * (width + 1)), height, width + 1);
            IDictionary<Edge, EdgeInfo> edgeStatesVertical = edgeMatrixToDict(edgesVertical, false);
            IDictionary<Edge, EdgeInfo> edgeStates = edgeStatesHorizontal
                                                            .Concat(edgeStatesVertical)
                                                            .ToDictionary(x => x.Key, x => x.Value);
            return new GameController(height, width, numbers, edgeStates);
        }

        private IDictionary<Edge, EdgeInfo> edgeMatrixToDict(List<List<int>> matr, bool isHorizontal) {
            IDictionary<Edge, EdgeInfo> states = new Dictionary<Edge, EdgeInfo>();
            for (int row = 0; row < matr.Count; ++row) {
                for (int col = 0; col < matr[0].Count; ++col) {
                    EdgeInfo edgeInfo = EdgeInfo.FromInt(matr[row][col]);
                    Edge edge;
                    if (isHorizontal) {
                        edge = new Edge(new GridPoint(row, col), new GridPoint(row, col + 1));
                    } else {
                        edge = new Edge(new GridPoint(row, col), new GridPoint(row + 1, col));
                    }
                    states.Add(edge, edgeInfo);
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

            List<List<int>> numbersMatr = new List<List<int>> {
                new List<int> {  3,  1, -1 },
                new List<int> {  3,  2, -1 },
                new List<int> { -1, -1,  3 }
            };
            IDictionary<GridCell, int> numbers = new Dictionary<GridCell, int>(rowCount * colCount);
            for (int row = 0; row < rowCount; ++row) {
                for (int col = 0; col < colCount; ++col) {
                    numbers.Add(new GridCell(row, col), numbersMatr[row][col]);
                }
            }


            IDictionary<Edge, EdgeInfo> edgeInfos = new Dictionary<Edge, EdgeInfo>();
            // horizontal edges
            for (int i = 0; i < rowCount + 1; ++i) {
                for (int j = 0; j < colCount; ++j) {
                    edgeInfos.Add(new Edge(new GridPoint(i, j), new GridPoint(i, j + 1)), new EdgeInfo());
                }
            }
            // vertical edges
            for (int i = 0; i < rowCount; ++i) {
                for (int j = 0; j < colCount + 1; ++j) {
                    edgeInfos.Add(new Edge(new GridPoint(i, j), new GridPoint(i + 1, j)), new EdgeInfo());
                }
            }
            return new GameController(rowCount, colCount, numbers, edgeInfos);
        }
    }
}
