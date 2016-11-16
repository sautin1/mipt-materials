using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Slitherlink {
    interface IGameSaver{
        void SaveGame(GameController controller);
    }

    class FileGameSaver : IGameSaver {
        private string path;

        public FileGameSaver(string path) {
            this.path = path;
        }

        public void SaveGame(GameController controller) {
            List<string> lines = new List<string> {
                controller.RowCount.ToString() + ' ' + controller.ColCount.ToString()
            };
            List<List<int>> numbers = controller.GetNumbers();
            for (int row = 0; row < controller.RowCount; ++row) {
                lines.Add(String.Join(" ", numbers[row]));
            }

            IDictionary<Edge, Edge.EdgeState> edgeStates = controller.GetEdgeStates();
            for (int row = 0; row < controller.RowCount + 1; ++row) {
                List<int> rowStates = new List<int>(controller.ColCount);
                for (int col = 0; col < controller.ColCount; ++col) {
                    GridPoint from = new GridPoint(row, col);
                    GridPoint to = new GridPoint(row, col + 1);
                    rowStates.Add((int)edgeStates[new Edge(from, to)]);
                }
                lines.Add(String.Join(" ", rowStates));
            }

            for (int row = 0; row < controller.RowCount; ++row) {
                List<int> rowStates = new List<int>(controller.ColCount + 1);
                for (int col = 0; col < controller.ColCount + 1; ++col) {
                    GridPoint from = new GridPoint(row, col);
                    GridPoint to = new GridPoint(row + 1, col);
                    rowStates.Add((int)edgeStates[new Edge(from, to)]);
                }
                lines.Add(String.Join(" ", rowStates));
            }

            FileUtils.ClearFile(path);
            foreach (string line in lines) {
                FileUtils.AppendLineToFile(path, line);
            }
        }
    }
}
