using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Slitherlink {
    using GridCell = GridPoint;

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
            IDictionary<GridCell, int> numbers = controller.Numbers();
            for (int row = 0; row < controller.RowCount; ++row) {
                IList<int> numberRow = new List<int>(controller.ColCount);
                for (int col = 0; col < controller.ColCount; ++col) {
                    numberRow.Add(numbers[new GridCell(row, col)]);
                }
                lines.Add(String.Join(" ", numberRow));
            }

            IDictionary<Edge, EdgeInfo> edgeInfos = controller.EdgeInfos();
            for (int row = 0; row < controller.RowCount + 1; ++row) {
                List<int> rowStates = new List<int>(controller.ColCount);
                for (int col = 0; col < controller.ColCount; ++col) {
                    GridPoint from = new GridPoint(row, col);
                    GridPoint to = new GridPoint(row, col + 1);
                    rowStates.Add(edgeInfos[new Edge(from, to)].ToInt());
                }
                lines.Add(String.Join(" ", rowStates));
            }

            for (int row = 0; row < controller.RowCount; ++row) {
                List<int> rowStates = new List<int>(controller.ColCount + 1);
                for (int col = 0; col < controller.ColCount + 1; ++col) {
                    GridPoint from = new GridPoint(row, col);
                    GridPoint to = new GridPoint(row + 1, col);
                    rowStates.Add(edgeInfos[new Edge(from, to)].ToInt());
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
