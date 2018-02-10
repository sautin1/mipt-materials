using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Slitherlink {
    class FileUtils {
        public static string LoadFileContents(string path) {
            string line = "";
            using (StreamReader sr = new StreamReader(path)) {
                line = sr.ReadToEnd();
            }
            return line;
        }

        public static List<List<int>> LoadMatrix(string path) {
            List<List<int>> matr = new List<List<int>>();
            using (StreamReader sr = new StreamReader(path)) {
                string line = sr.ReadLine();
                string[] numbers = line.Split();
                int height = int.Parse(numbers[0]);
                int width = int.Parse(numbers[1]);
                for (int i = 0; i < height; ++i) {
                    List<int> row = new List<int>(sr.ReadLine().Split().Select(str => int.Parse(str)));
                    matr.Add(row);
                }
            }
            return matr;
        }

        public static void ClearFile(string path) {
            File.WriteAllText(path, string.Empty);
        }

        public static void AppendLineToFile(string path, string line) {
            using (StreamWriter sw = File.AppendText(path)) {
                sw.WriteLine(line);
            }
        }
    }
}
