using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Slitherlink {
    class FileUtils {
        public string LoadFileText(string path) {
            string line = "";
            using (StreamReader sr = new StreamReader(path)) {
                line = sr.ReadToEnd();
                /**/Console.WriteLine(line);
            }
            return line;
        }

        public List<List<int>> LoadMatrix(string path) {
            
            throw new NotImplementedException();
        }
    }
}
