using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Slitherlink {
    abstract class GameLoader {
        abstract public GameController LoadGame();
    }

    class FileGameLoader : GameLoader {
        public FileGameLoader(string path) {

        }

        public override GameController LoadGame() {
            throw new NotImplementedException();
        }
    }

    class SimpleGameLoader : GameLoader {
        public override GameController LoadGame() {
            throw new NotImplementedException();
        }
    }
}
