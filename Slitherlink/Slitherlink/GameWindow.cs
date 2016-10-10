using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Slitherlink
{
    public partial class GameWindow : Form
    {
        Pen pen = new Pen(Color.Green);
        GameDrawer drawer;
        GameController gameController;

        public GameWindow()
        {
            InitializeComponent();
            Size clientRectSize = drawingArea.ClientRectangle.Size;
            gameController = new GameController();
            drawer = new GameDrawer(clientRectSize, gameController.RowCount, gameController.ColCount);
        }

        // Drawing area event handlers

        private void drawingArea_Resize(object sender, EventArgs e) {
            drawer.ResizeWindow(drawingArea.ClientRectangle.Size);
            drawingArea.Invalidate();
        }

        private void drawingArea_Paint(object sender, PaintEventArgs e) {
            drawer.DrawGrid(e.Graphics);
            drawer.DrawActiveEdges(e.Graphics, gameController.GetEdgesByState(Edge.EdgeState.Active));
        }

        private void drawingArea_Click(object sender, EventArgs e) {
            MouseEventArgs clickInfo = (MouseEventArgs)e;
            Edge edgeNearest = drawer.NearestGridEdge(clickInfo.Location);
            gameController.ToggleEdgeState(edgeNearest, clickInfo.Button == MouseButtons.Right);
            drawingArea.Invalidate();
        }

        // Menu event handlers

        private void exitToolStripMenuItem_Click(object sender, EventArgs e) {
            Application.Exit();
        }

        // auxiliary functions
        
    }
}
