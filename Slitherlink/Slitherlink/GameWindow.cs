using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Windows;

namespace Slitherlink
{
    public partial class GameWindow : Form
    {
        private GameDrawer drawer;
        private GameController gameController;
        
        private int formWidthInitial = 250;
        private int formHeightExcess = 49;

        public GameWindow()
        {
            InitializeComponent();
            Size clientRectSize = drawingArea.ClientRectangle.Size;
            gameController = new SimpleGameLoader().LoadGame();
            drawer = new GameDrawer(clientRectSize, gameController.RowCount, gameController.ColCount);
            this.Size = new Size(formWidthInitial, countFormHeight(formWidthInitial));
        }

        private int countFormHeight(int width)
        {
            return Convert.ToInt32(Math.Round(width * getAspectRatio()) + formHeightExcess);
        }

        private double getAspectRatio()
        {
            return 1.0 * gameController.RowCount / gameController.ColCount;
        }

        // Drawing area event handlers

        private void drawingArea_Resize(object sender, EventArgs e) {
            drawer.ResizeWindow(drawingArea.ClientRectangle.Size);
            drawingArea.Invalidate();
        }

        private void drawingArea_Paint(object sender, PaintEventArgs e) {
            drawer.DrawGrid(e.Graphics);
            drawer.DrawEdgesActive(e.Graphics, gameController.GetEdgesByState(Edge.EdgeState.Active));
            drawer.DrawEdgesCrossed(e.Graphics, gameController.GetEdgesByState(Edge.EdgeState.Crossed));
            drawer.DrawEdgesWrong(e.Graphics, gameController.GetEdgesByState(Edge.EdgeState.Wrong));
            drawer.DrawNumbers(e.Graphics, gameController.GetNumbers(), gameController.GetNumbersSatisfaction());
        }

        private void drawingArea_Click(object sender, EventArgs e) {
            MouseEventArgs clickInfo = (MouseEventArgs)e;
            Edge edgeNearest = drawer.NearestGridEdge(clickInfo.Location);
            gameController.ToggleEdgeState(edgeNearest, clickInfo.Button == MouseButtons.Right);
            drawingArea.Invalidate();
        }

        private void drawingArea_DoubleClick(object sender, EventArgs e) {
            drawingArea_Click(sender, e);
        }

        // Menu event handlers

        private void loadToolStripMenuItem_Click(object sender, EventArgs e) {
            OpenFileDialog openFileDialog = new OpenFileDialog();
            openFileDialog.Filter = "All Files (*.*)|*.*";
            openFileDialog.FilterIndex = 1;

            bool userClickedOK = openFileDialog.ShowDialog() == DialogResult.OK;
            if (userClickedOK == true) {
                string chosenFile = openFileDialog.FileName;
                gameController = new FileGameLoader(chosenFile).LoadGame();
                drawer.ChangeGameSizes(gameController.RowCount, gameController.ColCount);
                drawingArea.Invalidate();
            }
        }

        private void resetToolStripMenuItem_Click(object sender, EventArgs e) {
            gameController.ClearGame();
            drawingArea.Invalidate();
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e) {
            Application.Exit();
        }

        // Form event handlers

        private void GameWindow_Resize(object sender, EventArgs e) {
            int formWidth = this.Width;
            this.Size = new Size(formWidth, countFormHeight(formWidth));
        }
    }
}
