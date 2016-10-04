using System;
using System.Collections.Generic;
using System.Linq;
using System.Drawing;
using System.Text;
using System.Threading.Tasks;

namespace Slitherlink {
    class GameDrawer {
        private int rowCount;
        private int colCount;
        private Size windowSize;
        private int rowHeight;
        private int colWidth;
        private int rowLength;
        private int colLength;
        private int marginVertical = 10;
        private int marginHorizontal = 10;
        IDictionary<String, Pen> pens;

        public GameDrawer() {
            initSettings();
        }

        public GameDrawer(Size windowSize, int rowCount, int colCount) {
            this.rowCount = rowCount;
            this.colCount = colCount;
            ResizeWindow(windowSize);
            initSettings();
        }

        public void ChangeGameSizes(int rowCount, int colCount) {
            this.rowCount = rowCount;
            this.colCount = colCount;
            ResizeWindow(windowSize);
        }
        

        public void ResizeWindow(Size sizeNew) {
            windowSize = sizeNew;
            rowHeight = (windowSize.Height - 2 * marginVertical) / rowCount;
            colWidth = (windowSize.Width - 2 * marginHorizontal) / colCount;
            rowLength = colWidth * colCount;
            colLength = rowHeight * rowCount;
        }

        public void DrawGrid(Graphics graphics) {
            Pen pen = pens["penBorderInactive"];
            // draw horizontal lines
            for (int i = 0; i < rowCount + 1; ++i) {
                int y = marginVertical + rowHeight * i;
                graphics.DrawLine(pen, marginHorizontal, y, marginHorizontal + rowLength, y);
            }

            // draw vertical lines
            for (int i = 0; i < colCount + 1; ++i) {
                int x = marginHorizontal + colWidth * i;
                graphics.DrawLine(pen, x, marginVertical, x, marginVertical + colLength);
            }
        }

        private void initSettings() {
            Color colorBorderInactive = Color.FromArgb(180, 180, 180);
            int lineWidthInactive = 1;

            pens = new Dictionary<String, Pen>();
            Pen pen = new Pen(colorBorderInactive, lineWidthInactive);
            pen.DashStyle = System.Drawing.Drawing2D.DashStyle.Dash;
            pens["penBorderInactive"] = pen;
        }
    }
}
