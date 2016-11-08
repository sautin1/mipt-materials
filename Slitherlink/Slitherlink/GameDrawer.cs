﻿using System;
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
        private int rowWidth;
        private int colHeight;
        private int marginVertical = 10;
        private int marginHorizontal = 10;
        private double crossFrac = 0.1;
        private double fontSizeFraction = 0.5;
        private int crossMaxSize = 14;
        IDictionary<String, Pen> pens;
        IDictionary<String, Brush> brushes;

        public GameDrawer() {
            initSettings();
        }

        public GameDrawer(Size windowSize, int rowCount, int colCount) {
            ChangeGameSizes(rowCount, colCount);
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
            rowWidth = colWidth * colCount;
            colHeight = rowHeight * rowCount;
        }

        public void DrawGrid(Graphics graphics) {
            // TODO: call drawEdges() here
            Pen pen = pens["penEdgeInactive"];
            // draw horizontal lines
            for (int i = 0; i < rowCount + 1; ++i) {
                int y = marginVertical + rowHeight * i;
                graphics.DrawLine(pen, marginHorizontal, y, marginHorizontal + rowWidth, y);
            }

            // draw vertical lines
            for (int i = 0; i < colCount + 1; ++i) {
                int x = marginHorizontal + colWidth * i;
                graphics.DrawLine(pen, x, marginVertical, x, marginVertical + colHeight);
            }
        }

        public void DrawEdgesActive(Graphics graphics, IList<Edge> edges) {
            drawEdges(graphics, edges, "penEdgeActive");
        }

        public void DrawEdgesCrossed(Graphics graphics, IList<Edge> edges) {
            drawEdges(graphics, edges, "penEdgeCrossed");
            drawCrosses(graphics, edges);
        }

        public void DrawNumbers(Graphics graphics, List<List<int>> numbers) {
            StringFormat format = new StringFormat();
            Font font = new Font("Arial", (int)Math.Round(fontSizeFraction * Math.Min(rowHeight, colWidth)));
            format.Alignment = StringAlignment.Center;
            format.LineAlignment = StringAlignment.Center;
            Brush brush = brushes["colorNumberUnsatisfied"];
            for (int row = 0; row < numbers.Count; ++row) {
                for (int col = 0; col < numbers[row].Count; ++col) {
                    if (numbers[row][col] >= 0) {
                        GridPoint gridPointFrom = new GridPoint(row, col);
                        GridPoint gridPointTo = new GridPoint(row + 1, col + 1);
                        Point pointFrom = toPoint(gridPointFrom);
                        Point pointTo = toPoint(gridPointTo);
                        Size rectSize = new Size(pointTo.X - pointFrom.X, pointTo.Y - pointFrom.Y);
                        Rectangle rect = new Rectangle(pointFrom, rectSize);
                        graphics.DrawString(numbers[row][col].ToString(), font, brush, rect, format);
                    }
                }
            }
        }

        public GridPoint GetLeftUpperGridPoint(Point p) {
            return new GridPoint((p.Y - marginVertical) / rowHeight, (p.X - marginHorizontal) / colWidth);
        }

        public Edge NearestGridEdge(Point p) {
            GridPoint leftUpperGridPoint = GetLeftUpperGridPoint(p);
            leftUpperGridPoint.Col = Math.Max(leftUpperGridPoint.Col, 0);
            leftUpperGridPoint.Col = Math.Min(leftUpperGridPoint.Col, colCount - 1);
            leftUpperGridPoint.Row = Math.Max(leftUpperGridPoint.Row, 0);
            leftUpperGridPoint.Row = Math.Min(leftUpperGridPoint.Row, rowCount - 1);
            GridPoint rightLowerGridPoint = new GridPoint(leftUpperGridPoint.Row + 1, leftUpperGridPoint.Col + 1);
            Point leftUpperPoint = toPoint(leftUpperGridPoint);
            Point rightLowerPoint = toPoint(rightLowerGridPoint);
            
            int distBest = Math.Abs(p.X - leftUpperPoint.X);
            Edge edgeBest = new Edge(leftUpperGridPoint, new GridPoint(rightLowerGridPoint.Row, leftUpperGridPoint.Col));

            int distToRight = Math.Abs(rightLowerPoint.X - p.X);
            if (distToRight < distBest) {
                distBest = distToRight;
                edgeBest = new Edge(new GridPoint(leftUpperGridPoint.Row, rightLowerGridPoint.Col), rightLowerGridPoint);
            }

            int distToTop = Math.Abs(p.Y - leftUpperPoint.Y);
            if (distToTop < distBest) {
                distBest = distToTop;
                edgeBest = new Edge(leftUpperGridPoint, new GridPoint(leftUpperGridPoint.Row, rightLowerGridPoint.Col));
            }

            int distToBottom = Math.Abs(rightLowerPoint.Y - p.Y);
            if (distToBottom < distBest) {
                distBest = distToBottom;
                edgeBest = new Edge(new GridPoint(rightLowerGridPoint.Row, leftUpperGridPoint.Col), rightLowerGridPoint);
            }

            return edgeBest;
        }

        private void initSettings() {
            Color colorBorderInactive = Color.FromArgb(180, 180, 180);
            Color colorBorderActive = Color.FromArgb(0, 0, 250);
            Color colorNumberUnsatisfied = Color.FromArgb(180, 100, 100);
            Color colorNumberSatisfied = Color.FromArgb(80, 80, 80);
            int lineWidthInactive = 1;
            int lineWidthCross = 2;
            int lineWidthActive = 4;

            pens = new Dictionary<String, Pen>();
            Pen pen = new Pen(colorBorderInactive, lineWidthInactive);
            pen.DashStyle = System.Drawing.Drawing2D.DashStyle.Dash;
            pens["penEdgeInactive"] = pen;
            pens["penEdgeCrossed"] = pen;
            pens["penEdgeActive"] = new Pen(colorBorderActive, lineWidthActive);
            pens["penCross"] = new Pen(colorBorderInactive, lineWidthCross);

            brushes = new Dictionary<String, Brush>();
            brushes["colorNumberUnsatisfied"] = new SolidBrush(colorNumberUnsatisfied);
            brushes["colorNumberSatisfied"] = new SolidBrush(colorNumberSatisfied);
        }

        private void drawEdges(Graphics graphics, IList<Edge> edges, String penStyle) {
            Pen pen = pens[penStyle];
            foreach (Edge edge in edges) {
                graphics.DrawLine(pen, toPoint(edge.From), toPoint(edge.To));
            }
        }

        private void drawCrosses(Graphics graphics, IList<Edge> edges) {
            foreach (Edge edge in edges) {
                Point from = toPoint(edge.From);
                Point to = toPoint(edge.To);
                int crossSize = Math.Min(
                    Convert.ToInt32(Math.Round(crossFrac * Math.Max(colWidth, rowHeight))),
                    crossMaxSize
                );
                Point crossRectLeftPoint = new Point(
                    (from.X + to.X - crossSize) / 2, 
                    (from.Y + to.Y - crossSize) / 2
                );
                Rectangle rect = new Rectangle(crossRectLeftPoint, new Size(crossSize, crossSize));
                drawCross(graphics, rect);
            }
        }

        private void drawCross(Graphics graphics, Rectangle rect) {
            Pen pen = pens["penCross"];
            graphics.DrawLine(pen, new Point(rect.Left, rect.Top), new Point(rect.Right, rect.Bottom));
            graphics.DrawLine(pen, new Point(rect.Left, rect.Bottom), new Point(rect.Right, rect.Top));
        }

        private Point toPoint(GridPoint gridPoint) {
            return new Point {
                X = marginHorizontal + gridPoint.Col * colWidth,
                Y = marginVertical + gridPoint.Row * rowHeight
            };
        }
    }
}
