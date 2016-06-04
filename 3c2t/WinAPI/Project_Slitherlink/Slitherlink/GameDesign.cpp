#include "GameDesign.h"

// dimensions
const int CGameDesign::marginVertical = 10;
const int CGameDesign::marginHorizontal = 10;

CGameDesign::CGameDesign()
{
	initSettings();
}

CGameDesign::CGameDesign(int width, int height, int _rowCount, int _colCount)
	: rowCount(_rowCount), colCount(_colCount)
{
	ResizeWindow(width, height);
	initSettings();
}

CGameDesign::~CGameDesign()
{
	for( auto it = objects.begin(); it != objects.end(); ++it ) {
		::DeleteObject(it->second);
	}
}

void CGameDesign::initSettings()
{
	COLORREF colorBorderInactive = RGB(180, 180, 180);
	COLORREF colorBorderActive = RGB(100, 100, 255);
	COLORREF colorBorderWrong = RGB(255, 100, 100);
	COLORREF colorNumbersUnsatisfied = RGB(180, 100, 100);
	COLORREF colorNumbersSatisfied = RGB(80, 80, 80);

	colors["colorBorderInactive"] = colorBorderInactive;
	colors["colorBorderActive"] = colorBorderActive;
	colors["colorBorderWrong"] = colorBorderWrong;
	colors["colorNumbersUnsatisfied"] = colorNumbersUnsatisfied;
	colors["colorNumbersSatisfied"] = colorNumbersSatisfied;
	colors["colorBackground"] = RGB(255, 255, 255);

	int lineWidthActive = 5;
	int lineWidthInactive = 1;
	int fontSizePercent = 50;
	sizes["fontSizePercent"] = fontSizePercent;
	sizes["lineWidthActive"] = lineWidthActive;
	sizes["lineWidthInactive"] = lineWidthInactive;
	sizes["opacity"] = 255;

	fontChanged = false;
	HFONT sysFont = (HFONT)::GetStockObject(DEFAULT_GUI_FONT);
	::GetObject(sysFont, sizeof(LOGFONT), &logFontNumbers);
	objects["fontNumbers"] = createNewFontNumbers();

	objects["penBorderInactive"] = CreatePen(PS_DOT, lineWidthInactive, colorBorderInactive);
	objects["penBorderActive"] = CreatePen(PS_SOLID, lineWidthActive, colorBorderActive);
	objects["penBorderWrong"] = CreatePen(PS_SOLID, lineWidthActive, colorBorderWrong);

	HBRUSH brush = (HBRUSH)GetStockObject(WHITE_BRUSH);
	LOGBRUSH logBrush;
	::GetObject(brush, sizeof(LOGBRUSH), &logBrush);
	objects["brushBackground"] = brush;
}

void CGameDesign::updatePen(const std::string& penName, int newColor, int newWidth)
{
	auto itPen = objects.find(penName);
	if( itPen == objects.end() ) {
		return;
	}
	HGDIOBJ penOld = itPen->second;
	LOGPEN logPen;
	::GetObject(penOld, sizeof(logPen), &logPen);
	if( newColor > 0 ) {
		logPen.lopnColor = static_cast<DWORD>(newColor);
	}
	if( newWidth > 0 ) {
		POINT penSize;
		penSize.x = newWidth;
		logPen.lopnWidth = penSize;
	}
	::DeleteObject(penOld);
	objects[penName] = (static_cast<HGDIOBJ>(::CreatePenIndirect(&logPen)));
}

void CGameDesign::updateBrush(const std::string& brushName, int newColor)
{
	auto itBrush = objects.find(brushName);
	if( itBrush == objects.end() ) {
		return;
	}
	HGDIOBJ brushOld = itBrush->second;
	LOGBRUSH logBrush;
	::GetObject(brushOld, sizeof(logBrush), &logBrush);
	if( newColor > 0 ) {
		logBrush.lbColor = static_cast<DWORD>(newColor);
	}
	::DeleteObject(brushOld);
	objects[brushName] = (static_cast<HGDIOBJ>(::CreateBrushIndirect(&logBrush)));
}

void CGameDesign::SetColor(const std::string& propertyName, COLORREF newColor, bool needPenUpdate, bool needBrushUpdate)
{
	auto itColor = colors.find("color" + propertyName);
	if( itColor == colors.end() ) {
		return;
	}
	itColor->second = newColor;
	if( needPenUpdate ) {
		std::string objectName = "pen" + propertyName;
		updatePen(objectName, newColor, -1);
	}
	if( needBrushUpdate ) {
		std::string objectName = "brush" + propertyName;
		updateBrush(objectName, newColor);
	}
}

COLORREF CGameDesign::GetColor(const std::string& propertyName) const
{
	return colors.at(propertyName);
}

int CGameDesign::GetSize(const std::string& propertyName) const
{
	return sizes.at(propertyName);
}

HGDIOBJ CGameDesign::GetObject(const std::string& propertyName) const
{
	return objects.at(propertyName);
}

void CGameDesign::SetOpacity(HWND handle, int newOpacity)
{
	sizes["opacity"] = newOpacity;
	::SetLayeredWindowAttributes(handle, 0, newOpacity, LWA_ALPHA);
}

void CGameDesign::SetFontSize(int fontSizePercent)
{
	sizes["fontSizePercent"] = fontSizePercent;
	fontChanged = true;
}

void CGameDesign::ResizeWindow(int width, int height)
{
	windowWidth = width;
	windowHeight = height;
	rowHeight = (windowHeight - 2 * marginVertical) / rowCount;
	colWidth = (windowWidth - 2 * marginHorizontal) / colCount;
	rowLength = colWidth * colCount;
	colLength = rowHeight * rowCount;
	fontChanged = true;
}

void CGameDesign::ChangeGameSizes(int _rowCount, int _colCount)
{
	rowCount = _rowCount;
	colCount = _colCount;
	ResizeWindow(windowWidth, windowHeight);
}

int CGameDesign::countFontSize() const
{
	int fontSizePercent = sizes.at("fontSizePercent");
	return std::max(static_cast<int>(std::min(rowHeight, colWidth) * 1.0 * fontSizePercent / 100), 1);
}

HFONT CGameDesign::createNewFontNumbers()
{
	logFontNumbers.lfHeight = countFontSize();
	return CreateFontIndirect(&logFontNumbers);
}

void CGameDesign::updateFontNumbers()
{
	auto it = objects.find("fontNumbers");
	::DeleteObject(it->second);
	it->second = createNewFontNumbers();
	
	fontChanged = false;
}

POINT CGameDesign::countCoord(Point pointGrid) const
{
	POINT pointCoord;
	pointCoord.x = marginHorizontal + pointGrid.col * colWidth;
	pointCoord.y = marginVertical + pointGrid.row * rowHeight;
	return pointCoord;
}

Point CGameDesign::getGridPointUpLeft(POINT p) const
{
	int row = (p.y - marginVertical) / rowHeight;
	int col = (p.x - marginHorizontal) / colWidth;
	return Point(row, col);
}

Edge CGameDesign::NearestEdge(POINT p) const
{
	Point upLeftGrid = getGridPointUpLeft(p);
	if( upLeftGrid.row < 0 ) {
		upLeftGrid.row = 0;
	}
	if( upLeftGrid.row >= rowCount ) {
		upLeftGrid.row = rowCount - 1;
	}
	if( upLeftGrid.col < 0 ) {
		upLeftGrid.col = 0;
	}
	if( upLeftGrid.col >= colCount ) {
		upLeftGrid.col = colCount - 1;
	}
	Point downRightGrid(upLeftGrid.row + 1, upLeftGrid.col + 1);
	POINT upLeft = countCoord(upLeftGrid);
	POINT downRight = countCoord(downRightGrid);
	
	Edge edgeBest(upLeftGrid, Point(upLeftGrid.row, downRightGrid.col));
	int distBest = p.y - upLeft.y;

	if( p.x - upLeft.x < distBest ) {
		distBest = p.x - upLeft.x;
		edgeBest = Edge(upLeftGrid, Point(downRightGrid.row, upLeftGrid.col));
	}
	if( downRight.y - p.y < distBest ) {
		distBest = downRight.y - p.y;
		edgeBest = Edge(Point(downRightGrid.row, upLeftGrid.col), downRightGrid);
	}
	if( downRight.x - p.x < distBest ) {
		distBest = downRight.x - p.x;
		edgeBest = Edge(Point(upLeftGrid.row, downRightGrid.col), downRightGrid);
	}
	return edgeBest;
}

void CGameDesign::drawGrid(HDC hdc) const
{
	HGDIOBJ penDefault = SelectObject(hdc, objects.at("penBorderInactive"));
	POINT pointCur, pointInit;
	pointInit.x = marginHorizontal;
	pointInit.y = marginVertical;

	// draw horizontal lines
	pointCur = pointInit;
	::MoveToEx(hdc, pointInit.x, pointInit.y, NULL);
	for( int i = 0; i < rowCount + 1; ++i ) {
		pointCur.x += rowLength;
		::LineTo(hdc, pointCur.x, pointCur.y);
		pointCur.x -= rowLength;
		pointCur.y += rowHeight;
		::MoveToEx(hdc, pointCur.x, pointCur.y, NULL);
	}

	// draw vertical lines
	pointCur = pointInit;
	::MoveToEx(hdc, pointInit.x, pointInit.y, NULL);
	for( int i = 0; i < colCount + 1; ++i ) {
		pointCur.y += colLength;
		::LineTo(hdc, pointCur.x, pointCur.y);
		pointCur.x += colWidth;
		pointCur.y -= colLength;
		::MoveToEx(hdc, pointCur.x, pointCur.y, NULL);
	}
	::SelectObject(hdc, penDefault);
}

void CGameDesign::drawBorders(HDC hdc, const std::vector<Edge>& edges) const
{
	for( auto it = edges.begin(); it != edges.end(); ++it ) {
		POINT pointFrom = countCoord(it->from);
		POINT pointTo = countCoord(it->to);
		::MoveToEx(hdc, pointFrom.x, pointFrom.y, NULL);
		::LineTo(hdc, pointTo.x, pointTo.y);
	}
}

void CGameDesign::drawBorders(HDC hdc, const CSlitherlinkGame& game) const
{
	// draw active borders
	HGDIOBJ penDefault = ::SelectObject(hdc, objects.at("penBorderActive"));
	std::vector<Edge> edgesActive = game.GetEdgesByState(EDGE_STATE_ON);
	drawBorders(hdc, edgesActive);

	// draw wrong borders
	::SelectObject(hdc, objects.at("penBorderWrong"));
	std::vector<Edge> edgesWrong = game.GetEdgesByState(EDGE_STATE_ERR);
	drawBorders(hdc, edgesWrong);

	::SelectObject(hdc, penDefault);
}

void CGameDesign::drawNumbers(HDC hdc, const CSlitherlinkGame& game)
{
	const Matrix& numbers = game.GetNumbers();
	COLORREF colorDefault = ::SetTextColor(hdc, colors.at("colorNumbersUnsatisfied"));

	if( fontChanged ) {
		updateFontNumbers();
	}

	HGDIOBJ fontDefault = SelectObject(hdc, objects["fontNumbers"]);
	for( size_t row = 0; row < numbers.size(); ++row ) {
		int numberY = marginVertical + row * rowHeight;
		for( size_t col = 0; col < numbers[row].size(); ++col ) {
			int numberX = marginHorizontal + col * colWidth;
			if( game.IsShown(row, col) ) {
				std::wstring numberString = std::to_wstring(numbers[row][col]);
				RECT textBox;
				textBox.left = numberX;
				textBox.top = numberY;
				textBox.right = numberX + colWidth;
				textBox.bottom = numberY + rowHeight;
				bool isSatisfied = game.IsSatisfiedCell(row, col);
				if( isSatisfied ) {
					::SetTextColor(hdc, colors.at("colorNumbersSatisfied"));
				}
				::DrawText(hdc, numberString.c_str(), numberString.size(), &textBox, DT_SINGLELINE | DT_CENTER | DT_VCENTER);
				if( isSatisfied ) {
					::SetTextColor(hdc, colors.at("colorNumbersUnsatisfied"));
				}
			}
		}
	}
	::SetTextColor(hdc, colorDefault);
	::SelectObject(hdc, fontDefault);
}

void CGameDesign::DrawWindowContents(HDC hdc, const CSlitherlinkGame& game)
{
	drawGrid(hdc);
	drawBorders(hdc, game);
	drawNumbers(hdc, game);
}