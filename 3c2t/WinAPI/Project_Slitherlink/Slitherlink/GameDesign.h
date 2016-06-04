#pragma once
#define NOMINMAX
#include <Windows.h>
#include <algorithm>
#include <string>
#include <unordered_map>

#include "SlitherlinkGame.h"

class CGameDesign
{
public:
	CGameDesign();
	CGameDesign(int width, int height, int _rowCount, int _columnCount);
	~CGameDesign();
	
	void ResizeWindow(int width, int height);
	void ChangeGameSizes(int rowCount, int colCount);
	void DrawWindowContents(HDC hdc, const CSlitherlinkGame& game);
	Edge NearestEdge(POINT p) const;
	void SetColor(const std::string& propertyName, COLORREF newValue, bool needPenUpdate, bool needBrushUpdate);
	void SetFontSize(int fontSizePercent);
	void SetOpacity(HWND handle, int newOpacity);
	COLORREF GetColor(const std::string& propertyName) const;
	HGDIOBJ GetObject(const std::string& propertyName) const;
	int GetSize(const std::string& propertyName) const;
private:
	static const int marginVertical;
	static const int marginHorizontal;
	int rowCount;
	int colCount;

	void initSettings();

	HFONT createNewFontNumbers();
	void updateFontNumbers();
	void updatePen(const std::string& penName, int newColor, int newWidth);
	void updateBrush(const std::string& penName, int newColor);

	POINT countCoord(Point p) const;
	Point getGridPointUpLeft(POINT p) const;
	int countFontSize() const;

	void drawGrid(HDC hdc) const;
	void drawBorders(HDC hdc, const CSlitherlinkGame& game) const;
	void drawBorders(HDC hdc, const std::vector<Edge>& edges) const;
	void drawNumbers(HDC hdc, const CSlitherlinkGame& game);

	int rowHeight;
	int colWidth;
	int rowLength;
	int colLength;

	int windowWidth;
	int windowHeight;

	bool fontChanged;

	LOGFONT logFontNumbers;

	std::unordered_map<std::string, HGDIOBJ> objects;
	std::unordered_map<std::string, COLORREF> colors;
	std::unordered_map<std::string, int> sizes;
};

