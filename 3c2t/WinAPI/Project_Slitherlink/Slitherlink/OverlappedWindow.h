// Author: Andrew Sautin

#pragma once
#define NOMINMAX
#include <algorithm>
#include <vector>
#include <Windows.h>

#include "DialogWindow.h"
#include "DebugUtils.h"
#include "GameDesign.h"
#include "SlitherlinkGame.h"
#include "resource.h"

//#include <cmath>

class COverlappedWindow {
public:
	COverlappedWindow();
	~COverlappedWindow();

	static bool RegisterClass();		// Windows class registration
	bool Create();						// Creating window object
	void Show(int cmdShow);				// Showing window on the screen
	void Repaint();
	HWND Handle();
	CDialogWindow DialogWindow();
protected:
	// Event handlers
	void OnDestroy();					// Destroy window event handler (WM_DESTROY)
	void OnNCCreate(HWND handle);		// Create window context event handler (WM_NCCREATE)
	void OnCreate();					// Create window event handler (WM_CREATE)
	void OnResize();
	void OnClick(int clickX, int clickY);
	void OnVictory() const;
	void OnRightClick(int clickX, int clickY);
	bool OnSave();
	void OnReset();
	void OnLoad();
	void OnPaint();
	void OnClose();
	LRESULT OnColorEdit(HDC hdc);

private:
	static const LPCWSTR className;		// Class registration name
	static const LPCWSTR windowName;	// Window name
	static const int maxBufferSize;

	static int windowWidth;
	static int windowHeight;
	
	HWND handle;						// Window handle
	CDialogWindow dialogWindow;
	CSlitherlinkGame game;
	CGameDesign drawer;

	bool isChanged;

	static LRESULT __stdcall windowProc(HWND handle, UINT message, WPARAM wParam, LPARAM lParam);
};