// Author: Andrew Sautin

#pragma once
#include <algorithm>
#include <string>
#include <vector>
#include <Windows.h>
#include "EllipseWindow.h"

class COverlappedWindow {
public:
    COverlappedWindow();
    ~COverlappedWindow();

    static bool RegisterClass();        // Windows class registration
    bool Create();                      // Creating window object
    void Show(int cmdShow);             // Showing window on the screen
protected:
    // Event handlers
    void OnDestroy();                   // Destroy window event handler (WM_DESTROY)
    void OnNCCreate(HWND handle);       // Create window context event handler (WM_NCCREATE)
    void OnCreate();                    // Create window event handler (WM_CREATE)
    void OnPaint();                     // Repaint request event handler (WM_PAINT)
    void OnResize();
    void OnClick();
private:
    static const LPCTSTR className;     // Class registration name
    static const LPCTSTR windowName;    // Window name
    static const int colCount;
    static const int rowCount;
    HWND handle;                        // Window handle
    std::vector<CEllipseWindow> childWindows;

    static LRESULT __stdcall windowProc(HWND handle, UINT message, WPARAM wParam, LPARAM lParam);
    RECT countChildRect(int childId);
};
