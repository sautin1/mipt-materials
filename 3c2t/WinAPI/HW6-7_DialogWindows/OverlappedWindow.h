// Author: Andrew Sautin

#pragma once
#include <Windows.h>
#include "EditControl.h"
#include "DialogWindow.h"
#include "resource.h"

class COverlappedWindow {
public:
    COverlappedWindow();
    ~COverlappedWindow();

    static bool RegisterClass();        // Windows class registration
    bool Create();                      // Creating window object
    void Show(int cmdShow);             // Showing window on the screen
    HWND Handle();
    CEditControl EditWindow();
    CDialogWindow DialogWindow();

protected:
    // Event handlers
    void OnDestroy();                   // Destroy window event handler (WM_DESTROY)
    void OnNCCreate(HWND handle);       // Create window context event handler (WM_NCCREATE)
    void OnCreate();                    // Create window event handler (WM_CREATE)
    void OnResize();
    BOOL OnSave();
    void OnClose();
    void OnChange();
    LRESULT OnColorEdit(HDC hdc);

private:
    static const LPCWSTR className;     // Class registration name
    static const LPCWSTR windowName;    // Window name
    static const int maxBufferSize;
    static const size_t windowWidth;
    static const size_t windowHeight;
    static const size_t fontHeightDefault;
    HWND handle;                        // Window handle
    CEditControl editWindow;
    CDialogWindow dialogWindow;
    bool isChanged;
    HBRUSH usedBrush;

    void setDefaultFont();
    static LRESULT __stdcall windowProc(HWND handle, UINT message, WPARAM wParam, LPARAM lParam);
};
