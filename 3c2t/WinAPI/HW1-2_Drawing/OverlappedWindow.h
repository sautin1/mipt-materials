#pragma once
#include <algorithm>
#include <string>
#include <Windows.h>

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
    void OnTimer();                     // Timer stopped event handler (WM_TIMER)
private:
    static const double timeDiff;       // Time difference - added to time every moment WM_TIMER was received
    static const int radius;            // Circle radius
    static const LPCTSTR className;     // Class registration name
    static const LPCTSTR windowName;    // Window name
    static const COLORREF circleBackgroundColor;
    static const COLORREF circleBorderColor;
    static const UINT timerDuration;

    HWND handle;                        // Window handle
    UINT_PTR timerId;                   // Timer identificator
    double circleTime;                  // Time parameter (for circle movement)

    static LRESULT __stdcall windowProc(HWND handle, UINT message, WPARAM wParam, LPARAM lParam);

    void drawCircle(HDC context, int x, int y);
};
