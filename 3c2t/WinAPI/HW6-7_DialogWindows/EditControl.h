// Author: Andrew Sautin

#pragma once
#include <Windows.h>

class CEditControl {
public:
    CEditControl();
    ~CEditControl();

    bool Create(HWND parentHandle);
    void Show(int cmdShow);             // Showing window on the screen
    HWND Handle();
    void Destroy();
private:
    static const LPCWSTR className;     // Class registration name
    HWND handle;                        // Window handle
};
