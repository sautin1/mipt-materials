// Author: Andrew Sautin

#include "EllipseWindow.h"

const double CEllipseWindow::timeDiff = 0.2;
const UINT CEllipseWindow::timerDuration = 30;
const LPCTSTR CEllipseWindow::className = L"CEllipseWindow";
const LPCTSTR CEllipseWindow::windowName = L"MyEllipseWindow";
const int CEllipseWindow::radius = 30;
const COLORREF CEllipseWindow::circleBackgroundColor = RGB(0, 255, 255);
const COLORREF CEllipseWindow::circleClickedBackgroundColor = RGB(255, 255, 0);
const COLORREF CEllipseWindow::circleBorderColor = RGB(0, 0, 255);

CEllipseWindow::CEllipseWindow()
{
    handle = 0;
    timerId = 0;
    circleTime = 0;
}

CEllipseWindow::~CEllipseWindow()
{
}

bool CEllipseWindow::RegisterClass()
{
    WNDCLASSEX windowClass;
    ::ZeroMemory(&windowClass, sizeof(windowClass));
    windowClass.cbSize = sizeof(WNDCLASSEX);
    windowClass.lpfnWndProc = CEllipseWindow::windowProc;
    windowClass.hInstance = GetModuleHandle(0);
    windowClass.lpszClassName = className;
    windowClass.hbrBackground = 0;
    return (::RegisterClassEx(&windowClass) != 0);
}

bool CEllipseWindow::Create(HWND parentHandle, RECT childRect)
{
    int width = childRect.right - childRect.left;
    int height = childRect.bottom - childRect.top;
    //circleTime = childRect.right + childRect.bottom;
    CreateWindowEx(0, className, windowName, WS_CHILD | WS_BORDER, childRect.left, childRect.top, 
        width, height, parentHandle, 0, GetModuleHandle(0), this);
    return (handle != 0);
}

void CEllipseWindow::Show(int cmdShow)
{
    ShowWindow(handle, cmdShow);
}

HWND CEllipseWindow::Handle() {
    return handle;
}

void CEllipseWindow::OnDestroy()
{
    KillTimer(handle, timerId);
    PostQuitMessage(0);
}

void CEllipseWindow::OnNCCreate(HWND _handle) {
    handle = _handle;
}

void CEllipseWindow::OnCreate()
{
    timerId = SetTimer(handle, 0, timerDuration, 0);
}

void CEllipseWindow::drawCircle(HDC context, int centerX, int centerY)
{
    HPEN pen = CreatePen(PS_DASH, 2, circleBorderColor);
    HBRUSH inBrush = (handle == ::GetFocus()) ? CreateSolidBrush(circleClickedBackgroundColor) : CreateSolidBrush(circleBackgroundColor);
    HGDIOBJ oldPen = SelectObject(context, pen);
    HGDIOBJ oldBrush = SelectObject(context, inBrush);
    Ellipse(context, centerX - radius, centerY - radius, centerX + radius, centerY + radius);
    SelectObject(context, oldPen);
    SelectObject(context, oldBrush);
    DeleteObject(inBrush);
    DeleteObject(pen);
}

void CEllipseWindow::OnPaint()
{
    RECT clientRect;
    PAINTSTRUCT paintStruct;
    HDC windowDC = ::BeginPaint(handle, &paintStruct);
    ::GetClientRect(handle, &clientRect);

    HDC displayBufferDC = CreateCompatibleDC(windowDC);
    HBITMAP displayBuffer = CreateCompatibleBitmap(
        windowDC,
        clientRect.right - clientRect.left,
        clientRect.bottom - clientRect.top
        );
    HGDIOBJ oldDisplayBuffer = SelectObject(displayBufferDC, displayBuffer);

    int movementRadius = std::min<int>((clientRect.right - clientRect.left) / 2,
        (clientRect.bottom - clientRect.top) / 2) - radius;
    int centerX = (clientRect.left + clientRect.right) / 2 + movementRadius * std::cos(circleTime);
    int centerY = (clientRect.top + clientRect.bottom) / 2 + movementRadius * std::sin(circleTime);

    FillRect(displayBufferDC, &clientRect, (HBRUSH)GetStockObject(GRAY_BRUSH));
    drawCircle(displayBufferDC, centerX, centerY);
    BitBlt(windowDC, clientRect.left, clientRect.top, clientRect.right - clientRect.left, clientRect.bottom - clientRect.top,
        displayBufferDC, 0, 0, SRCCOPY);

    SelectObject(displayBufferDC, oldDisplayBuffer);
    DeleteObject(displayBuffer);
    DeleteDC(displayBufferDC);
    ::EndPaint(handle, &paintStruct);
}

void CEllipseWindow::OnTimer()
{
    RECT rect;
    ::GetClientRect(handle, &rect);
    circleTime += timeDiff;
    InvalidateRect(handle, &rect, FALSE);
}

void CEllipseWindow::OnClick()
{
    SetFocus(handle);
}

LRESULT CEllipseWindow::windowProc(HWND handle, UINT message, WPARAM wParam, LPARAM lParam)
{
    switch (message) {
    case WM_NCCREATE:
    {
        CEllipseWindow* window = reinterpret_cast<CEllipseWindow*>((reinterpret_cast<CREATESTRUCT*>(lParam))->lpCreateParams);
        SetLastError(0);
        SetWindowLongPtr(handle, GWLP_USERDATA, (LONG)window);
        if( GetLastError() != 0 ) {
            return GetLastError();
        }
        window->OnNCCreate(handle);
        return DefWindowProc(handle, message, wParam, lParam);
    }
    case WM_CREATE:
    {
        CEllipseWindow* window = reinterpret_cast<CEllipseWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnCreate();
        return DefWindowProc(handle, message, wParam, lParam);
    }
    case WM_DESTROY:
    {
        CEllipseWindow* window = reinterpret_cast<CEllipseWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnDestroy();
        return 0;
    }
    case WM_PAINT:
    {
        CEllipseWindow* window = reinterpret_cast<CEllipseWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnPaint();
        return 0;
    }
    case WM_TIMER:
    {
        CEllipseWindow* window = reinterpret_cast<CEllipseWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnTimer();
        return 0;
    }
    case WM_LBUTTONDOWN:
    {
        CEllipseWindow* window = reinterpret_cast<CEllipseWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnClick();
        return 0;
    }
    default:
        return DefWindowProc(handle, message, wParam, lParam);
    }
}
