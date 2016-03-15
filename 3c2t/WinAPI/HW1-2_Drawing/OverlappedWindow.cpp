#include "OverlappedWindow.h"

const double COverlappedWindow::timeDiff = 0.6;
const UINT COverlappedWindow::timerDuration = 1;
const LPCTSTR COverlappedWindow::className = L"OverlappedWindow";
const LPCTSTR COverlappedWindow::windowName = L"MyCoolWindow";
const int COverlappedWindow::radius = 30;
const COLORREF COverlappedWindow::circleBackgroundColor = RGB(0, 255, 255);
const COLORREF COverlappedWindow::circleBorderColor = RGB(0, 0, 255);

COverlappedWindow::COverlappedWindow()
{
    handle = 0;
    timerId = 0;
    circleTime = 0;
}

COverlappedWindow::~COverlappedWindow()
{
}

bool COverlappedWindow::RegisterClass()
{
    WNDCLASSEX windowClass;
    ::ZeroMemory(&windowClass, sizeof(windowClass));
    windowClass.cbSize = sizeof(WNDCLASSEX);
    windowClass.lpfnWndProc = COverlappedWindow::windowProc;
    windowClass.hInstance = GetModuleHandle(0);
    windowClass.lpszClassName = className;
    windowClass.hbrBackground = 0;
    return (::RegisterClassEx(&windowClass) != 0);
}

bool COverlappedWindow::Create()
{
    CreateWindowEx(0, className, windowName, WS_OVERLAPPEDWINDOW | WS_SIZEBOX, 
        CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, 0, 0, GetModuleHandle(0), this);
    return (handle != 0);
}

void COverlappedWindow::Show(int cmdShow)
{
    ShowWindow(handle, cmdShow);
}

void COverlappedWindow::OnDestroy()
{
    KillTimer(handle, timerId);
    PostQuitMessage(0);
}

void COverlappedWindow::OnNCCreate(HWND _handle) {
    handle = _handle;
}

void COverlappedWindow::OnCreate()
{
    timerId = SetTimer(handle, 0, timerDuration, 0);
}

void COverlappedWindow::drawCircle(HDC context, int centerX, int centerY)
{
    HPEN pen = CreatePen(PS_DASH, 2, circleBorderColor);
    HBRUSH inBrush = CreateSolidBrush(circleBackgroundColor);
    HGDIOBJ oldPen = SelectObject(context, pen);
    HGDIOBJ oldBrush = SelectObject(context, inBrush);
    Ellipse(context, centerX - radius, centerY - radius, centerX + radius, centerY + radius);
    SelectObject(context, oldPen);
    SelectObject(context, oldBrush);
    DeleteObject(inBrush);
    DeleteObject(pen);
}

void COverlappedWindow::OnPaint()
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

    /*FillRect(displayBufferDC, &clientRect, (HBRUSH)GetStockObject(GRAY_BRUSH));
    drawCircle(displayBufferDC, centerX, centerY);
    BitBlt(windowDC, clientRect.left, clientRect.top, clientRect.right - clientRect.left, clientRect.bottom - clientRect.top, 
        displayBufferDC, 0, 0, SRCCOPY);*/
    drawCircle(windowDC, centerX, centerY);

    SelectObject(displayBufferDC, oldDisplayBuffer);
    DeleteObject(displayBuffer);
    DeleteDC(displayBufferDC);
    ::EndPaint(handle, &paintStruct);
}

void COverlappedWindow::OnTimer()
{
    RECT rect;
    ::GetClientRect(handle, &rect);
    circleTime += timeDiff;
    InvalidateRect(handle, &rect, FALSE);
}

LRESULT COverlappedWindow::windowProc(HWND handle, UINT message, WPARAM wParam, LPARAM lParam)
{
    switch (message) {
    case WM_NCCREATE:
    {
        COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>((reinterpret_cast<CREATESTRUCT*>(lParam))->lpCreateParams);
        SetLastError(0);
        SetWindowLongPtr(handle, GWLP_USERDATA, (LONG)window);
        if (GetLastError() != 0) {
            return GetLastError();
        }
        window->OnNCCreate(handle);
        return DefWindowProc(handle, message, wParam, lParam);
    }
    case WM_CREATE:
    {
        COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnCreate();
        return DefWindowProc(handle, message, wParam, lParam);
    }
    case WM_DESTROY:
    {
        COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnDestroy();
        return 0;
    }
    case WM_PAINT:
    {
        COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnPaint();
        return 0;
    }
    case WM_TIMER:
    {
        COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnTimer();
        return 0;
    }
    default:
        return DefWindowProc(handle, message, wParam, lParam);
    }
}
