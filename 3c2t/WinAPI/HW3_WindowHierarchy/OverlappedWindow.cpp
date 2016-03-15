// Author: Andrew Sautin

#include "OverlappedWindow.h"

const LPCTSTR COverlappedWindow::className = L"OverlappedWindow";
const LPCTSTR COverlappedWindow::windowName = L"MyCoolWindow";
const int COverlappedWindow::colCount = 4;
const int COverlappedWindow::rowCount = 5;

COverlappedWindow::COverlappedWindow()
    : childWindows(colCount * rowCount)
{
    handle = 0;
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
    for( int i = 0; i < rowCount * colCount; ++i ) {
        childWindows[i].Show(cmdShow);
    }
}

void COverlappedWindow::OnDestroy()
{
    PostQuitMessage(0);
}

void COverlappedWindow::OnNCCreate(HWND _handle) {
    handle = _handle;
}

RECT COverlappedWindow::countChildRect(int childId) {
    RECT clientRect;
    ::GetClientRect(handle, &clientRect);
    int width = (clientRect.right - clientRect.left) / colCount;
    int height = (clientRect.bottom - clientRect.top) / rowCount;
    int horizontalOffset = childId % colCount;
    int verticalOffset = childId / colCount;
    clientRect.top = verticalOffset * height;
    clientRect.bottom = clientRect.top + height;
    clientRect.left = horizontalOffset * width;
    clientRect.right = clientRect.left + width;
    return clientRect;
}

void COverlappedWindow::OnCreate()
{
    for( int i = 0; i < rowCount * colCount; ++i ) {
        RECT childRect = countChildRect(i);
        childWindows[i].Create(handle, childRect);
    }
}

void COverlappedWindow::OnPaint()
{
    RECT clientRect;
    PAINTSTRUCT paintStruct;
    HDC windowDC = ::BeginPaint(handle, &paintStruct);
    ::GetClientRect(handle, &clientRect);
    ::EndPaint(handle, &paintStruct);
}

void COverlappedWindow::OnResize()
{
    for( int i = 0; i < rowCount * colCount; ++i ) {
        RECT childRect = countChildRect(i);
        int width = childRect.right - childRect.left;
        int height = childRect.bottom - childRect.top;
        SetWindowPos(childWindows[i].Handle(), 0, childRect.left, childRect.top, width, height, 0);
    }
}

LRESULT COverlappedWindow::windowProc(HWND handle, UINT message, WPARAM wParam, LPARAM lParam)
{
    switch (message) {
    case WM_NCCREATE:
    {
        COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>((reinterpret_cast<CREATESTRUCT*>(lParam))->lpCreateParams);
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
    case WM_SIZE:
    {
        COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnResize();
    }
    default:
        return DefWindowProc(handle, message, wParam, lParam);
    }
}
