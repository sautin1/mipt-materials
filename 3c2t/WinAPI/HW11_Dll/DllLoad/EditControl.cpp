// Author: Andrew Sautin

#include "EditControl.h"

const LPCWSTR CEditControl::className = L"EDIT";

CEditControl::CEditControl()
{
    handle = 0;
}

CEditControl::~CEditControl()
{
}

bool CEditControl::Create(HWND parentHandle)
{
    RECT parentRect;
    GetClientRect(parentHandle, &parentRect);
    int width = parentRect.right - parentRect.left;
    int height = parentRect.bottom - parentRect.top;
    handle = CreateWindowEx(0, className, NULL, WS_CHILD | WS_VISIBLE | WS_VSCROLL |
        ES_LEFT | ES_MULTILINE | ES_AUTOVSCROLL, parentRect.left, parentRect.top,
        width, height, parentHandle, (HMENU)1, GetModuleHandle(0), this);
    return (handle != 0);
}

void CEditControl::Show(int cmdShow)
{
    ShowWindow(handle, cmdShow);
    UpdateWindow(handle);
}

HWND CEditControl::Handle() {
    return handle;
}
 
void CEditControl::Destroy()
{
    PostQuitMessage(0);
}
