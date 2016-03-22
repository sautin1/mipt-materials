// Author: Andrew Sautin

#include "OverlappedWindow.h"

const LPCTSTR COverlappedWindow::className = TEXT("OverlappedWindow");
const LPCTSTR COverlappedWindow::windowName = TEXT("MyNotepad");
const int COverlappedWindow::maxFilenameSize = 1024;
const size_t COverlappedWindow::windowWidth = 500;
const size_t COverlappedWindow::windowHeight = 300;

COverlappedWindow::COverlappedWindow()
{
    handle = 0;
    isChanged = false;
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
    return (::RegisterClassEx(&windowClass) != 0 );
}

bool COverlappedWindow::Create()
{
    CreateWindowEx(0, className, windowName, WS_OVERLAPPEDWINDOW | WS_SIZEBOX,
        CW_USEDEFAULT, CW_USEDEFAULT, windowWidth, windowHeight, 0, 0, GetModuleHandle(0), this);
    return (handle != 0);
}

void COverlappedWindow::Show(int cmdShow)
{
    ShowWindow(handle, cmdShow);
    UpdateWindow(handle);
    editWindow.Show(cmdShow);
}

void COverlappedWindow::OnDestroy()
{
    editWindow.Destroy();
    PostQuitMessage(0);
}

void COverlappedWindow::OnNCCreate(HWND _handle) {
    handle = _handle;
}

void COverlappedWindow::OnCreate()
{
    editWindow.Create(handle);
}

void COverlappedWindow::OnResize()
{
    RECT childRect;
    ::GetClientRect(handle, &childRect);
    int width = childRect.right - childRect.left;
    int height = childRect.bottom - childRect.top;
    SetWindowPos(editWindow.Handle(), 0, childRect.left, childRect.top, width, height, 0);
}

BOOL COverlappedWindow::OnSave()
{
    DWORD bufferSize = SendMessage(editWindow.Handle(), WM_GETTEXTLENGTH, 0, 0);
    BOOL isSaved = false;
    if( bufferSize > 0 ) {
        LPTSTR buffer = new TCHAR[bufferSize + 1];
        memset(buffer, 0, (bufferSize + 1) * sizeof(TCHAR));
        TCHAR filename[maxFilenameSize];
        OPENFILENAME saveInfo;
        memset(filename, 0, maxFilenameSize * sizeof(TCHAR));
        memset(&saveInfo, 0, sizeof(saveInfo));

        SendMessage(editWindow.Handle(), WM_GETTEXT, (WPARAM)(bufferSize + 1), LPARAM(buffer));
        saveInfo.lStructSize = sizeof(OPENFILENAME);
        saveInfo.hwndOwner = handle;
        saveInfo.lpstrFilter = TEXT("All Files\0*.*\0\0");
        saveInfo.lpstrTitle = TEXT("Save As");
        saveInfo.lpstrInitialDir = TEXT("C:\\");
        saveInfo.lpstrFile = (LPTSTR)filename;
        saveInfo.nMaxFile = maxFilenameSize;
        saveInfo.Flags = OFN_PATHMUSTEXIST | OFN_EXPLORER;
        if( GetSaveFileName(&saveInfo) ) {
            HANDLE outFile = CreateFile(saveInfo.lpstrFile, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
            DWORD writtenCount;
            isSaved = WriteFile(outFile, buffer, bufferSize * sizeof(TCHAR), &writtenCount, NULL);
            CloseHandle(outFile);
            if( !isSaved ) {
                MessageBox(handle, TEXT("File cannot be saved"), TEXT("IO Error"), MB_OK);
            }
            else {
                MessageBox(handle, TEXT("File is saved!"), TEXT("Success"), MB_OK);
            }
        }
        delete[] buffer;
    }
    isChanged = !isSaved;
    return isSaved;
}

void COverlappedWindow::OnClose()
{
    BOOL canExit = !isChanged;
    if( isChanged ) {
        int answer = MessageBox(handle, TEXT("Do you want to save changes?"), TEXT("Save on exit"), MB_YESNOCANCEL);
        if( answer == IDYES ) {
            canExit = OnSave();
        } else if( answer == IDNO ) {
            canExit = true;
        }
    }
    if( canExit ) {
        DestroyWindow(handle);
    }
}

void COverlappedWindow::OnChange() {
    isChanged = true;
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
    case WM_SIZE:
    {
        COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnResize();
        return 0;
    }
    case WM_CLOSE:
    {
        COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        window->OnClose();
        return 0;
    }
    case WM_COMMAND:
    {
        COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
        if( HIWORD(wParam) == EN_CHANGE ) {
            window->OnChange();
            return 0;
        } else {
            return DefWindowProc(handle, message, wParam, lParam);
        }
    }
    default:
        return DefWindowProc(handle, message, wParam, lParam);
    }
}
