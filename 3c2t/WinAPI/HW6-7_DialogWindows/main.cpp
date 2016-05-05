// Author: Andrew Sautin

#include <Windows.h>
#include "OverlappedWindow.h"

int WINAPI wWinMain(HINSTANCE instance, HINSTANCE prevInstance, LPWSTR commandLine, int cmdShow)
{
    if( !COverlappedWindow::RegisterClass() ) {
        return -1;
    }
    COverlappedWindow window;
    if( !window.Create() ) {
        return -1;
    }
    window.Show(cmdShow);
    HACCEL acceleratorHandle = ::LoadAccelerators(GetModuleHandle(0), MAKEINTRESOURCE(IDR_ACCELERATOR1));
    MSG message;
    BOOL getMessageResult = 0;
    while( getMessageResult = ::GetMessage(&message, 0, 0, 0) ) {
        if( getMessageResult == -1 ) {
            return -1;
        } else {
            if( !TranslateAccelerator(window.Handle(), acceleratorHandle, &message) && !IsDialogMessage(window.DialogWindow().Handle(), &message) ) {
                ::TranslateMessage(&message);
                ::DispatchMessage(&message);
            }
        }
    }
    DestroyAcceleratorTable(acceleratorHandle);
    return 0;
}
