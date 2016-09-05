// Author: Andrew Sautin

#include <Windows.h>

#include "DialogWindow.h"

int WINAPI wWinMain(HINSTANCE instance, HINSTANCE prevInstance, LPWSTR commandLine, int cmdShow)
{
    CDialogWindow dialogWindow;
    dialogWindow.Create(instance, 0);

    MSG message;
    BOOL getMessageResult = 0;
    while (getMessageResult = ::GetMessage(&message, 0, 0, 0)) {
        if (getMessageResult == -1) {
            return -1;
        } else if (!IsDialogMessage(dialogWindow.Handle(), &message)) {
            ::TranslateMessage(&message);
            ::DispatchMessage(&message);
        }
    }
    return 0;
}
