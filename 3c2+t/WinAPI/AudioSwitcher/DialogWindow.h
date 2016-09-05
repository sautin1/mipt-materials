// Author: Andrew Sautin

#pragma once
#define TRAY_ICON_MESSAGE               0xBFFE
#define TRAY_ICON_ID                    1
#define IDT_TIMER_1SEC                  1

#include <ctime>
#include <sstream>
#include <vector>

#include <Windows.h>
#include <Shlobj.h>     // for SHBrowseForFolder, SHGetPathFromIDList, BROWSEINFO and other
#include <Strsafe.h>    // for StringCchCopy
#include <Windowsx.h>   // for Button_GetCheck

#include "FileUtils.h"
#include "IntervalUtils.h"
#include "resource.h"
#include "Switcher.h"
#include "WinUtils.h"

class CDialogWindow {
public:
    CDialogWindow();
    ~CDialogWindow();

    void Create(HINSTANCE hInst, HWND parentHwnd);
    HWND Handle() const;

protected:
    void OnInit(HWND hDlg);

    void OnButtonBrowse() const;
    void OnButtonStart();
    void OnButtonStop();
    void OnButtonMinimize();
    void OnMenuOpen();
    void OnButtonExit();
    bool OnCommand(WORD command);

    void OnPopupMenu();
    bool OnTrayMessage(UINT msg);
    void OnTimer();
    LRESULT OnCtlColor(HDC hdc, HWND cntrlHandle);
    void OnClose();
    void OnDestroy();

private:
    const int timeWaitFileOpen;
    const int timerFontHeight;
    const COLORREF timerFontColor;
    const std::wstring tooltipPrefix;

    HWND handle;
    CSwitcher switcher;
    std::chrono::duration<int> timeRemaining;
    bool isMinimized;

    NOTIFYICONDATA niData;
    HICON hIcon;
    HICON hIconSmall;
    HFONT timerFont;

    void createTimerFieldsFont();
    void initIcons();
    void initEditControls();
    void initSpinControls();
    void initTimerFields();
    void initTrayInfo();

    void updateTrayIcon();
    void updateTimerFields(const std::chrono::duration<int>& interval) const;
    void updateSettingsActive();

    int extractTimeFromEditControl(int dlgCntrlId) const;
    std::wstring extractPathFromEditControl() const;

    void startTimer();

    static INT_PTR CALLBACK dialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
};
