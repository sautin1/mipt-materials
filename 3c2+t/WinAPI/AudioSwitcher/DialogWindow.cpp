// Author: Andrew Sautin

#include "DialogWindow.h"

CDialogWindow::CDialogWindow()
    : timerFontHeight(60), timerFontColor(RGB(0, 0, 255)), tooltipPrefix(L"AudioSwitcher: "),
    isMinimized(false), timeRemaining(-1), timeWaitFileOpen(50)
{
}

CDialogWindow::~CDialogWindow()
{
}

void CDialogWindow::Create(HINSTANCE hInst, HWND parentHwnd)
{
    handle = CreateDialogParam(hInst, MAKEINTRESOURCE(IDD_DIALOG_MAIN), parentHwnd, reinterpret_cast<DLGPROC>(CDialogWindow::dialogProc), reinterpret_cast<LPARAM>(this));
    ShowWindow(handle, SW_SHOW);
}

HWND CDialogWindow::Handle() const
{
    return handle;
}

// on-event methods

void CDialogWindow::OnInit(HWND hDlg)
{
    handle = hDlg;
    std::srand((int)std::time(NULL));
    
    initIcons();
    initEditControls();
    initSpinControls();
    initTimerFields();
    initTrayInfo();

    ::CoInitializeEx(NULL, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE); // for ShellExecute
}

void CDialogWindow::OnButtonBrowse() const
{
    const int maxBufferSize = CSwitcherSettingsSerializer::maxPathLength;
    WCHAR* folderName = new WCHAR[maxBufferSize];
    BROWSEINFO browseInfo;
    browseInfo.hwndOwner = Handle();
    browseInfo.pidlRoot = NULL;
    browseInfo.pszDisplayName = folderName;
    browseInfo.lpszTitle = L"Browse";
    browseInfo.ulFlags = BIF_NEWDIALOGSTYLE;
    browseInfo.lpfn = NULL;
    browseInfo.lParam = 0;
    browseInfo.iImage = -1;

    LPITEMIDLIST lpItem = ::SHBrowseForFolder(&browseInfo);
    if (lpItem && ::SHGetPathFromIDList(lpItem, folderName)) {
        ::SetDlgItemText(Handle(), IDC_EDIT_PATH, folderName);
    }
    delete[] folderName;
}

void CDialogWindow::OnButtonStart()
{
    if (timeRemaining.count() == -1) {
        updateSettingsActive();
        if (switcher.settingsActive.interval.count() > 0) {
            if (CheckDirecoryExists(switcher.settingsActive.path)) {
                startTimer();
                if (isMinimized) {
                    updateTrayIcon();
                }
            } else {
                ::MessageBox(Handle(), L"Error: directory doesn't exist.", L"Wrong Path", MB_ICONWARNING);
            }
        } else {
            ::MessageBox(Handle(), L"Error: time interval cannot be zero.", L"Wrong Interval", MB_ICONWARNING);
        }
    }
}

void CDialogWindow::OnButtonStop()
{
    if (timeRemaining.count() >= 0) {
        ::KillTimer(Handle(), IDT_TIMER_1SEC);
        timeRemaining = std::chrono::duration<int>(-1);
        if (isMinimized) {
            updateTrayIcon();
        }
        updateTimerFields(switcher.settingsActive.interval);
    }
}

void CDialogWindow::OnButtonMinimize()
{
    isMinimized = true;
    ShowWindow(Handle(), SW_HIDE);
    updateTrayIcon();
}

void CDialogWindow::OnMenuOpen()
{
    isMinimized = false;
    ShowWindow(Handle(), SW_SHOW);
    Shell_NotifyIcon(NIM_DELETE, &niData);
}

void CDialogWindow::OnButtonExit()
{
    ::SendMessage(Handle(), WM_CLOSE, 0, 0);
}

bool CDialogWindow::OnCommand(WORD command) {
    switch (command) {
    case IDC_BUTTON_BROWSE:
    {
        OnButtonBrowse();
        return true;
    }
    case IDM_START:
    case IDSTART:
    {
        OnButtonStart();
        return true;
    }
    case IDM_STOP:
    case IDSTOP:
    {
        OnButtonStop();
        return true;
    }
    case IDMINIMIZE:
    {
        OnButtonMinimize();
        return true;
    }
    case IDM_OPEN:
    {
        OnMenuOpen();
        return true;
    }
    case IDM_EXIT:
    case IDEXIT:
    {
        OnButtonExit();
        return true;
    }
    }
    return false;
}

void CDialogWindow::OnPopupMenu()
{
    HMENU hMenuMain = ::LoadMenu(NULL, MAKEINTRESOURCE(IDR_MENU_POPUP));
    HMENU hMenu = ::GetSubMenu(hMenuMain, 0);
    POINT cursorPos;
    ::GetCursorPos(&cursorPos);
    ::SetForegroundWindow(Handle());
    ::TrackPopupMenu(hMenu, TPM_LEFTALIGN | TPM_LEFTBUTTON | TPM_VERNEGANIMATION,
        cursorPos.x, cursorPos.y, 0, Handle(), NULL);
    ::PostMessage(Handle(), WM_NULL, 0, 0);
    ::DestroyMenu(hMenuMain);
}

bool CDialogWindow::OnTrayMessage(UINT msg)
{
    switch (msg) {
    case WM_LBUTTONDBLCLK:
    {
        OnMenuOpen();
        return true;
    }
    case WM_RBUTTONDOWN:
    case WM_CONTEXTMENU:
    {
        OnPopupMenu();
        return true;
    }
    }
    return false;
}

void CDialogWindow::OnTimer()
{
    --timeRemaining;
    updateTimerFields(timeRemaining);
    if (timeRemaining.count() == 0) {
        ::KillTimer(Handle(), IDT_TIMER_1SEC);
        std::wstring fileName = GetRandomFile(switcher.settingsActive.path);
        std::wstring path = AppendFileToPath(switcher.settingsActive.path, fileName);
        //HWND hWndForeground = ::GetForegroundWindow();
        HANDLE hProc;
        int error = OpenFileWithDefaultProgram(path, SW_SHOWMINNOACTIVE/*SW_SHOWMINIMIZED*/, &hProc);
        if (error <= 32) {
            std::wstring errorMsg = L"Error with file \'" + fileName + L"\': " + GetLastErrorAsWString();
            ::MessageBox(NULL, errorMsg.c_str(), L"Cannot open file", NULL);
        } else {
            //while (::GetForegroundWindow() == hWndForeground) {
            //  ::WaitForSingleObject(hProc, timeWaitFileOpen);
            //}
            //::WaitForSingleObject(hProc, timeWaitFileOpen);
            //::SetForegroundWindow(hWndForeground);
            startTimer();
        }
    }
}

LRESULT CDialogWindow::OnCtlColor(HDC hdc, HWND cntrlHandle)
{
    LONG cntrlId = ::GetWindowLong(cntrlHandle, GWL_ID);
    if (cntrlId == IDC_STATIC_TIMER_H || cntrlId == IDC_STATIC_TIMER_M || cntrlId == IDC_STATIC_TIMER_S) {
        ::SetTextColor(hdc, timerFontColor);
        DWORD bkColor = ::GetSysColor(COLOR_MENU);
        ::SetBkColor(hdc, COLORREF(RGB(GetRValue(bkColor), GetGValue(bkColor), GetBValue(bkColor))));
        return (LRESULT)::GetSysColorBrush(COLOR_MENU);
    }
    return NULL;
}

void CDialogWindow::OnClose()
{
    int answer = IDYES;
    if (timeRemaining.count() >= 0) {
        answer = ::MessageBox(Handle(), L"The timer is still running. Exit anyway?", L"Exit", MB_YESNO);
    }
    if (answer == IDYES) {
        ::KillTimer(Handle(), IDT_TIMER_1SEC);
        if (isMinimized) {
            Shell_NotifyIcon(NIM_DELETE, &niData);
        }
        HWND checkCtrl = ::GetDlgItem(Handle(), IDC_CHECK_SET_DEFAULT);
        if (Button_GetCheck(checkCtrl) == BST_CHECKED) {
            updateSettingsActive();
            switcher.settingsDefault = switcher.settingsActive;
            CSwitcherSettingsSerializer::WriteSettingsToFile(switcher.GetSettingsFileName(), switcher.settingsDefault);
        }
        ::DestroyWindow(Handle());
    }
}

void CDialogWindow::OnDestroy()
{
    ::DestroyIcon(hIcon);
    ::DestroyIcon(hIconSmall);
    ::DeleteObject(timerFont);
    ::PostQuitMessage(0);
}

// init methods

void CDialogWindow::createTimerFieldsFont()
{
    timerFont = (HFONT)::SendDlgItemMessage(Handle(), IDC_STATIC_TIMER_H, WM_GETFONT, 0, 0);
    if (!timerFont) {
        timerFont = (HFONT)::GetStockObject(DEFAULT_GUI_FONT);
    }
    LOGFONT logFont;
    ::GetObject(timerFont, sizeof(LOGFONT), &logFont);
    logFont.lfHeight = timerFontHeight;
    logFont.lfWidth = 0;
    logFont.lfWeight = FW_SEMIBOLD;
    timerFont = CreateFontIndirect(&logFont);
}

void CDialogWindow::initIcons()
{
    hIconSmall = (HICON)::LoadImage(GetModuleHandle(0), MAKEINTRESOURCE(IDI_ICON_TRAY), IMAGE_ICON,
        GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
    hIcon = (HICON)::LoadImage(GetModuleHandle(0), MAKEINTRESOURCE(IDI_ICON_TRAY), IMAGE_ICON,
        GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON), LR_DEFAULTCOLOR);
    ::SendMessage(Handle(), WM_SETICON, ICON_BIG, (LPARAM)hIcon);
    ::SendMessage(Handle(), WM_SETICON, ICON_SMALL, (LPARAM)hIconSmall);
}

void CDialogWindow::initEditControls()
{
    switcher.InitSwitcherSettings();
    ::SetDlgItemText(Handle(), IDC_EDIT_PATH, switcher.settingsActive.path.c_str());
    std::wstring timeStr = ConvertToWString<int>(GetIntervalHours(switcher.settingsActive.interval));
    
    ::SetDlgItemText(Handle(), IDC_EDIT_TIME_H, timeStr.c_str());
    
    timeStr = ConvertToWString<int>(GetIntervalMinutes(switcher.settingsActive.interval));
    ::SetDlgItemText(Handle(), IDC_EDIT_TIME_MIN, timeStr.c_str());
    
    timeStr = ConvertToWString<int>(GetIntervalSeconds(switcher.settingsActive.interval));
    ::SetDlgItemText(Handle(), IDC_EDIT_TIME_SEC, timeStr.c_str());
}

void CDialogWindow::initSpinControls()
{
    ::SendDlgItemMessage(Handle(), IDC_SPIN_TIME_H, UDM_SETRANGE, 0, MAKELPARAM(23, 0));
    ::SendDlgItemMessage(Handle(), IDC_SPIN_TIME_MIN, UDM_SETRANGE, 0, MAKELPARAM(59, 0));
    ::SendDlgItemMessage(Handle(), IDC_SPIN_TIME_SEC, UDM_SETRANGE, 0, MAKELPARAM(59, 0));
}

void CDialogWindow::initTimerFields()
{
    createTimerFieldsFont();
    ::SendDlgItemMessage(Handle(), IDC_STATIC_TIMER_H, WM_SETFONT, reinterpret_cast<WPARAM>(timerFont), true);
    ::SendDlgItemMessage(Handle(), IDC_STATIC_TIMER_M, WM_SETFONT, reinterpret_cast<WPARAM>(timerFont), true);
    ::SendDlgItemMessage(Handle(), IDC_STATIC_TIMER_S, WM_SETFONT, reinterpret_cast<WPARAM>(timerFont), true);
    updateTimerFields(switcher.settingsActive.interval);
}

void CDialogWindow::initTrayInfo()
{
    ::ZeroMemory(&niData, sizeof(NOTIFYICONDATA));
    niData.cbSize = sizeof(NOTIFYICONDATA);
    niData.uID = TRAY_ICON_ID;
    niData.uFlags = NIF_ICON | NIF_MESSAGE | NIF_TIP;
    niData.hIcon = hIconSmall;
    niData.hWnd = Handle();
    niData.uCallbackMessage = TRAY_ICON_MESSAGE;
}

// update methods

void CDialogWindow::updateTrayIcon()
{
    if (isMinimized) {
        std::wstring tooltipSuffix = (timeRemaining.count() >= 0) ? L"Running" : L"Stopped";
        std::wstring tooltip = tooltipPrefix + tooltipSuffix;

        Shell_NotifyIcon(NIM_DELETE, &niData);
        ::StringCchCopy(niData.szTip, 128, tooltip.c_str());
        Shell_NotifyIcon(NIM_ADD, &niData);
        Shell_NotifyIcon(NIM_SETVERSION, &niData);
    }
}

void CDialogWindow::updateTimerFields(const std::chrono::duration<int>& interval) const
{
    std::wstring timeStr = AddLeadingZeros(ConvertToWString<int>(GetIntervalHours(interval)), 2);
    ::SetDlgItemText(Handle(), IDC_STATIC_TIMER_H, timeStr.c_str());
    timeStr = AddLeadingZeros(ConvertToWString<int>(GetIntervalMinutes(interval)), 2);
    ::SetDlgItemText(Handle(), IDC_STATIC_TIMER_M, timeStr.c_str());
    timeStr = AddLeadingZeros(ConvertToWString<int>(GetIntervalSeconds(interval)), 2);
    ::SetDlgItemText(Handle(), IDC_STATIC_TIMER_S, timeStr.c_str());
}

void CDialogWindow::updateSettingsActive()
{
    switcher.settingsActive.interval = CreateIntervalSeconds(
        extractTimeFromEditControl(IDC_EDIT_TIME_H),
        extractTimeFromEditControl(IDC_EDIT_TIME_MIN),
        extractTimeFromEditControl(IDC_EDIT_TIME_SEC)
    );
    switcher.settingsActive.path = extractPathFromEditControl();
}

int CDialogWindow::extractTimeFromEditControl(int dlgCntrlId) const
{
    WCHAR timeStr[3];
    ::GetDlgItemText(Handle(), dlgCntrlId, timeStr, 3);
    return ConvertFromWString<int>(std::wstring(timeStr));
}

std::wstring CDialogWindow::extractPathFromEditControl() const
{
    WCHAR* pathStr = new WCHAR[CSwitcherSettingsSerializer::maxPathLength];
    ::GetDlgItemText(Handle(), IDC_EDIT_PATH, pathStr, CSwitcherSettingsSerializer::maxPathLength);
    std::wstring res(pathStr);
    delete[] pathStr;
    return res;
}

void CDialogWindow::startTimer() {
    timeRemaining = switcher.settingsActive.interval;
    updateTimerFields(timeRemaining);
    SetTimer(Handle(), IDT_TIMER_1SEC, 1000, 0);
}

INT_PTR CDialogWindow::dialogProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam) 
{
    if (uMsg == WM_INITDIALOG) {
        CDialogWindow* window = reinterpret_cast<CDialogWindow*>(lParam);
        ::SetWindowLongPtr(hDlg, GWLP_USERDATA, (LONG)window);
        window->OnInit(hDlg);
        return true;
    }
    CDialogWindow* window = reinterpret_cast<CDialogWindow*>(GetWindowLongPtr(hDlg, GWLP_USERDATA));
    switch (uMsg) {
    case WM_COMMAND:
    {
        return window->OnCommand(LOWORD(wParam));
    }
    case WM_TIMER:
    {
        window->OnTimer();
        return true;
    }
    case WM_CTLCOLORSTATIC:
    {
        return window->OnCtlColor(reinterpret_cast<HDC>(wParam), (HWND)lParam);
    }
    case TRAY_ICON_MESSAGE:
    {
        return window->OnTrayMessage((UINT)lParam);
    }
    case WM_CLOSE:
    {
        window->OnClose();
        return true;
    }
    case WM_DESTROY:
    {
        window->OnDestroy();
        return true;
    }
    }
    return false;
}
