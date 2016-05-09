// Author: Andrew Sautin

#include "DialogWindow.h"
#include "OverlappedWindow.h"

const int CDialogWindow::fontMin = 8;
const int CDialogWindow::fontMax = 72;
const int CDialogWindow::opacityMin = 0;
const int CDialogWindow::opacityMax = 255;

CDialogWindow::CSettings::CSettings(LOGFONT _font, int _opacity, COLORREF _fontColor, COLORREF _backgroundColor)
    : font(_font), opacity(_opacity), fontColor(_fontColor), backgroundColor(_backgroundColor) 
{
}

CDialogWindow::CDialogWindow()
{
    settingsDefault.opacity = 255;
    settingsDefault.backgroundColor = RGB(255, 255, 255);
    settingsDefault.fontColor = RGB(0, 0, 0);
    settingsNew = settingsDefault;
}

CDialogWindow::~CDialogWindow()
{
}

void CDialogWindow::Create(HWND parentHwnd)
{
    parentHandle = parentHwnd;
    HINSTANCE hInstance = (HINSTANCE)::GetWindowLong(parentHwnd, GWL_HINSTANCE);
    //::DialogBoxParam(GetModuleHandle(0), MAKEINTRESOURCE(IDD_DIALOG1), parentHwnd, reinterpret_cast<DLGPROC>(CDialogWindow::dialogProc), reinterpret_cast<LPARAM>(this));
    CreateDialogParam(GetModuleHandle(0), MAKEINTRESOURCE(IDD_DIALOG1), parentHwnd, reinterpret_cast<DLGPROC>(CDialogWindow::dialogProc), reinterpret_cast<LPARAM>(this));
    ShowWindow(handle, SW_SHOW);
}

CDialogWindow::CSettings CDialogWindow::SettingsNew() const {
    return settingsNew;
}

void CDialogWindow::InitScrolls(HWND hwndDlg)
{
    SendDlgItemMessage(hwndDlg, IDC_SLIDER_FONT, TBM_SETRANGEMIN, true, fontMin);
    SendDlgItemMessage(hwndDlg, IDC_SLIDER_FONT, TBM_SETRANGEMAX, true, fontMax);
    SendDlgItemMessage(hwndDlg, IDC_SLIDER_FONT, TBM_SETPOS, true, settingsDefault.font.lfHeight);
    SendDlgItemMessage(hwndDlg, IDC_SLIDER_OPACITY, TBM_SETRANGEMIN, true, opacityMin);
    SendDlgItemMessage(hwndDlg, IDC_SLIDER_OPACITY, TBM_SETRANGEMAX, true, opacityMax);
    SendDlgItemMessage(hwndDlg, IDC_SLIDER_OPACITY, TBM_SETPOS, true, settingsDefault.opacity);
}

HWND CDialogWindow::Handle() const
{
    return handle;
}

void CDialogWindow::ApplyFontSize(COverlappedWindow* parentWindow, CSettings settings) {
    DeleteObject(usedFont);
    usedFont = ::CreateFontIndirect(&settings.font);
    SendMessage(parentWindow->EditWindow().Handle(), WM_SETFONT, reinterpret_cast<WPARAM>(usedFont), true);
}

void CDialogWindow::ApplyOpacity(COverlappedWindow* parentWindow, CSettings settings)
{
    ::SetLayeredWindowAttributes(parentWindow->Handle(), 0, settings.opacity, LWA_ALPHA);
}

void CDialogWindow::ApplySettings(CSettings settings)
{
    COverlappedWindow* parentWindow = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(parentHandle, GWLP_USERDATA));
    ApplyFontSize(parentWindow, settings);
    ApplyOpacity(parentWindow, settings);
}

void CDialogWindow::MakePreview()
{
    if( isPreview ) {
        ApplySettings(settingsNew);
    }
}

void CDialogWindow::ChangeColor(HWND dialogHandle, COLORREF& color)
{
    CHOOSECOLOR colorInfo;
    ::ZeroMemory(&colorInfo, sizeof(colorInfo));
    COLORREF acrCustClr[16];
    colorInfo.lStructSize = sizeof(CHOOSECOLOR);
    colorInfo.Flags = CC_FULLOPEN | CC_RGBINIT;
    colorInfo.lpCustColors = (LPDWORD)acrCustClr;
    colorInfo.rgbResult = color;
    colorInfo.hwndOwner = ::GetParent(dialogHandle);
    colorInfo.hInstance = dialogHandle;
    if (::ChooseColor(&colorInfo)) {
        color = colorInfo.rgbResult;
    }
}

void CDialogWindow::OnInit(HWND hwndDlg)
{
    isPreview = false;
    handle = hwndDlg;

    COverlappedWindow* parentWindow = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(parentHandle, GWLP_USERDATA));
    usedFont = reinterpret_cast<HFONT>(SendMessage(parentWindow->EditWindow().Handle(), WM_GETFONT, 0, 0));
    ::GetObject(usedFont, sizeof(LOGFONT), &(settingsDefault.font));
    settingsNew = settingsDefault;
    ::ZeroMemory(&diff, sizeof(diff));
    InitScrolls(hwndDlg);
}

void CDialogWindow::OnScroll(HWND hwndDlg, HWND scrollCtrl) {
    int scrollId = ::GetDlgCtrlID(scrollCtrl);
    if( scrollId == IDC_SLIDER_FONT ) {
        settingsNew.font.lfHeight = SendDlgItemMessage(hwndDlg, IDC_SLIDER_FONT, TBM_GETPOS, 0, 0);
        //diff.fontSize = true;
    } else {
        settingsNew.opacity = SendDlgItemMessage(hwndDlg, IDC_SLIDER_OPACITY, TBM_GETPOS, 0, 0);
        //diff.opacity = true;
    }
    MakePreview();
}

bool CDialogWindow::OnCommand(HWND hwndDlg, WORD command) {
    switch (command) {
    case IDC_BUTTON_BACKGROUND:
    {
        ChangeColor(hwndDlg, settingsNew.backgroundColor);
        //diff.backgroundColor = true;
        break;
    }
    case IDC_BUTTON_FONT:
    {
        ChangeColor(hwndDlg, settingsNew.fontColor);
        //diff.fontColor = true;
        break;
    }
    case IDC_CHECK1:
    {
        HWND checkCtrl = ::GetDlgItem(hwndDlg, IDC_CHECK1);
        isPreview = Button_GetCheck(checkCtrl) == BST_CHECKED;
        if( !isPreview ) {
            ApplySettings(settingsDefault);
        }
        break;
    }
    case IDC_OK:
    {
        settingsDefault = settingsNew;
        ApplySettings(settingsNew);
        ::EndDialog(hwndDlg, 0);
        return true;
    }
    case IDC_CANCEL:
    {
        settingsNew = settingsDefault;
        ApplySettings(settingsDefault);
        ::EndDialog(hwndDlg, 0);
        return true;
    }
    }
    MakePreview();
    return false;
}

INT_PTR CDialogWindow::dialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    if( uMsg == WM_INITDIALOG ) {
        CDialogWindow* window = reinterpret_cast<CDialogWindow*>(lParam);
        ::SetWindowLongPtr(hwndDlg, GWLP_USERDATA, (LONG)window);
        window->OnInit(hwndDlg);
        return true;
    }
    CDialogWindow* window = reinterpret_cast<CDialogWindow*>(GetWindowLongPtr(hwndDlg, GWLP_USERDATA));
    switch (uMsg) {
    case WM_COMMAND:
    {
        return window->OnCommand(hwndDlg, LOWORD(wParam));
    }
    case WM_HSCROLL:
    {
        window->OnScroll(hwndDlg, (HWND)lParam);
        return true;
    }
    case WM_CLOSE:
    {
        ::EndDialog(hwndDlg, 0);
        return true;
    }
    }
    return false;
}
