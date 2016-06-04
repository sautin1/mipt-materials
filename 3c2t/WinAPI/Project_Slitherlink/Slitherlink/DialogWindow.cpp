// Author: Andrew Sautin

#include "DialogWindow.h"
#include "OverlappedWindow.h"

const int CDialogWindow::fontMin = 10;
const int CDialogWindow::fontMax = 95;
const int CDialogWindow::opacityMin = 0;
const int CDialogWindow::opacityMax = 255;

CDialogWindow::CDialogWindow()
{
}

CDialogWindow::~CDialogWindow()
{
}

void CDialogWindow::Create(HWND parentHwnd, CGameDesign* _drawer)
{
	parentHandle = parentHwnd;
	drawer = _drawer;
	settingsDefault.backgroundColor = drawer->GetColor("colorBackground");
	settingsDefault.fontColorUnsat = drawer->GetColor("colorNumbersUnsatisfied");
	settingsDefault.fontColorSat = drawer->GetColor("colorNumbersSatisfied");
	settingsDefault.lineColorGrid = drawer->GetColor("colorBorderInactive");
	settingsDefault.lineColorCorr = drawer->GetColor("colorBorderActive");
	settingsDefault.lineColorWrong = drawer->GetColor("colorBorderWrong");
	settingsDefault.opacity = drawer->GetSize("opacity");
	settingsDefault.font.lfHeight = drawer->GetSize("fontSizePercent");
	settingsNew = settingsDefault;
	HINSTANCE hInstance = (HINSTANCE)::GetWindowLong(parentHwnd, GWL_HINSTANCE);
	CreateDialogParam(GetModuleHandle(0), MAKEINTRESOURCE(IDD_DIALOG1), parentHwnd, reinterpret_cast<DLGPROC>(CDialogWindow::dialogProc), reinterpret_cast<LPARAM>(this));
	ShowWindow(handle, SW_SHOW);
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

void CDialogWindow::ApplySettings(CSettings settings)
{
	COverlappedWindow* parentWindow = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(parentHandle, GWLP_USERDATA));
	drawer->SetFontSize(settings.font.lfHeight);
	drawer->SetOpacity(parentWindow->Handle(), settings.opacity);
	drawer->SetColor("Background", settings.backgroundColor, false, true);
	drawer->SetColor("NumbersUnsatisfied", settings.fontColorUnsat, false, false);
	drawer->SetColor("NumbersSatisfied", settings.fontColorSat, false, false);
	drawer->SetColor("BorderInactive", settings.lineColorGrid, true, false);
	drawer->SetColor("BorderActive", settings.lineColorCorr, true, false);
	drawer->SetColor("BorderWrong", settings.lineColorWrong, true, false);
	
	parentWindow->Repaint();
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
	InitScrolls(hwndDlg);
}

void CDialogWindow::OnScroll(HWND hwndDlg, HWND scrollCtrl) {
	int scrollId = ::GetDlgCtrlID(scrollCtrl);
	if( scrollId == IDC_SLIDER_FONT ) {
		settingsNew.font.lfHeight = SendDlgItemMessage(hwndDlg, IDC_SLIDER_FONT, TBM_GETPOS, 0, 0);
	} else {
		settingsNew.opacity = SendDlgItemMessage(hwndDlg, IDC_SLIDER_OPACITY, TBM_GETPOS, 0, 0);
	}
	MakePreview();
}

bool CDialogWindow::OnCommand(HWND hwndDlg, WORD command) {
	switch (command) {
	case IDC_BUTTON_BACKGROUND:
	{
		ChangeColor(hwndDlg, settingsNew.backgroundColor);
		break;
	}
	case IDC_BUTTON_FONT_UNSAT:
	{
		ChangeColor(hwndDlg, settingsNew.fontColorUnsat);
		break;
	}
	case IDC_BUTTON_FONT_SAT:
	{
		ChangeColor(hwndDlg, settingsNew.fontColorSat);
		break;
	}
	case IDC_BUTTON_LINE_CORR:
	{
		ChangeColor(hwndDlg, settingsNew.lineColorCorr);
		break;
	}
	case IDC_BUTTON_LINE_WRNG:
	{
		ChangeColor(hwndDlg, settingsNew.lineColorWrong);
		break;
	}
	case IDC_BUTTON_LINE_GRID:
	{
		ChangeColor(hwndDlg, settingsNew.lineColorGrid);
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
		::DestroyWindow(hwndDlg);
		return true;
	}
	}
	return false;
}
