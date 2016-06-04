// Author: Andrew Sautin

#pragma once
#include <Windows.h>
#include <Windowsx.h>
#include <Commctrl.h>
#include "resource.h"
#include "GameDesign.h"

class COverlappedWindow;

class CDialogWindow {
public:
	CDialogWindow();
	~CDialogWindow();

	struct CSettings {
		LOGFONT font;
		int opacity;
		COLORREF fontColorSat;
		COLORREF fontColorUnsat;
		COLORREF lineColorGrid;
		COLORREF lineColorCorr;
		COLORREF lineColorWrong;
		COLORREF backgroundColor;
		CSettings() = default;
	};

	void Create(HWND parentHwnd, CGameDesign* _drawer);
	CSettings SettingsNew() const;
	HWND Handle() const;

protected:
	void InitScrolls(HWND hwndDlg);
	void ApplyFontSize(COverlappedWindow* parentWindow, CSettings settings);
	void ApplyOpacity(COverlappedWindow* parentWindow, CSettings settings);
	void ApplySettings(CSettings settings);
	void MakePreview();
	void ChangeColor(HWND dialogHandle, COLORREF& color);
	void OnInit(HWND hwndDlg);
	void OnScroll(HWND hwndDlg, HWND scrollCtrl);
	bool OnCommand(HWND hwndDlg, WORD command);
private:
	struct CDiff {
		bool fontSize;
		bool opacity;
		bool fontColor;
		bool backgroundColor;
	};

	static const int fontMin;
	static const int fontMax;
	static const int opacityMin;
	static const int opacityMax;
	HWND parentHandle;
	CSettings settingsDefault;
	CSettings settingsNew;
	CDiff diff;
	HFONT usedFont;
	HWND handle;
	bool isPreview;
	CGameDesign* drawer;

	static INT_PTR CALLBACK dialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
};