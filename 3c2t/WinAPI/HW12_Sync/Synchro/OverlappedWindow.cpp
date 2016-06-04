// Author: Andrew Sautin

#include "OverlappedWindow.h"

const LPCWSTR COverlappedWindow::className = L"OverlappedWindow";
const LPCWSTR COverlappedWindow::windowName = L"MyNotepad";
const int COverlappedWindow::maxFilenameSize = 1024;
const size_t COverlappedWindow::windowWidth = 500;
const size_t COverlappedWindow::windowHeight = 300;
const size_t COverlappedWindow::fontHeightDefault = 20;

COverlappedWindow::COverlappedWindow()
	: slaveIds(slaveCount, 0)
{
	handle = 0;
	isChanged = false;
	usedBrush = 0;
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
	windowClass.hIcon = (HICON)::LoadImage(windowClass.hInstance, MAKEINTRESOURCE(IDI_ICON1), IMAGE_ICON, 32, 32, 0);
	windowClass.hIconSm = (HICON)::LoadImage(windowClass.hInstance, MAKEINTRESOURCE(IDI_ICON1), IMAGE_ICON, 16, 16, 0);
	windowClass.lpszMenuName = MAKEINTRESOURCE(IDR_MENU1);
	windowClass.lpszClassName = className;
	windowClass.hbrBackground = 0;
	return (::RegisterClassEx(&windowClass) != 0 );
}

bool COverlappedWindow::Create()
{
	CreateWindowEx(WS_EX_LAYERED, className, windowName, WS_OVERLAPPEDWINDOW | WS_SIZEBOX,
		CW_USEDEFAULT, CW_USEDEFAULT, windowWidth, windowHeight, 0, 0, GetModuleHandle(0), this);
	if( handle != 0 ) {
		SetLayeredWindowAttributes(handle, 0, 255, LWA_ALPHA);

		WCHAR windowTitle[maxFilenameSize];
		HINSTANCE hInstance = GetModuleHandle(0);
		::LoadString(hInstance, IDS_TITLE, windowTitle, maxFilenameSize);
		SetWindowText(handle, windowTitle);

		HRSRC resourceSource = ::FindResource(hInstance, MAKEINTRESOURCE(IDR_PLACEHOLDER_TEXT1), L"PLACEHOLDER_TEXT");
		HGLOBAL resourceHandle = ::LoadResource(hInstance, resourceSource);
		LPCWSTR resourceData = (LPCWSTR)::LockResource(resourceHandle);
		DWORD resourceDataSize = ::SizeofResource(hInstance, resourceSource);

		SetWindowText(editWindow.Handle(), resourceData);
	}
	return (handle != 0);
}

void COverlappedWindow::Show(int cmdShow)
{
	ShowWindow(handle, cmdShow);
}

HWND COverlappedWindow::Handle()
{
	return handle;
}

CEditControl COverlappedWindow::EditWindow()
{
	return editWindow;
}

CDialogWindow COverlappedWindow::DialogWindow()
{
	return dialogWindow;
}

void COverlappedWindow::OnDestroy()
{
	HANDLE termEvent = OpenEvent(EVENT_ALL_ACCESS, FALSE, L"Global\\KillProcesses");
	SetEvent(termEvent);
	CloseHandle(termEvent);
	DeleteObject(usedBrush);
	editWindow.Destroy();
	PostQuitMessage(0);
}

void COverlappedWindow::OnNCCreate(HWND _handle) {
	handle = _handle;
}

void COverlappedWindow::OnCreate()
{
	editWindow.Create(handle);
	setDefaultFont();
	createSlaves();
}

void COverlappedWindow::createSlaves()
{
	for( int i = 0; i < slaveCount; i++ ) {
		STARTUPINFO startUpInfo;
		PROCESS_INFORMATION processInfo;
		startUpInfo.cb = sizeof(startUpInfo);
		ZeroMemory(&startUpInfo, sizeof(startUpInfo));
		ZeroMemory(&processInfo, sizeof(processInfo));
		HANDLE creation = CreateEvent(0, false, false, L"Global\\EventsInit");
		CreateProcess(L"D:\\Files\\3c2t_WinAPI\\HW12\\Synchro\\Debug\\DictCleaner.exe",
			0, 0, 0, FALSE, 0, 0, 0, &startUpInfo, &processInfo);
		WaitForSingleObject(creation, static_cast<DWORD>(INFINITY));
		slaveIds[i] = processInfo.dwProcessId;
	}
}

void COverlappedWindow::OnResize()
{
	RECT childRect;
	::GetClientRect(handle, &childRect);
	int width = childRect.right - childRect.left;
	int height = childRect.bottom - childRect.top;
	SetWindowPos(editWindow.Handle(), 0, childRect.left, childRect.top, width, height, 0);
}

void COverlappedWindow::OnOpen()
{
	WCHAR filename[maxFilenameSize];
	OPENFILENAME openInfo;
	memset(filename, 0, maxFilenameSize * sizeof(WCHAR));
	memset(&openInfo, 0, sizeof(openInfo));
	openInfo.lStructSize = sizeof(OPENFILENAME);
	openInfo.hwndOwner = handle;
	openInfo.lpstrFilter = L"All Files\0*.*\0\0";
	openInfo.lpstrTitle = L"Open";
	openInfo.lpstrInitialDir = L"C:\\";
	openInfo.lpstrFile = (LPWSTR)filename;
	openInfo.nMaxFile = maxFilenameSize;
	openInfo.Flags = OFN_PATHMUSTEXIST | OFN_EXPLORER;
	if( ::GetOpenFileName(&openInfo) ) {
		HANDLE inFile = ::CreateFile(openInfo.lpstrFile, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
		DWORD bufferSize = ::GetFileSize(inFile, NULL);
		LPWSTR buffer = new WCHAR[bufferSize + 1];
		memset(buffer, 0, (bufferSize + 1) * sizeof(WCHAR));
		DWORD readCount = 0;
		::ReadFile(inFile, buffer, bufferSize, &readCount, NULL);
		SendMessage(editWindow.Handle(), WM_SETTEXT, NULL, LPARAM(buffer));
		::CloseHandle(inFile);
		delete[] buffer;
	}
}

BOOL COverlappedWindow::OnSave()
{
	DWORD bufferSize = SendMessage(editWindow.Handle(), WM_GETTEXTLENGTH, 0, 0);
	BOOL isSaved = false;
	if( bufferSize > 0 ) {
		LPWSTR buffer = new WCHAR[bufferSize + 1];
		memset(buffer, 0, (bufferSize + 1) * sizeof(WCHAR));
		WCHAR filename[maxFilenameSize];
		OPENFILENAME saveInfo;
		memset(filename, 0, maxFilenameSize * sizeof(WCHAR));
		memset(&saveInfo, 0, sizeof(saveInfo));

		SendMessage(editWindow.Handle(), WM_GETTEXT, (WPARAM)(bufferSize + 1), LPARAM(buffer));
		saveInfo.lStructSize = sizeof(OPENFILENAME);
		saveInfo.hwndOwner = handle;
		saveInfo.lpstrFilter = L"All Files\0*.*\0\0";
		saveInfo.lpstrTitle = L"Save As";
		saveInfo.lpstrInitialDir = L"C:\\";
		saveInfo.lpstrFile = (LPWSTR)filename;
		saveInfo.nMaxFile = maxFilenameSize;
		saveInfo.Flags = OFN_PATHMUSTEXIST | OFN_EXPLORER;
		if( GetSaveFileName(&saveInfo) ) {
			HANDLE outFile = CreateFile(saveInfo.lpstrFile, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
			DWORD writtenCount = 0;
			WORD bom = 0xFEFF;
			WriteFile(outFile, &bom, 2, &writtenCount, NULL);
			isSaved = WriteFile(outFile, buffer, bufferSize * sizeof(WCHAR), &writtenCount, NULL);
			CloseHandle(outFile);
			if( !isSaved ) {
				MessageBox(handle, L"File cannot be saved", L"IO Error", MB_OK);
			} else {
				MessageBox(handle, L"File is saved!", L"Success", MB_OK);
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
		int answer = MessageBox(handle, L"Do you want to save changes?", L"Save on exit", MB_YESNOCANCEL);
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

void COverlappedWindow::OnChange()
{
	isChanged = true;
}

LRESULT COverlappedWindow::OnColorEdit(HDC hdc)
{
	::SetTextColor(hdc, dialogWindow.SettingsNew().fontColor);
	COLORREF backgroundColor = dialogWindow.SettingsNew().backgroundColor;
	::SetBkColor(hdc, backgroundColor);
	if( usedBrush ) {
		DeleteObject(usedBrush);
	}
	usedBrush = ::CreateSolidBrush(backgroundColor);
	return (LRESULT)usedBrush;
}

void COverlappedWindow::filterText()
{
	int length = ::SendMessage(editWindow.Handle(), WM_GETTEXTLENGTH, 0, 0);
	wchar_t* text = new wchar_t[length + 1];
	::SendMessage(editWindow.Handle(), WM_GETTEXT, length + 1, reinterpret_cast<LPARAM>(text));
	int textSize = wcslen(text);
	
	wchar_t* borders[slaveCount + 1];
	for( int i = 0; i < slaveCount; i++ ) {
		borders[i] = text + textSize / slaveCount * i;
	}
	borders[slaveCount] = text + textSize;
	for( int i = 1; i < slaveCount; i++ ) {
		while( iswalpha(*borders[i]) || iswdigit(*borders[i]) ) {
			borders[i]++;
		}
	}
	HANDLE mappingFiles[slaveCount];
	wchar_t* buffers[slaveCount];
	for( int i = 0; i < slaveCount; i++ ) {
		wchar_t fileName[40];
		swprintf_s(fileName, L"Local\\Text%d", slaveIds[i]);
		int partLen = borders[i + 1] - borders[i] + 1;
		HANDLE file = CreateFileMapping(INVALID_HANDLE_VALUE, 0, PAGE_READWRITE, 0, partLen * sizeof(wchar_t), fileName);
		wchar_t* buffer = static_cast<wchar_t*>(MapViewOfFile(file, FILE_MAP_ALL_ACCESS, 0, 0, 0));
		memcpy(buffer, borders[i], (partLen - 1) * sizeof(wchar_t));
		buffer[partLen - 1] = '\0';
		buffers[i] = buffer;
		mappingFiles[i] = file;
	}
	HANDLE waitEvents[slaveCount];
	for( int i = 0; i < slaveCount; i++ ) {
		wchar_t eventName[40];
		swprintf_s(eventName, L"Global\\TextProceeded%d", slaveIds[i]);
		waitEvents[i] = CreateEvent(0, false, false, eventName);
	}
	for( int i = 0; i < slaveCount; i++ ) {
		wchar_t eventName[40];
		swprintf_s(eventName, L"Global\\StartWorking%d", slaveIds[i]);
		HANDLE newTextEvent = OpenEvent(EVENT_ALL_ACCESS, true, eventName);
		SetEvent(newTextEvent);
		CloseHandle(newTextEvent);
	}
	WaitForMultipleObjects(slaveCount, waitEvents, true, INFINITE);
	wchar_t* current = text;
	for( int i = 0; i < slaveCount; i++ ) {
		int blockLen = wcslen(buffers[i]);
		memcpy(current, buffers[i], blockLen * sizeof(wchar_t));
		current += blockLen;
		CloseHandle(mappingFiles[i]);
		CloseHandle(waitEvents[i]);
	}
	*current = '\0';
	SendMessage(editWindow.Handle(), WM_SETTEXT, 0, reinterpret_cast<LPARAM>(text));
	delete[] text;
}

void COverlappedWindow::setDefaultFont()
{
	HFONT sysFont = (HFONT)SendMessage(editWindow.Handle(), WM_GETFONT, 0, 0);
	if (!sysFont) {
		sysFont = (HFONT)::GetStockObject(DEFAULT_GUI_FONT);
	}
	LOGFONT logFont;
	::GetObject(sysFont, sizeof(LOGFONT), &logFont);
	logFont.lfHeight = fontHeightDefault;
	SendMessage(editWindow.Handle(), WM_SETFONT, reinterpret_cast<WPARAM>(CreateFontIndirect(&logFont)), true);
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
		} else if( LOWORD(wParam) == ID_FILE_OPEN ) {
			window->OnOpen();
			return 0;
		} else if( LOWORD(wParam) == ID_FILE_SAVE ) {
			window->OnSave();
			return 0;
		} else if( LOWORD(wParam) == ID_FILE_EXIT ) {
			window->OnClose();
			return 0;
		} else if( LOWORD(wParam) == ID_ACCELERATOR_QUIT ) {
			DestroyWindow(handle);
			return 0;
		} else if( LOWORD(wParam) == ID_VIEW_SETTINGS ) {
			window->dialogWindow.Create(window->Handle());
			return 0;
		} else if( LOWORD(wParam) == ID_VIEW_FILTERWORDS ) {
			window->filterText();
			return 0;
		}
	}
	case WM_CTLCOLOREDIT:
	{
		COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
		return window->OnColorEdit(reinterpret_cast<HDC>(wParam));
	}
	default:
		return DefWindowProc(handle, message, wParam, lParam);
	}
}
