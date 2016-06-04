// Author: Andrew Sautin

#include "OverlappedWindow.h"

// system things
const LPCWSTR COverlappedWindow::className = L"OverlappedWindow";
const LPCWSTR COverlappedWindow::windowName = L"Slitherlink Game";
const int COverlappedWindow::maxBufferSize = 1024;

int COverlappedWindow::windowWidth = 500;
int COverlappedWindow::windowHeight = 500;

COverlappedWindow::COverlappedWindow()
{
	handle = 0;
	isChanged = false;
	game = CSlitherlinkGame::StartNewGame(L"data/00.dat");
	drawer.ChangeGameSizes(game.GetRowCount(), game.GetColCount());
	Repaint();
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
	windowClass.hCursor = LoadCursor(NULL, IDC_ARROW);
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

		WCHAR windowTitle[maxBufferSize];
		HINSTANCE hInstance = GetModuleHandle(0);
		::LoadString(hInstance, IDS_TITLE, windowTitle, maxBufferSize);
		SetWindowText(handle, windowTitle);
	}
	return (handle != 0);
}

void COverlappedWindow::Show(int cmdShow)
{
	ShowWindow(handle, cmdShow);
}

void COverlappedWindow::Repaint()
{
	RECT rect;
	::GetClientRect(handle, &rect);
	::InvalidateRect(handle, &rect, FALSE);
}

HWND COverlappedWindow::Handle()
{
	return handle;
}

CDialogWindow COverlappedWindow::DialogWindow()
{
	return dialogWindow;
}

void COverlappedWindow::OnDestroy()
{
	PostQuitMessage(0);
}

void COverlappedWindow::OnNCCreate(HWND _handle) {
	handle = _handle;
}

void COverlappedWindow::OnCreate()
{
}

void COverlappedWindow::OnResize()
{
	RECT rect;
	::GetClientRect(handle, &rect);
	windowWidth = rect.right - rect.left;
	windowHeight = rect.bottom - rect.top;
	drawer.ResizeWindow(windowWidth, windowHeight);
	::InvalidateRect(handle, &rect, FALSE);
}

void COverlappedWindow::OnClick(int clickX, int clickY)
{
	POINT click;
	click.x = clickX;
	click.y = clickY;
	Edge edge = drawer.NearestEdge(click);
	bool isSolved = game.ToggleEdgeStatus(edge);
	isChanged = true;
	Repaint();

	if( isSolved ) {
		MessageBox(handle, L"Puzzle solved!", L"Congratulations!", NULL);
	}
}

void COverlappedWindow::OnPaint()
{
	RECT clientRect;
	PAINTSTRUCT paintStruct;
	HDC windowDC = ::BeginPaint(handle, &paintStruct);
	::GetClientRect(handle, &clientRect);

	HDC displayBufferDC = CreateCompatibleDC(windowDC);
	HBITMAP displayBuffer = CreateCompatibleBitmap(
		windowDC,
		clientRect.right - clientRect.left,
		clientRect.bottom - clientRect.top
		);
	HGDIOBJ oldDisplayBuffer = SelectObject(displayBufferDC, displayBuffer);
	::SetBkMode(displayBufferDC, TRANSPARENT);
	::FillRect(displayBufferDC, &clientRect, static_cast<HBRUSH>(drawer.GetObject("brushBackground")));
	drawer.DrawWindowContents(displayBufferDC, game);
	BitBlt(windowDC, clientRect.left, clientRect.top, clientRect.right - clientRect.left, clientRect.bottom - clientRect.top, displayBufferDC, 0, 0, SRCCOPY);

	SelectObject(displayBufferDC, oldDisplayBuffer);
	DeleteObject(displayBuffer);
	DeleteDC(displayBufferDC);
	::EndPaint(handle, &paintStruct);
}

bool COverlappedWindow::OnSave()
{
	bool isSaved = false;
	WCHAR filename[maxBufferSize];
	OPENFILENAME saveInfo;
	memset(filename, 0, maxBufferSize * sizeof(WCHAR));
	memset(&saveInfo, 0, sizeof(saveInfo));

	saveInfo.lStructSize = sizeof(OPENFILENAME);
	saveInfo.hwndOwner = handle;
	saveInfo.lpstrFilter = L"All Files\0*.*\0\0";
	saveInfo.lpstrTitle = L"Save As";
	saveInfo.lpstrInitialDir = L"C:\\";
	saveInfo.lpstrFile = (LPWSTR)filename;
	saveInfo.nMaxFile = maxBufferSize;
	saveInfo.Flags = OFN_PATHMUSTEXIST | OFN_EXPLORER;
	if( ::GetSaveFileName(&saveInfo) ) {
		isSaved = game.SaveToFile(std::wstring(saveInfo.lpstrFile));
		if( !isSaved ) {
			::MessageBox(handle, L"File cannot be saved", L"IO Error", MB_OK);
		} else {
			::MessageBox(handle, L"File is saved!", L"Success", MB_OK);
		}
	}
	isChanged = !isSaved;
	return isSaved;
}

void COverlappedWindow::OnReset()
{
	BOOL canReset = !isChanged;
	if( isChanged ) {
		int answer = MessageBox(handle, L"Do you really want to reset the puzzle?", L"Reset", MB_YESNO);
		if( answer == IDYES ) {
			game.EraseBorders();
			Repaint();
			isChanged = false;
		}
	}
}

void COverlappedWindow::OnLoad()
{
	BOOL canExit = !isChanged;
	if( isChanged ) {
		int answer = MessageBox(handle, L"Do you want to save changes?", L"Save before new game", MB_YESNOCANCEL);
		if( answer == IDYES ) {
			canExit = OnSave();
		} else if( answer == IDNO ) {
			canExit = true;
		}
	}
	if( !canExit ) {
		return;
	}
	bool isOpened = false;
	WCHAR filename[maxBufferSize];
	OPENFILENAME openInfo;
	memset(filename, 0, maxBufferSize * sizeof(WCHAR));
	memset(&openInfo, 0, sizeof(openInfo));

	openInfo.lStructSize = sizeof(OPENFILENAME);
	openInfo.hwndOwner = handle;
	openInfo.lpstrFilter = L".dat files\0*.dat\0\0";
	openInfo.lpstrTitle = L"Open";
	openInfo.lpstrInitialDir = L"C:\\";
	openInfo.lpstrFile = (LPWSTR)filename;
	openInfo.nMaxFile = maxBufferSize;
	openInfo.Flags = OFN_PATHMUSTEXIST | OFN_EXPLORER;
	if( ::GetOpenFileName(&openInfo) ) {
		game = CSlitherlinkGame::StartNewGame(std::wstring(openInfo.lpstrFile));
	}
	drawer.ChangeGameSizes(game.GetRowCount(), game.GetColCount());
	Repaint();
	isChanged = false;
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
		if( LOWORD(wParam) == ID_GAME_RESET ) {
			window->OnReset();
			return 0;
		} else if( LOWORD(wParam) == ID_GAME_LOAD ) {
			window->OnLoad();
			return 0;
		} else if( LOWORD(wParam) == ID_GAME_SAVE ) {
			window->OnSave();
			return 0;
		} else if( LOWORD(wParam) == ID_GAME_EXIT ) {
			window->OnClose();
			return 0;
		} else if( LOWORD(wParam) == ID_ACCELERATOR_QUIT ) {
			DestroyWindow(handle);
			return 0;
		} else if( LOWORD(wParam) == ID_VIEW_SETTINGS ) {
			window->dialogWindow.Create(window->Handle(), &(window->drawer));
			return 0;
		}
	}
	case WM_PAINT:
	{
		COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
		window->OnPaint();
		return 0;
	}
	case WM_LBUTTONDOWN:
	{
		COverlappedWindow* window = reinterpret_cast<COverlappedWindow*>(GetWindowLongPtr(handle, GWLP_USERDATA));
		window->OnClick(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam));
		return 0;
	}
	default:
		return DefWindowProc(handle, message, wParam, lParam);
	}
}
