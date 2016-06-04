#include <set>
#include <sstream>
#include <string>
#include <Windows.h>

using Dictionary = std::set<std::wstring>;

void readDictionary(Dictionary& dict)
{
	dict.insert(L"please");
	dict.insert(L"thanks");
	dict.insert(L"hello");
	dict.insert(L"sorry");
	dict.insert(L"bye");
}

void filterTextByDictionary(LPWSTR buffer, Dictionary& dict)
{
	int charPos;
	std::wstring word;
	std::wstring text;
	for( charPos = 0; buffer[charPos]; charPos++ ) {
		if( iswalpha(buffer[charPos]) || iswdigit(buffer[charPos]) ) {
			word += buffer[charPos];
		} else {
			if( word.size() > 0 ) {
				if( dict.count(word) > 0 ) {
					text += word;
				}
				word.clear();
			}
			text += buffer[charPos];
		}
	}
	if( word.size() > 0 && dict.count(word) > 0 ) {
		text += word;
	}
	for( int i = 0; i < charPos * sizeof(wchar_t); ++i ) {
		buffer[i] = 0;
	}
	memcpy(buffer, text.c_str(), (text.size() + 1) * sizeof(wchar_t));
}

void proceedText(Dictionary& dict)
{
	std::wstringstream wss;
	wss << L"Local\\Text" << GetCurrentProcessId();
	HANDLE file = OpenFileMapping(FILE_MAP_ALL_ACCESS, false, wss.str().c_str());
	wchar_t* buffer = static_cast<wchar_t*>(MapViewOfFile(file, FILE_MAP_ALL_ACCESS, 0, 0, 0));
	filterTextByDictionary(buffer, dict);
	CloseHandle(file);
}

int WINAPI wWinMain(HINSTANCE instance, HINSTANCE prevInstance, LPWSTR commandLine, int cmdShow)
{
	/*std::wstringstream wss;
	wss << L"Global\\NewText" << GetCurrentProcessId();*/
	wchar_t eventName[40];
	swprintf_s(eventName, L"Global\\StartWorking%d", GetCurrentProcessId());
	HANDLE eventStartWorking = CreateEvent(0, FALSE, FALSE, eventName);
	swprintf_s(eventName, L"Global\\KillProcesses");
	HANDLE eventClose = CreateEvent(0, TRUE, FALSE, eventName);
	HANDLE eventsInitialized = OpenEvent(EVENT_ALL_ACCESS, false, L"Global\\EventsInit");
	SetEvent(eventsInitialized);
	Dictionary dictionary;
	readDictionary(dictionary);
	bool terminated = false;
	HANDLE events[] = { eventStartWorking, eventClose };
	HANDLE textProceededEvent;
	while( !terminated ) {
		switch( WaitForMultipleObjects(2, events, false, INFINITE) ) {
		case WAIT_OBJECT_0:
			proceedText(dictionary);
			swprintf_s(eventName, L"Global\\TextProceeded%d", GetCurrentProcessId());
			textProceededEvent = OpenEvent(EVENT_ALL_ACCESS, false, eventName);
			SetEvent(textProceededEvent);
			CloseHandle(textProceededEvent);
			break;
		case WAIT_OBJECT_0 + 1:
			terminated = true;
			break;
		}
	}
	CloseHandle(eventStartWorking);
	CloseHandle(eventClose);
}