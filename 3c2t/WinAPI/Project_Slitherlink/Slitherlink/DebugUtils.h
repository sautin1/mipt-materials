#pragma once
#include <sstream>
#include <Windows.h>

template<typename T>
void printDebugMsg(const T& msg) {
	std::wstringstream sstream;
	sstream << msg;
	::MessageBox(NULL, sstream.str().c_str(), L"Debug", NULL);
}