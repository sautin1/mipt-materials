#ifndef NOTEPADUTILSAPI
#define NOTEPADUTILSAPI extern "C" __declspec(dllimport)
#else
#define NOTEPADUTILSAPI extern "C" __declspec(dllexport)
#endif

NOTEPADUTILSAPI int WordsCount(const wchar_t* text);
