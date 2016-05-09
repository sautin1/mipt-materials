#include <cwctype>
#include <string>

#define NOTEPADUTILSAPI extern "C" __declspec(dllexport)

#include "NotepadUtils.h"

int WordsCount(const wchar_t* text)
{
    const std::wstring inStringChars = L"\'-";
    std::wstring str(text);
    bool wasAlpha = false;
    int count = 0;
    for( auto it = str.begin(); it != str.end(); ++it ) {
        bool isAlpha = std::iswalpha(*it) > 0;
        bool isInStringChar = inStringChars.find(*it) != std::wstring::npos;
        if( isAlpha && !wasAlpha ) {
            ++count;
            wasAlpha = true;
        } else if( !isAlpha && (!wasAlpha || !isInStringChar) ) {
            wasAlpha = false;
        }
    }
    return count;
}
