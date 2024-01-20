////////////////////////////////////////////////////////////////////////////
//	Module 		: os_clipboard.cpp
//	Created 	: 21.02.2008
//	Author		: Evgeniy Sokolov
//	Description : os clipboard class implementation
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#pragma hdrstop
#include "os_clipboard.h"

void os_clipboard::copy_to_clipboard(LPCSTR buf)
{
    SDL_SetClipboardText(buf);
}

void os_clipboard::paste_from_clipboard	( LPSTR buffer, u32 const& buffer_size )
{
    if (!SDL_HasClipboardText())
        return;

    char* clipData = SDL_GetClipboardText();
    strncpy_s(buffer, buffer_size, clipData, buffer_size - 1);
}

void os_clipboard::update_clipboard(LPCSTR string)
{
    if (!SDL_HasClipboardText())
    {
        copy_to_clipboard(string);
        return;
    }

    char* clipData = SDL_GetClipboardText();

    if (!clipData)
    {
        VERIFY3(clipData, "Failed to get text from the clipboard", SDL_GetError());
        copy_to_clipboard(string);
        return;
    }

    xr_string NewText = clipData;
    NewText += string;

    copy_to_clipboard(NewText.c_str());
}