#pragma once

#include "UTF8.h"
#include "OSMath.h"
#include "OSFile.h"
#include "OSThread.h"
#include "OSMemory.h"

#include "MutexHandle.h"

#define ENTRY_ARGS HINSTANCE hInstance, HINSTANCE hPrevInstance, char* lpCmdLine, int nCmdShow
#define ENTRY_ARGS_PUSH hInstance, hPrevInstance, lpCmdLine, nCmdShow
#define ENTRY_FUNCTION APIENTRY WinMain