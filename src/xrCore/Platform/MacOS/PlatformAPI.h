#pragma once

#include "OSFile.h"
#include "OSMath.h"
#include "UTF8.h"
#include "OSThread.h"
#include "OSStrings.h"
#include "OSDebug.h"
#include "OSMemory.h"
#include "ComPtr.h"

#define ENTRY_ARGS int argc, char *argv[]
#define ENTRY_ARGS_PUSH argc, argv
#define ENTRY_FUNCTION main