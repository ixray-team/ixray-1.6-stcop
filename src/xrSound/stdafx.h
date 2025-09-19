#ifndef stdafxH
#define stdafxH
#pragma once

#include "../xrCore/xrCore.h"
#ifdef XRSOUND_EXPORTS

#ifdef IXR_WINDOWS
// mmsystem.h
#define MMNOSOUND
#define MMNOMIDI
#define MMNOAUX
#define MMNOMIXER
#define MMNOJOY
#include <mmsystem.h>

// mmreg.h
#define NOMMIDS
#define NONEWRIFF
#define NOJPEGDIB
#define NONEWIC
#define NOBITMAP
#include <mmreg.h>
#endif

#include <vorbis/codec.h>
#ifndef IXR_WINDOWS
#   define NULL 0
#endif
#include <vorbis/vorbisfile.h>

#include "../xrCore/Collision/xrCDB.h"
#include "Sound.h"

#define ENGINE_API
#endif

#include "../xrCore/xr_resource.h"

#endif
