/* ============================================================================================ */
/* FMOD Ex - Main C/C++ Network event system header file.                                       */
/* Copyright (c), Firelight Technologies Pty, Ltd. 2004-2008.                                   */
/*                                                                                              */
/* Use this header to enable network tweaking and auditioning in realtime from FMOD Designer.   */
/* ============================================================================================ */

#ifndef __FMOD_EVENT_NET_H__
#define __FMOD_EVENT_NET_H__

#ifndef _FMOD_HPP
#include "fmod.hpp"
#endif

/*
    FMOD NetEventSystem version number.  Check this against NetEventSystem_GetVersion.
    0xaaaabbcc -> aaaa = major version number.  bb = minor version number.  cc = development version number.
*/
#define FMOD_EVENT_NET_VERSION 0x00041708

/*
    Default port that the target (game) will listen on
*/
#define FMOD_EVENT_NET_PORT    17997

namespace FMOD
{
    class EventSystem;

    FMOD_RESULT F_API NetEventSystem_Init(EventSystem *eventsystem, unsigned short port = FMOD_EVENT_NET_PORT);
    FMOD_RESULT F_API NetEventSystem_Update();
    FMOD_RESULT F_API NetEventSystem_Shutdown();
    FMOD_RESULT F_API NetEventSystem_GetVersion(unsigned int *version);
}

#endif
