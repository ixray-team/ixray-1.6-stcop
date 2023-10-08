#include "stdafx.h"

#define DECL_ENUM_ELEMENT( element ) #element
#define BEGIN_ENUM( ENUM_NAME ) char* gs_##ENUM_NAME [] =
#define END_ENUM( ENUM_NAME ) ; char* getString##ENUM_NAME(\
            ENUM_NAME index){ return gs_##ENUM_NAME [(size_t)index]; }

BEGIN_ENUM(EEngineExternalUI) {
    DECL_ENUM_ELEMENT(HQIcons),
} END_ENUM(EEngineExternalUI)

BEGIN_ENUM(EEngineExternalPhysical) {
    DECL_ENUM_ELEMENT(None),
} END_ENUM(EEngineExternalPhysical)

BEGIN_ENUM(EEngineExternalGame) {
    DECL_ENUM_ELEMENT(None),
} END_ENUM(EEngineExternalGame)

BEGIN_ENUM(EEngineExternalRender) {
    DECL_ENUM_ELEMENT(None),
} END_ENUM(EEngineExternalRender)
