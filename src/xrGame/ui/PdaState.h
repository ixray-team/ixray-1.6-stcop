#pragma once

#include "UIPdaAux.h"

extern u32 g_pda_info_state;

class PdaState final
{
public:
    static void Clear(const pda_section::part section)
    {
        g_pda_info_state &= ~section;
    }

    static void MarkUpdated(const pda_section::part section)
    {
        g_pda_info_state |= section;
    }

    static bool HasUpdates(const pda_section::part section)
    {
        return (g_pda_info_state & section) != 0;
    }
};
