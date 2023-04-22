#pragma once

#include "../xrCore/intrusive_ptr.h"

class CPhraseDialog;

typedef intrusive_ptr<CPhraseDialog>	DIALOG_SHARED_PTR;

#include "PhraseDialog.h"

using DIALOG_ID_VECTOR = xr_vector<shared_str>;
using DIALOG_ID_IT = DIALOG_ID_VECTOR::iterator;
