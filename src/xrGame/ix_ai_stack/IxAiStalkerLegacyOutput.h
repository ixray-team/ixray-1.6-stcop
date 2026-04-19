#pragma once

class CAI_Stalker;
class IxAiAgent;

// Maps IX agent state into legacy stalker danger / visibility when bridgeEnabled.
void IxAiStalkerLegacyOutput_Apply(CAI_Stalker& stalker, IxAiAgent& agent);
