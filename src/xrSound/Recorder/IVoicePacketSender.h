#pragma once
#include "VoicePacket.h"

class IVoicePacketSender
{
public:
	virtual ~IVoicePacketSender() = default;
	virtual void Send(VoicePacket** packets, u8 count) = 0;
};