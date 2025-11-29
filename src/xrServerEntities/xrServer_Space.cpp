#include "stdafx.h"
#include "xrServer_Space.h"

ISaveObject& operator<<(ISaveObject& Object, SRotation& Value) {
	return Object << Value.yaw << Value.pitch << Value.roll;
}