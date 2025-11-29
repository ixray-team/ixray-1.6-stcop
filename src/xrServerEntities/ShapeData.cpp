#pragma once
#include "StdAfx.h"
#include "ShapeData.h"
#include "../xrCore/Save/SaveObject.h"

ISaveObject& operator<<(ISaveObject& Object, CShapeData::shape_def& Value) {
	BEGIN_CHUNK(Object, "CShapeData::shape_def")
	{
		Object << Value.type;
		switch (Value.type) {
		case CShapeData::cfSphere: {
				Object << Value.data.sphere;
				break;
		}
		case CShapeData::cfBox: {
				Object << Value.data.box;
				break;
		}
		}
	}
	return Object;
}