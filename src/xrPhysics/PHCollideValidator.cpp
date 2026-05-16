#include "StdAfx.h"
#include "PHObject.h"
#include "PHCollideValidator.h"

CGID CPHCollideValidator::freeGroupID=0;
_flags<CLClassBits> CPHCollideValidator::ClassFlags={CLClassBits(0)};	
_flags<CLClassBits> CPHCollideValidator::ClassNCFlags={CLClassBits(0)};
_flags<CLClassBits> CPHCollideValidator::NonTypeFlags={CLClassBits(0)};	
void CPHCollideValidator::Init()
{
	freeGroupID=0;
	NonTypeFlags.set(cbNCGroupObject,true);

	ClassFlags.set(cbClassDynamic|cbClassCharacter|cbClassSmall|cbClassRagDoll|cbClassAnimated,true);
	ClassNCFlags.set(cbNCClassCharacter|cbNCClassSmall|cbNCClassDynamic|cbNCClassRagDoll|cbNCClassAnimated,true);

}
CGID CPHCollideValidator::RegisterGroup()
{
	++freeGroupID;
	return freeGroupID-1;
}

void CPHCollideValidator::InitObject(CPHObject& obj)
{
	obj.collide_class_bits().assign(0);
	obj.collide_class_bits().set(cbClassDynamic,true);
	obj.collide_bits()=0;
}
void CPHCollideValidator::RegisterObjToGroup(CGID group,CPHObject& obj)
{
	R_ASSERT(group<freeGroupID);
	obj.collide_bits()=group;
	obj.collide_class_bits().set(cbNCGroupObject,true);
}
bool CPHCollideValidator::IsGroupObject(const CPHObject& obj)
{
	return !!obj.collide_class_bits().test(cbNCGroupObject);
}


bool CPHCollideValidator::IsAnimatedObject(const CPHObject& obj)
{
	return !!obj.collide_class_bits().test(cbClassAnimated);
}


void CPHCollideValidator::RegisterObjToLastGroup(CPHObject& obj)
{
	RegisterObjToGroup(LastGroupRegistred(),obj);
}

CGID CPHCollideValidator::LastGroupRegistred()
{
	return freeGroupID-1;
}

void CPHCollideValidator::RestoreGroupObject(const CPHObject& obj)
{
}

void CPHCollideValidator::SetStaticNotCollide(CPHObject& obj)
{
	obj.collide_class_bits().set(cbNCStatic,true);
}
void CPHCollideValidator::SetDynamicNotCollide(CPHObject& obj)
{
	obj.collide_class_bits().set(cbNCClassDynamic,true);
}

void CPHCollideValidator::SetNonDynamicObject(CPHObject& obj)
{
	obj.collide_class_bits().set(cbClassDynamic,false);
}

void	CPHCollideValidator::SetCharacterClass			(CPHObject& obj)
{
	obj.collide_class_bits().set(cbClassCharacter,true);
}

void	CPHCollideValidator::SetCharacterClassNotCollide	(CPHObject& obj)
{
	obj.collide_class_bits().set(cbNCClassCharacter,true);
}

void	CPHCollideValidator::SetRagDollClass				(CPHObject& obj)
{
	obj.collide_class_bits().set(cbClassRagDoll,true);
}

void	CPHCollideValidator::SetRagDollClassNotCollide		(CPHObject& obj)
{
	obj.collide_class_bits().set(cbNCClassRagDoll,true);
}

	//Относит физический объект к классу анимированных объектов
	void	CPHCollideValidator::SetAnimatedClass				(CPHObject& obj)
	{
		obj.collide_class_bits().set(cbClassAnimated,true);
	}

	//Задаёт игнорирование коллизий данного физического
	//объекта с анимированными телами
	void	CPHCollideValidator::SetAnimatedClassNotCollide		(CPHObject& obj)
	{
		obj.collide_class_bits().set(cbNCClassAnimated,true);
	}

void	CPHCollideValidator::		SetClassSmall				(CPHObject& obj)
{
	obj.collide_class_bits().set(cbClassSmall,true);
}
void	CPHCollideValidator::		SetClassSmallNotCollide		(CPHObject& obj)
{
	obj.collide_class_bits().set(cbNCClassSmall,true);
}

CGID			RegisterGroup				()
{
	return CPHCollideValidator::RegisterGroup();
}