#include "stdafx.h"
#include "AsureXML.h"
#include <magic_enum/magic_enum.hpp>

IC void CXMLOverride::IterateElement(tinyxml2::XMLElement* Start, std::function<void(tinyxml2::XMLElement*, EOverrideMode)> Callback)
{
	tinyxml2::XMLElement* ModifElement = Start;
	while (ModifElement != nullptr)
	{
		tinyxml2::XMLElement* TestChild = ModifElement->FirstChildElement();
		IterateElement(TestChild, Callback);

		if (ModifElement->Attribute("override"))
		{
			xr_string Copy = ModifElement->Attribute("override");
			std::transform(Copy.begin(), Copy.end(), Copy.begin(), [](unsigned char c) { return std::tolower(c); });

			EOverrideMode Mode = magic_enum::enum_cast<EOverrideMode>(Copy).value_or(EOverrideMode::none);
			Callback(ModifElement, Mode);
		}

		ModifElement = ModifElement->NextSiblingElement();
	}
}

void CXMLOverride::ApplyNewNode(tinyxml2::XMLNode* Parent, tinyxml2::XMLElement* Inner)
{
	if (Parent->ToElement() == nullptr)
	{
		R_ASSERT2(Parent->ToElement(), "TinyXML does not support using XmlNode as a container!");
		return;
	}

	tinyxml2::XMLElement* NewNode = Parent->ToElement()->InsertNewChildElement(Inner->Value());

	// Reg attribs
	tinyxml2::XMLAttribute* Attribs = const_cast<tinyxml2::XMLAttribute*>(Inner->FirstAttribute());
	while (Attribs != nullptr)
	{
		NewNode->SetAttribute(Attribs->Name(), Attribs->Value());
		Attribs = const_cast<tinyxml2::XMLAttribute*>(Attribs->Next());
	}

	// Setup text block
	if (Inner->GetText() != nullptr)
	{
		NewNode->SetText(Inner->GetText());
	}

	// Parse childs
	tinyxml2::XMLElement* ChildIter = Inner->FirstChildElement();

	while (ChildIter != nullptr)
	{
		ApplyNewNode(NewNode, ChildIter);
		ChildIter = ChildIter->NextSiblingElement();
	}
}

void CXMLOverride::GenerateNewDoc(tinyxml2::XMLDocument& Original, tinyxml2::XMLDocument& Modif)
{
	tinyxml2::XMLElement* ModifElement = Modif.FirstChildElement();
	xr_vector<tinyxml2::XMLElement*> ParentList;

	IterateElement
	(
		ModifElement,
		[&](tinyxml2::XMLElement* ChildElement, EOverrideMode OverrideMode)
		{
			if (OverrideMode == EOverrideMode::none)
				return;

			ParentList.clear();

			tinyxml2::XMLNode* Parent = ChildElement->Parent();
			while (Parent != nullptr)
			{
				if (Parent->ToElement() != nullptr)
				{
					// Root element is null
					ParentList.insert(ParentList.begin(), Parent->ToElement());
				}
				Parent = Parent->Parent();
			}
			ParentList.push_back(ChildElement);

			tinyxml2::XMLElement* IterateElement = Original.FirstChildElement(ParentList[0]->Value());
			ParentList.erase(ParentList.begin());

			tinyxml2::XMLNode* MyParent = nullptr;

			for (auto Element : ParentList)
			{
				xr_string ElValue = Element->Value();
				if (ElValue == "string")
				{
					const char* IDAttrib = Element->Attribute("id");

					IterateElement = IterateElement->FirstChildElement();
					while (IterateElement != nullptr)
					{
						xr_string CheckID = IterateElement->Attribute("id");
						if (CheckID == IDAttrib)
							break;

						IterateElement = IterateElement->NextSiblingElement();
					}
				}
				else
				{
					tinyxml2::XMLElement* TestChild = IterateElement->FirstChildElement(Element->Value());
					if (TestChild != nullptr)
					{
						IterateElement = TestChild;
					}
				}
			}

			MyParent = IterateElement->Parent();

			if (OverrideMode == EOverrideMode::remove || OverrideMode == EOverrideMode::replace)
			{
				size_t NodeCount = MyParent->ChildElementCount();
				MyParent->DeleteChild(IterateElement);

				if (OverrideMode == EOverrideMode::replace)
				{
					ApplyNewNode(MyParent, ChildElement);
					VERIFY(NodeCount == MyParent->ChildElementCount());
				}
			}
			else if (OverrideMode == EOverrideMode::add)
			{
				ApplyNewNode(MyParent, ChildElement);
			}
		}
	);
}

FS_FileSet CXMLOverride::GetModifFiles(const char* Path, const char* File)
{
	std::filesystem::path OrigXML = File;
	xr_string ValidFileName = OrigXML.filename().generic_string().c_str();
	ValidFileName = ValidFileName.substr(0, ValidFileName.length() - OrigXML.extension().generic_string().length());

	xr_string AddPath = OrigXML.parent_path().generic_string().c_str();
	std::replace(AddPath.begin(), AddPath.end(), '/', '\\');

	xr_string ModifPathMask = xr_string("mod_") + ValidFileName + "_*" + OrigXML.extension().generic_string().c_str();
	ModifPathMask = AddPath + "\\" + ModifPathMask;

	FS_FileSet ModifyList;
	FS.file_list(ModifyList, "$game_config$", FS_ListFiles, ModifPathMask.c_str());

	return std::move(ModifyList);
}
