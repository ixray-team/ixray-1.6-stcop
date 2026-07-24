#pragma once

enum : u32
{
	REG_PRIORITY_LOW = 0x11111111ul,
	REG_PRIORITY_NORMAL = 0x22222222ul,
	REG_PRIORITY_HIGH = 0x33333333ul,
	REG_PRIORITY_CAPTURE = 0x7ffffffful,
	REG_PRIORITY_INVALID = 0xfffffffful
};

enum PureRenderPriority : u32
{
	UI_LOAD_SCREEN = 0,
	UI_CONSOLE = 1,
	UI_CURSOR = 2,
	UI_TUTORIAL = 3,
	UI_MAIN_MENU = 4,
};

#define CREATE_PURE_DEFINITION(name) \
	class ENGINE_API pure##name		 \
	{								 \
	public:							 \
		virtual void On##name() = 0; \
	}								 \

CREATE_PURE_DEFINITION(Frame);						// OnFrame
CREATE_PURE_DEFINITION(Render);						// OnRender
CREATE_PURE_DEFINITION(AppActivate);				// OnAppActivate
CREATE_PURE_DEFINITION(AppDeactivate);				// OnAppDeactivate
CREATE_PURE_DEFINITION(AppStart);					// OnAppStart
CREATE_PURE_DEFINITION(AppEnd);						// OnAppEnd
CREATE_PURE_DEFINITION(DeviceReset);				// OnDeviceReset
CREATE_PURE_DEFINITION(ScreenResolutionChanged);	// OnScreenResolutionChanged
CREATE_PURE_DEFINITION(DrawUI);						// OnDrawUI

template<typename T>
struct PureReg
{
	T* object;
    int	priority;
};

template<class T>
class CRegistrator
{
	bool changed;
	bool in_process;
	
	void cleanup_invalidated_objects()
	{
		auto begin = std::remove_if(pure_objects.begin(), pure_objects.end(),
									[&](const PureReg<T>& reg_entry)
									{
										return reg_entry.priority == REG_PRIORITY_INVALID;
									});
		
		pure_objects.erase(begin, pure_objects.end());
	}
	
public:
	xr_vector<PureReg<T>> pure_objects;

	CRegistrator()
	{
		in_process = false;
		changed = false;
		pure_objects.reserve(1024);
	}
	
	template<typename U> requires std::derived_from<U, T>
	void Add(U* pure_object, int priority = REG_PRIORITY_NORMAL)
	{
		if (pure_object == nullptr)
		{
			return;
		}

		PureReg<T> reg_entry { static_cast<T*>(pure_object), priority };
		pure_objects.push_back(reg_entry);

		if (in_process)
		{
			changed = true;
		}
		else
		{
			Resort();
		}
	}

	template<typename U> requires std::derived_from<U, T>
	void Remove(U* to_remove)
	{
		if (to_remove == nullptr)
		{
			return;
		}

		T* const target = static_cast<T*>(to_remove);
		for (PureReg<T>& reg_entry : pure_objects)
		{
			if (reg_entry.object == target)
			{
				reg_entry.priority = REG_PRIORITY_INVALID;
			}
		}

		if (in_process)
		{
			changed = true;
		}
		else
		{
			Resort();
		}
	}
	
	template<void (T::*pureMethod)()>
	void Process()
	{
		if (pure_objects.empty())
			return;
		
		in_process = true;
		
		for (PureReg<T>& reg_entry : pure_objects)
		{
			if (reg_entry.priority == REG_PRIORITY_INVALID)
				continue;
			
			if (reg_entry.object == nullptr)
				continue;
			
			(reg_entry.object->*pureMethod)();
		}

		if (changed)
			Resort();
		
		in_process = false;
	}
	
	void Resort()
	{
		std::sort(pure_objects.begin(), pure_objects.end(),
		          [](const PureReg<T>& a, const PureReg<T>& b)
		          {
			          return a.priority > b.priority;
		          });
		
		cleanup_invalidated_objects();
		
		changed = false;
	}
};