#pragma once

#ifdef DEBUG_DRAW
#   define ECS_COMPONENT(Type) \
		friend class CECSComponentStorage<Type>; \
		using ECS_TYPEID = Type; \
		static const char* ECS_Name() { return #Type; } \
		void ECS_DebugDraw() const { \
			ImGui::Separator(); \
			ImGui::Text("Component: %s", #Type);

#   define ECS_VALUE(var, name) \
			ImGui::Text("%s: %.3f", name, static_cast<float>(var));

#   define ECS_STRING(var, name) \
			ImGui::Text("%s: %s", name, var ? var : "(null)");

#   define ECS_PTR(var, name) \
			ImGui::Text("%s: %p", name, var);

#   define ECS_END \
		} \
		struct ECS_RegisterDraw { \
			ECS_RegisterDraw() { \
				if (GECSManager) { \
					GECSManager->RegisterDrawFunc<ECS_TYPEID>([](CECSComponentStorage<ECS_TYPEID>* Storage){ \
						const auto& Comps = Storage->Data(); \
						const auto& Owners = Storage->Entities(); \
						for (size_t i=0;i<Comps.size();++i) { \
							ImGui::PushID(i); \
							ImGui::Text("Owner: %p", Owners[i]); \
							Comps[i].ECS_DebugDraw(); \
							ImGui::PopID(); \
						} \
					}); \
				} \
			} \
		} ecsRegisterDrawInstance;

#else
#   define ECS_COMPONENT(Type) friend class CECSComponentStorage<Type>; static const char* ECS_Name() { return #Type; } void ECS_DebugDraw() const { 
#   define ECS_VALUE(var, name)
#   define ECS_STRING(var, name)
#   define ECS_PTR(var, name)
#   define ECS_END }
#endif