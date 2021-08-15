////////////////////////////////////////////////////////////////////////////
//	Module 		: object_type_traits.h
//	Created 	: 21.01.2003
//  Modified 	: 12.07.2004
//	Author		: Dmitriy Iassenev
//	Description : Object type traits
////////////////////////////////////////////////////////////////////////////

#pragma once

	#define declare_has(a) \
		template <typename T>\
		struct has_##a {\
			template <typename P> static detail::yes	select(detail::other<typename P::a>*);\
			template <typename P> static detail::no	select(...);\
			enum { value = sizeof(detail::yes) == sizeof(select<T>(0)) };\
		};

	namespace object_type_traits {
		namespace detail {
			struct yes {char a[1];};
			struct no  {char a[2];};
			template <typename T> struct other{};
		};

		template <template <typename _1> class T1, typename T2>
		struct is_base_and_derived_or_same_from_template {
			template <typename P>
			static detail::yes	select(T1<P>*);
			static detail::no	select(...);

			enum { value = sizeof(detail::yes) == sizeof(select((T2*)0))};
		};

		declare_has(iterator);
		declare_has(const_iterator);
		declare_has(value_type);
		declare_has(size_type);

		template <typename T>
		struct is_stl_container {
			enum { 
				value = 
					has_iterator<T>::value &&
					has_const_iterator<T>::value &&
					has_size_type<T>::value &&
					has_value_type<T>::value
			};
		};
	};
