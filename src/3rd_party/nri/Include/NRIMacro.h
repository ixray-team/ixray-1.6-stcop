#pragma once

#if defined(__cplusplus)
    #if !defined(NRI_FORCE_C)
        #define NRI_CPP
    #endif
#else
    #include <stdbool.h>
#endif

#define NRI_EXPAND(args) args

#if defined(_MSC_VER)
    #define _NRI_NARGS(_1, _2, _3, _4, _5, _6_, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, _65, _66, _67, _68, _69, _70, _71, _72, _73, VAL, ...) VAL
    #define _NRI_NARGS1(...) NRI_EXPAND(_NRI_NARGS(__VA_ARGS__, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0))
    #define _NRI_AUGMENTER(...) unused, __VA_ARGS__
    #define NRI_NARGS(...) _NRI_NARGS1(_NRI_AUGMENTER(__VA_ARGS__))
#else
    #define _NRI_NARGS(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, _65, _66, _67, _68, _69, _70, _71, _72, _73, VAL,...) VAL
    #define NRI_NARGS(...) _NRI_NARGS(0, ## __VA_ARGS__, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#endif

#define _NRI_MERGE_TOKEN2(_0, _1) _0##_1
#define NRI_MERGE_TOKEN2(_0, _1) _NRI_MERGE_TOKEN2(_0, _1)

#define _NRI_MERGE_TOKEN3(_0, _1, _2) _0##_1##_2
#define NRI_MERGE_TOKEN3(_0, _1, _2) _NRI_MERGE_TOKEN3(_0, _1, _2)

#define _NRI_SEQ0(code, ...)
#define _NRI_SEQ1(code, ...) NRI_EXPAND(code(0, __VA_ARGS__))
#define _NRI_SEQ2(code, ...) _NRI_SEQ1(code, __VA_ARGS__), NRI_EXPAND(code(1, __VA_ARGS__))
#define _NRI_SEQ3(code, ...) _NRI_SEQ2(code, __VA_ARGS__), NRI_EXPAND(code(2, __VA_ARGS__))
#define _NRI_SEQ4(code, ...) _NRI_SEQ3(code, __VA_ARGS__), NRI_EXPAND(code(3, __VA_ARGS__))
#define _NRI_SEQ5(code, ...) _NRI_SEQ4(code, __VA_ARGS__), NRI_EXPAND(code(4, __VA_ARGS__))
#define _NRI_SEQ6(code, ...) _NRI_SEQ5(code, __VA_ARGS__), NRI_EXPAND(code(5, __VA_ARGS__))
#define _NRI_SEQ7(code, ...) _NRI_SEQ6(code, __VA_ARGS__), NRI_EXPAND(code(6, __VA_ARGS__))
#define _NRI_SEQ8(code, ...) _NRI_SEQ7(code, __VA_ARGS__), NRI_EXPAND(code(7, __VA_ARGS__))
#define _NRI_SEQ9(code, ...) _NRI_SEQ8(code, __VA_ARGS__), NRI_EXPAND(code(8, __VA_ARGS__))
#define _NRI_SEQ10(code, ...) _NRI_SEQ9(code, __VA_ARGS__), NRI_EXPAND(code(9, __VA_ARGS__))
#define _NRI_SEQ11(code, ...) _NRI_SEQ10(code, __VA_ARGS__), NRI_EXPAND(code(10, __VA_ARGS__))
#define _NRI_SEQ12(code, ...) _NRI_SEQ11(code, __VA_ARGS__), NRI_EXPAND(code(11, __VA_ARGS__))
#define _NRI_SEQ13(code, ...) _NRI_SEQ12(code, __VA_ARGS__), NRI_EXPAND(code(12, __VA_ARGS__))
#define _NRI_SEQ14(code, ...) _NRI_SEQ13(code, __VA_ARGS__), NRI_EXPAND(code(13, __VA_ARGS__))
#define _NRI_SEQ15(code, ...) _NRI_SEQ14(code, __VA_ARGS__), NRI_EXPAND(code(14, __VA_ARGS__))
#define _NRI_SEQ16(code, ...) _NRI_SEQ15(code, __VA_ARGS__), NRI_EXPAND(code(15, __VA_ARGS__))
#define _NRI_SEQ17(code, ...) _NRI_SEQ16(code, __VA_ARGS__), NRI_EXPAND(code(16, __VA_ARGS__))
#define _NRI_SEQ18(code, ...) _NRI_SEQ17(code, __VA_ARGS__), NRI_EXPAND(code(17, __VA_ARGS__))
#define _NRI_SEQ19(code, ...) _NRI_SEQ18(code, __VA_ARGS__), NRI_EXPAND(code(18, __VA_ARGS__))
#define _NRI_SEQ20(code, ...) _NRI_SEQ19(code, __VA_ARGS__), NRI_EXPAND(code(19, __VA_ARGS__))
#define _NRI_SEQ21(code, ...) _NRI_SEQ20(code, __VA_ARGS__), NRI_EXPAND(code(20, __VA_ARGS__))
#define _NRI_SEQ22(code, ...) _NRI_SEQ21(code, __VA_ARGS__), NRI_EXPAND(code(21, __VA_ARGS__))
#define _NRI_SEQ23(code, ...) _NRI_SEQ22(code, __VA_ARGS__), NRI_EXPAND(code(22, __VA_ARGS__))
#define _NRI_SEQ24(code, ...) _NRI_SEQ23(code, __VA_ARGS__), NRI_EXPAND(code(23, __VA_ARGS__))
#define _NRI_SEQ25(code, ...) _NRI_SEQ24(code, __VA_ARGS__), NRI_EXPAND(code(24, __VA_ARGS__))
#define _NRI_SEQ26(code, ...) _NRI_SEQ25(code, __VA_ARGS__), NRI_EXPAND(code(25, __VA_ARGS__))
#define _NRI_SEQ27(code, ...) _NRI_SEQ26(code, __VA_ARGS__), NRI_EXPAND(code(26, __VA_ARGS__))
#define _NRI_SEQ28(code, ...) _NRI_SEQ27(code, __VA_ARGS__), NRI_EXPAND(code(27, __VA_ARGS__))
#define _NRI_SEQ29(code, ...) _NRI_SEQ28(code, __VA_ARGS__), NRI_EXPAND(code(28, __VA_ARGS__))
#define _NRI_SEQ30(code, ...) _NRI_SEQ29(code, __VA_ARGS__), NRI_EXPAND(code(29, __VA_ARGS__))
#define _NRI_SEQ31(code, ...) _NRI_SEQ30(code, __VA_ARGS__), NRI_EXPAND(code(30, __VA_ARGS__))
#define _NRI_SEQ32(code, ...) _NRI_SEQ31(code, __VA_ARGS__), NRI_EXPAND(code(31, __VA_ARGS__))
#define _NRI_SEQ33(code, ...) _NRI_SEQ32(code, __VA_ARGS__), NRI_EXPAND(code(32, __VA_ARGS__))
#define _NRI_SEQ34(code, ...) _NRI_SEQ33(code, __VA_ARGS__), NRI_EXPAND(code(33, __VA_ARGS__))
#define _NRI_SEQ35(code, ...) _NRI_SEQ34(code, __VA_ARGS__), NRI_EXPAND(code(34, __VA_ARGS__))
#define _NRI_SEQ36(code, ...) _NRI_SEQ35(code, __VA_ARGS__), NRI_EXPAND(code(35, __VA_ARGS__))
#define _NRI_SEQ37(code, ...) _NRI_SEQ36(code, __VA_ARGS__), NRI_EXPAND(code(36, __VA_ARGS__))
#define _NRI_SEQ38(code, ...) _NRI_SEQ37(code, __VA_ARGS__), NRI_EXPAND(code(37, __VA_ARGS__))
#define _NRI_SEQ39(code, ...) _NRI_SEQ38(code, __VA_ARGS__), NRI_EXPAND(code(38, __VA_ARGS__))
#define _NRI_SEQ40(code, ...) _NRI_SEQ39(code, __VA_ARGS__), NRI_EXPAND(code(39, __VA_ARGS__))
#define _NRI_SEQ41(code, ...) _NRI_SEQ40(code, __VA_ARGS__), NRI_EXPAND(code(40, __VA_ARGS__))
#define _NRI_SEQ42(code, ...) _NRI_SEQ41(code, __VA_ARGS__), NRI_EXPAND(code(41, __VA_ARGS__))
#define _NRI_SEQ43(code, ...) _NRI_SEQ42(code, __VA_ARGS__), NRI_EXPAND(code(42, __VA_ARGS__))
#define _NRI_SEQ44(code, ...) _NRI_SEQ43(code, __VA_ARGS__), NRI_EXPAND(code(43, __VA_ARGS__))
#define _NRI_SEQ45(code, ...) _NRI_SEQ44(code, __VA_ARGS__), NRI_EXPAND(code(44, __VA_ARGS__))
#define _NRI_SEQ46(code, ...) _NRI_SEQ45(code, __VA_ARGS__), NRI_EXPAND(code(45, __VA_ARGS__))
#define _NRI_SEQ47(code, ...) _NRI_SEQ46(code, __VA_ARGS__), NRI_EXPAND(code(46, __VA_ARGS__))
#define _NRI_SEQ48(code, ...) _NRI_SEQ47(code, __VA_ARGS__), NRI_EXPAND(code(47, __VA_ARGS__))
#define _NRI_SEQ49(code, ...) _NRI_SEQ48(code, __VA_ARGS__), NRI_EXPAND(code(48, __VA_ARGS__))
#define _NRI_SEQ50(code, ...) _NRI_SEQ49(code, __VA_ARGS__), NRI_EXPAND(code(49, __VA_ARGS__))
#define _NRI_SEQ51(code, ...) _NRI_SEQ50(code, __VA_ARGS__), NRI_EXPAND(code(50, __VA_ARGS__))
#define _NRI_SEQ52(code, ...) _NRI_SEQ51(code, __VA_ARGS__), NRI_EXPAND(code(51, __VA_ARGS__))
#define _NRI_SEQ53(code, ...) _NRI_SEQ52(code, __VA_ARGS__), NRI_EXPAND(code(52, __VA_ARGS__))
#define _NRI_SEQ54(code, ...) _NRI_SEQ53(code, __VA_ARGS__), NRI_EXPAND(code(53, __VA_ARGS__))
#define _NRI_SEQ55(code, ...) _NRI_SEQ54(code, __VA_ARGS__), NRI_EXPAND(code(54, __VA_ARGS__))
#define _NRI_SEQ56(code, ...) _NRI_SEQ55(code, __VA_ARGS__), NRI_EXPAND(code(55, __VA_ARGS__))
#define _NRI_SEQ57(code, ...) _NRI_SEQ56(code, __VA_ARGS__), NRI_EXPAND(code(56, __VA_ARGS__))
#define _NRI_SEQ58(code, ...) _NRI_SEQ57(code, __VA_ARGS__), NRI_EXPAND(code(57, __VA_ARGS__))
#define _NRI_SEQ59(code, ...) _NRI_SEQ58(code, __VA_ARGS__), NRI_EXPAND(code(58, __VA_ARGS__))
#define _NRI_SEQ60(code, ...) _NRI_SEQ59(code, __VA_ARGS__), NRI_EXPAND(code(59, __VA_ARGS__))
#define _NRI_SEQ61(code, ...) _NRI_SEQ60(code, __VA_ARGS__), NRI_EXPAND(code(60, __VA_ARGS__))
#define _NRI_SEQ62(code, ...) _NRI_SEQ61(code, __VA_ARGS__), NRI_EXPAND(code(61, __VA_ARGS__))
#define _NRI_SEQ63(code, ...) _NRI_SEQ62(code, __VA_ARGS__), NRI_EXPAND(code(62, __VA_ARGS__))
#define _NRI_SEQ64(code, ...) _NRI_SEQ63(code, __VA_ARGS__), NRI_EXPAND(code(63, __VA_ARGS__))
#define _NRI_SEQ65(code, ...) _NRI_SEQ64(code, __VA_ARGS__), NRI_EXPAND(code(64, __VA_ARGS__))
#define _NRI_SEQ66(code, ...) _NRI_SEQ65(code, __VA_ARGS__), NRI_EXPAND(code(65, __VA_ARGS__))
#define _NRI_SEQ67(code, ...) _NRI_SEQ66(code, __VA_ARGS__), NRI_EXPAND(code(66, __VA_ARGS__))
#define _NRI_SEQ68(code, ...) _NRI_SEQ67(code, __VA_ARGS__), NRI_EXPAND(code(67, __VA_ARGS__))
#define _NRI_SEQ69(code, ...) _NRI_SEQ68(code, __VA_ARGS__), NRI_EXPAND(code(68, __VA_ARGS__))
#define _NRI_SEQ70(code, ...) _NRI_SEQ69(code, __VA_ARGS__), NRI_EXPAND(code(69, __VA_ARGS__))
#define _NRI_SEQ71(code, ...) _NRI_SEQ70(code, __VA_ARGS__), NRI_EXPAND(code(70, __VA_ARGS__))
#define _NRI_SEQ72(code, ...) _NRI_SEQ71(code, __VA_ARGS__), NRI_EXPAND(code(71, __VA_ARGS__))
#define NRI_SEQN(code, count, ...) NRI_MERGE_TOKEN2(_NRI_SEQ, count)(code, __VA_ARGS__)

#define _NRI_VA_ARGS_AT0(_0, ...) _0
#define _NRI_VA_ARGS_AT1(_0, _1, ...) _1
#define _NRI_VA_ARGS_AT2(_0, _1, _2, ...) _2
#define _NRI_VA_ARGS_AT3(_0, _1, _2, _3, ...) _3
#define _NRI_VA_ARGS_AT4(_0, _1, _2, _3, _4, ...) _4
#define _NRI_VA_ARGS_AT5(_0, _1, _2, _3, _4, _5, ...) _5
#define _NRI_VA_ARGS_AT6(_0, _1, _2, _3, _4, _5, _6, ...) _6
#define _NRI_VA_ARGS_AT7(_0, _1, _2, _3, _4, _5, _6, _7, ...) _7
#define _NRI_VA_ARGS_AT8(_0, _1, _2, _3, _4, _5, _6, _7, _8, ...) _8
#define _NRI_VA_ARGS_AT9(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, ...) _9
#define _NRI_VA_ARGS_AT10(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, ...) _10
#define _NRI_VA_ARGS_AT11(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, ...) _11
#define _NRI_VA_ARGS_AT12(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, ...) _12
#define _NRI_VA_ARGS_AT13(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, ...) _13
#define _NRI_VA_ARGS_AT14(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, ...) _14
#define _NRI_VA_ARGS_AT15(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, ...) _15
#define _NRI_VA_ARGS_AT16(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, ...) _16
#define _NRI_VA_ARGS_AT17(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, ...) _17
#define _NRI_VA_ARGS_AT18(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, ...) _18
#define _NRI_VA_ARGS_AT19(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, ...) _19
#define _NRI_VA_ARGS_AT20(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, ...) _20
#define _NRI_VA_ARGS_AT21(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, ...) _21
#define _NRI_VA_ARGS_AT22(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, ...) _22
#define _NRI_VA_ARGS_AT23(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, ...) _23
#define _NRI_VA_ARGS_AT24(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, ...) _24
#define _NRI_VA_ARGS_AT25(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, ...) _25
#define _NRI_VA_ARGS_AT26(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, ...) _26
#define _NRI_VA_ARGS_AT27(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, ...) _27
#define _NRI_VA_ARGS_AT28(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, ...) _28
#define _NRI_VA_ARGS_AT29(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, ...) _29
#define _NRI_VA_ARGS_AT30(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, ...) _30
#define _NRI_VA_ARGS_AT31(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, ...) _31
#define _NRI_VA_ARGS_AT32(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, ...) _32
#define _NRI_VA_ARGS_AT33(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, ...) _33
#define _NRI_VA_ARGS_AT34(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, ...) _34
#define _NRI_VA_ARGS_AT35(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, ...) _35
#define _NRI_VA_ARGS_AT36(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, ...) _36
#define _NRI_VA_ARGS_AT37(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, ...) _37
#define _NRI_VA_ARGS_AT38(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, ...) _38
#define _NRI_VA_ARGS_AT39(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, ...) _39
#define _NRI_VA_ARGS_AT40(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, ...) _40
#define _NRI_VA_ARGS_AT41(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, ...) _41
#define _NRI_VA_ARGS_AT42(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, ...) _42
#define _NRI_VA_ARGS_AT43(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, ...) _43
#define _NRI_VA_ARGS_AT44(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, ...) _44
#define _NRI_VA_ARGS_AT45(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, ...) _45
#define _NRI_VA_ARGS_AT46(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, ...) _46
#define _NRI_VA_ARGS_AT47(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, ...) _47
#define _NRI_VA_ARGS_AT48(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, ...) _48
#define _NRI_VA_ARGS_AT49(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, ...) _49
#define _NRI_VA_ARGS_AT50(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, ...) _50
#define _NRI_VA_ARGS_AT51(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, ...) _51
#define _NRI_VA_ARGS_AT52(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, ...) _52
#define _NRI_VA_ARGS_AT53(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, ...) _53
#define _NRI_VA_ARGS_AT54(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, ...) _54
#define _NRI_VA_ARGS_AT55(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, ...) _55
#define _NRI_VA_ARGS_AT56(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, ...) _56
#define _NRI_VA_ARGS_AT57(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, ...) _57
#define _NRI_VA_ARGS_AT58(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, ...) _58
#define _NRI_VA_ARGS_AT59(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, ...) _59
#define _NRI_VA_ARGS_AT60(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, ...) _60
#define _NRI_VA_ARGS_AT61(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, ...) _61
#define _NRI_VA_ARGS_AT62(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, ...) _62
#define _NRI_VA_ARGS_AT63(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, ...) _63
#define _NRI_VA_ARGS_AT64(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, ...) _64
#define _NRI_VA_ARGS_AT65(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, _65, ...) _65
#define _NRI_VA_ARGS_AT66(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, _65, _66, ...) _66
#define _NRI_VA_ARGS_AT67(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, _65, _66, _67, ...) _67
#define _NRI_VA_ARGS_AT68(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, _65, _66, _67, _68, ...) _68
#define _NRI_VA_ARGS_AT69(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, _65, _66, _67, _68, _69, ...) _69
#define _NRI_VA_ARGS_AT70(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, _65, _66, _67, _68, _69, _70, ...) _70
#define _NRI_VA_ARGS_AT71(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, _65, _66, _67, _68, _69, _70, _71, ...) _71
#define _NRI_VA_ARGS_AT72(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, _65, _66, _67, _68, _69, _70, _71, _72, ...) _72
#define NRI_VA_ARGS_AT(index, ...) NRI_EXPAND(NRI_MERGE_TOKEN2(_NRI_VA_ARGS_AT, index)(__VA_ARGS__))

#if defined(NRI_CPP)
    #define _NRI_ENUM_ENTRY(index, ...) NRI_VA_ARGS_AT(index, __VA_ARGS__)
    #define _NRI_EXPAND_ENUM(...) NRI_SEQN(_NRI_ENUM_ENTRY, NRI_NARGS(__VA_ARGS__), __VA_ARGS__)

    #define NRI_NAMESPACE_BEGIN namespace nri {
    #define NRI_NAMESPACE_END }
    #define NRI_NAME(name) name
    #define NRI_STRUCT(name) struct name
    #define NRI_UNION(name) union name
    #define NRI_FORWARD_STRUCT(name) struct name
    #define NRI_REF(arg) arg&
    #define NRI_REF_ACCESS(arg) (&arg)
    #define NRI_ENUM_MEMBER(name, nameCaps, member) name::member
    #define NRI_DEFAULT_VALUE(arg) = arg
    #define NRI_ZERO_INIT {}

    #define NRI_ENUM_BITS(name, nameCaps, type, ...) \
        enum class name : type; \
        constexpr type  operator &  (name val0, name val1)  { return (type)val0 & (type)val1; } \
        constexpr name  operator |  (name val0, name val1)  { return (name)((type)val0 | (type)val1); } \
        constexpr name& operator &= (name& val0, name val1) { val0 = (name)(val0 & val1); return val0; } \
        constexpr name& operator |= (name& val0, name val1) { val0 = (name)(val0 | val1); return val0; } \
        NRI_ENUM(name, nameCaps, type, __VA_ARGS__)

    #define NRI_ENUM(name, nameCaps, type, ...) \
        enum class name : type \
        { \
            _NRI_EXPAND_ENUM(__VA_ARGS__) \
        }
#else
    #define _NRI_ENUM_PREFIX(name) NRI_MERGE_TOKEN3(NRI_, name, _)
    #define _NRI_ENUM_NAME(name) nri_##name##_
    #define _NRI_ENUM_ENTRY(index, prefix, ...) NRI_MERGE_TOKEN2(prefix, NRI_VA_ARGS_AT(index, __VA_ARGS__))
    #define _NRI_EXPAND_ENUM(prefix, ...) NRI_SEQN(_NRI_ENUM_ENTRY, NRI_NARGS(__VA_ARGS__), prefix, __VA_ARGS__)

    #define NRI_NAMESPACE_BEGIN
    #define NRI_NAMESPACE_END
    #define NRI_NAME(name) nri_##name
    #define NRI_STRUCT(name) typedef struct NRI_NAME(name) NRI_NAME(name); struct NRI_NAME(name)
    #define NRI_UNION(name) typedef union NRI_NAME(name) NRI_NAME(name); union NRI_NAME(name)
    #define NRI_FORWARD_STRUCT(name) typedef struct NRI_NAME(name) NRI_NAME(name)
    #define NRI_REF(arg) arg*
    #define NRI_REF_ACCESS(arg) (arg)
    #define NRI_ENUM_MEMBER(name, nameCaps, member) _NRI_ENUM_PREFIX(nameCaps)##member
    #define NRI_DEFAULT_VALUE(arg)
    #define NRI_ZERO_INIT {0}

    #define NRI_ENUM_BITS(name, nameCaps, type, ...) \
        NRI_ENUM(name, nameCaps, type, __VA_ARGS__)

    #define NRI_ENUM(name, nameCaps, type, ...) \
        typedef type NRI_NAME(name); \
        typedef enum \
        { \
            _NRI_EXPAND_ENUM(_NRI_ENUM_PREFIX(nameCaps), __VA_ARGS__) \
        } _NRI_ENUM_NAME(name)
#endif

#define NRI_REF_NAME(arg) NRI_REF(NRI_NAME(arg))
