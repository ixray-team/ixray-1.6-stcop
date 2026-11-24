"""dbgout.py <cwd> <out file> <command line...>: runs a process as its debugger and writes its debug strings to a file.
Stops at the first breakpoint after the loader's own, which is where the D3D12 debug layer breaks on an error."""
import ctypes, sys
from ctypes import wintypes as w

k = ctypes.WinDLL("kernel32", use_last_error=True)


class STARTUPINFO(ctypes.Structure):
    _fields_ = [("cb", w.DWORD), ("r", w.LPWSTR), ("d", w.LPWSTR), ("t", w.LPWSTR), ("x", w.DWORD * 8), ("f", w.DWORD), ("s", w.WORD), ("c", w.WORD), ("p", ctypes.c_void_p), ("i", w.HANDLE), ("o", w.HANDLE), ("e", w.HANDLE)]


class PROCESS_INFORMATION(ctypes.Structure):
    _fields_ = [("hProcess", w.HANDLE), ("hThread", w.HANDLE), ("pid", w.DWORD), ("tid", w.DWORD)]


class EXCEPTION_RECORD(ctypes.Structure):
    _fields_ = [("Code", w.DWORD), ("Flags", w.DWORD), ("Record", ctypes.c_void_p), ("Address", ctypes.c_void_p), ("Count", w.DWORD), ("Info", ctypes.c_void_p * 15)]


class EXCEPTION_DEBUG_INFO(ctypes.Structure):
    _fields_ = [("Record", EXCEPTION_RECORD), ("First", w.DWORD)]


class OUTPUT_DEBUG_STRING_INFO(ctypes.Structure):
    _fields_ = [("Data", ctypes.c_void_p), ("Unicode", w.WORD), ("Length", w.WORD)]


class U(ctypes.Union):
    _fields_ = [("Exception", EXCEPTION_DEBUG_INFO), ("String", OUTPUT_DEBUG_STRING_INFO), ("Pad", ctypes.c_byte * 160)]


class DEBUG_EVENT(ctypes.Structure):
    _fields_ = [("Code", w.DWORD), ("pid", w.DWORD), ("tid", w.DWORD), ("u", U)]


cwd, out = sys.argv[1], sys.argv[2]
line = ctypes.create_unicode_buffer(" ".join('"%s"' % a if " " in a else a for a in sys.argv[3:]))
si = STARTUPINFO()
si.cb = ctypes.sizeof(si)
pi = PROCESS_INFORMATION()
DEBUG_ONLY_THIS_PROCESS = 2

if not k.CreateProcessW(None, line, None, None, False, DEBUG_ONLY_THIS_PROCESS, None, cwd, ctypes.byref(si), ctypes.byref(pi)):
    raise ctypes.WinError(ctypes.get_last_error())

k.ReadProcessMemory.argtypes = [w.HANDLE, ctypes.c_void_p, ctypes.c_void_p, ctypes.c_size_t, ctypes.c_void_p]
ev = DEBUG_EVENT()
breaks = 0
DBG_CONTINUE, DBG_NOT_HANDLED = 0x00010002, 0x80010001

with open(out, "w", encoding="utf-8", errors="replace") as f:
    while k.WaitForDebugEvent(ctypes.byref(ev), 0xFFFFFFFF):
        status = DBG_CONTINUE

        if ev.Code == 8:
            s = ev.u.String
            buf = ctypes.create_string_buffer(s.Length * (2 if s.Unicode else 1))
            k.ReadProcessMemory(pi.hProcess, s.Data, buf, len(buf), None)
            text = buf.raw.decode("utf-16-le" if s.Unicode else "mbcs", "replace").rstrip("\0")
            f.write(text if text.endswith("\n") else text + "\n")
            f.flush()
        elif ev.Code == 1:
            code = ev.u.Exception.Record.Code

            if code == 0x80000003:
                breaks += 1

                if breaks > 1:
                    f.write("== breakpoint %d\n" % breaks)
                    k.TerminateProcess(pi.hProcess, 3)
            elif code == 0x87A:
                pass
            elif code not in (0x406D1388, 0xE06D7363, 0x4001000A, 0x40010006):
                f.write("== exception %08x first=%d\n" % (code, ev.u.Exception.First))
                status = DBG_NOT_HANDLED

                if not ev.u.Exception.First:
                    k.TerminateProcess(pi.hProcess, 3)
            else:
                status = DBG_NOT_HANDLED
        elif ev.Code == 5:
            break

        k.ContinueDebugEvent(ev.pid, ev.tid, status)
