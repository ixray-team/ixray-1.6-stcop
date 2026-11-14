import argparse
import asyncio
import ctypes
import json
import os
import struct
from pathlib import Path
from subprocess import list2cmdline
from tempfile import TemporaryDirectory
from typing import Any

from mcp.server.fastmcp import FastMCP


def reuse_editor(editor: Path, fsltx: Path, macro: Path, result_path: Path) -> int:
    import pywintypes
    import win32api
    import win32con
    import win32gui
    import win32process

    payload = json.dumps([str(fsltx), str(macro), str(result_path)], ensure_ascii=False).encode("utf-8")
    if len(payload) > 65536:
        raise ValueError("Macro request is too large")
    # WM_COPYDATA needs a pointer to the payload, which stays alive until the send returns.
    data = ctypes.create_string_buffer(payload)
    message = struct.pack("@PIP", 0x4958524D, len(payload), ctypes.addressof(data))
    user32 = ctypes.WinDLL("user32", use_last_error=True)
    get_property = user32.GetPropW
    get_property.argtypes = [ctypes.c_void_p, ctypes.c_wchar_p]
    get_property.restype = ctypes.c_void_p
    send = user32.SendMessageTimeoutW
    send.argtypes = [ctypes.c_void_p, ctypes.c_uint, ctypes.c_size_t, ctypes.c_void_p,
                     ctypes.c_uint, ctypes.c_uint, ctypes.POINTER(ctypes.c_size_t)]
    send.restype = ctypes.c_ssize_t
    status = 0

    def visit(window: int, _: None) -> bool:
        nonlocal status
        if not get_property(window, "IXRay.SdkMacro.v1"):
            return True
        _, pid = win32process.GetWindowThreadProcessId(window)
        try:
            process = win32api.OpenProcess(0x1000, False, pid)
            try:
                path = Path(win32process.GetModuleFileNameEx(process, 0))
            finally:
                process.Close()
        except pywintypes.error:
            return True
        if path != editor:
            return True
        reply = ctypes.c_size_t()
        if not send(window, win32con.WM_COPYDATA, 0, ctypes.cast(message, ctypes.c_void_p),
                    win32con.SMTO_ABORTIFHUNG, 1000, ctypes.byref(reply)):
            status = 3
            return True
        if reply.value in (1, 3):
            status = reply.value
        return reply.value != 1

    try:
        win32gui.EnumWindows(visit, None)
    except pywintypes.error:
        if status != 1:
            raise
    return status


async def open_editor(editor: Path, fsltx: Path, macro: Path, result_path: Path,
                      timeout: int) -> dict[str, Any]:
    try:
        status = reuse_editor(editor, fsltx, macro, result_path)
        if status == 3:
            return {"success": False, "code": "editor_busy", "exit_code": None}
        if status == 0:
            # Use the desktop Explorer to keep a newly launched editor alive after disconnect.
            from pythoncom import VT_BYREF, VT_I4
            from win32com.client import Dispatch, VARIANT

            windows = Dispatch("Shell.Application").Windows()
            hwnd = VARIANT(VT_BYREF | VT_I4, 0)
            desktop = windows.FindWindowSW(0, 0, 8, hwnd, 1)
            desktop.Document.Application.ShellExecute(
                str(editor), list2cmdline([
                    "--open-sdk-macro", str(macro), str(result_path), "-fsltx", str(fsltx)
                ]), str(fsltx.parent), "open", 1,
            )
    except Exception:
        return {"success": False, "code": "editor_launch_failed", "exit_code": None}

    try:
        async with asyncio.timeout(timeout):
            while True:
                try:
                    result = json.loads(result_path.read_text(encoding="utf-8"))
                except (OSError, ValueError):
                    await asyncio.sleep(0.1)
                    continue
                if isinstance(result, dict) and type(result.get("success")) is bool:
                    return {**result, "exit_code": None if result["success"] else 1}
                return {"success": False, "code": "invalid_editor_result", "exit_code": None}
    except TimeoutError:
        return {"success": False, "code": "timeout", "exit_code": None}


def create_server(editor: Path, fsltx: Path, macros: Path, timeout: int, xrai: Path | None = None) -> FastMCP:
    server = FastMCP("IX-Ray SDK Macros")
    run_lock = asyncio.Lock()
    editor_env = {**os.environ, "IXRAY_MACRO_FSLTX": str(fsltx)}
    if xrai is not None:
        editor_env["IXRAY_MACRO_XRAI"] = str(xrai)

    @server.tool()
    def list_sdk_macros() -> dict[str, Any]:
        """List .json graphs and .txt command lists in the configured macro directory."""
        formats = {".json": "graph", ".txt": "command_list"}
        entries = []
        for path in sorted(macros.iterdir(), key=lambda entry: (entry.name.casefold(), entry.name)):
            if ":" in path.name or "\\" in path.name:
                continue
            kind = formats.get(path.suffix.lower())
            if kind is None:
                continue
            try:
                resolved = path.resolve(strict=True)
                if resolved.parent == macros and resolved.is_file():
                    entries.append({"name": path.name, "format": kind})
            except (OSError, RuntimeError):
                continue
        return {"macros": entries}

    @server.tool()
    async def run_sdk_macro(name: str, keep_open: bool = False) -> dict[str, Any]:
        """Run a trusted Macro Editor JSON graph or text command-list macro by filename.

        JSON graphs return results in execution order and stop at the first failed node.
        The result reflects completion of the macro's command handlers. Commands that
        start background work may return before that work finishes.
        Set keep_open to run in an open editor for this SDK, or open one if needed.
        """
        if not name or Path(name).name != name or ":" in name or "\\" in name or name in {".", ".."}:
            raise ValueError("name must be a filename in the configured macro directory")
        macro = (macros / name).resolve()
        if macro.parent != macros or not macro.is_file():
            raise ValueError("Macro was not found in the configured directory")

        async with run_lock:
            with TemporaryDirectory(prefix="ixray-sdk-macro-") as directory:
                result_path = Path(directory) / "result.json"
                if keep_open:
                    return {**await open_editor(editor, fsltx, macro, result_path, timeout), "name": name}
                process = await asyncio.create_subprocess_exec(
                    str(editor), "--run-sdk-macro",
                    str(macro), str(result_path), "-fsltx", str(fsltx),
                    cwd=fsltx.parent, env=editor_env, stdin=asyncio.subprocess.DEVNULL,
                    stdout=asyncio.subprocess.DEVNULL, stderr=asyncio.subprocess.DEVNULL,
                )
                try:
                    await asyncio.wait_for(process.wait(), timeout)
                except (TimeoutError, asyncio.CancelledError) as error:
                    if process.returncode is None:
                        try:
                            process.kill()
                        except ProcessLookupError:
                            pass
                    await process.wait()
                    if isinstance(error, asyncio.CancelledError):
                        raise
                    return {"name": name, "success": False, "code": "timeout", "exit_code": process.returncode}

                try:
                    result = json.loads(result_path.read_text(encoding="utf-8"))
                    if not isinstance(result, dict) or type(result.get("success")) is not bool:
                        raise ValueError("Invalid editor result")
                except (OSError, ValueError):
                    return {"name": name, "success": False, "code": "missing_editor_result",
                            "exit_code": process.returncode}

                return {**result, "name": name, "exit_code": process.returncode,
                        "success": result["success"] and process.returncode == 0}

    return server


def main() -> None:
    sdk_root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description="Run existing Level Editor macros over local MCP stdio")
    parser.add_argument("--editor", type=Path, default=sdk_root / "bin" / "LevelEditor.exe")
    parser.add_argument("--fsltx", type=Path, default=sdk_root / "fs.ltx")
    parser.add_argument("--macros", type=Path, help="Trusted macro directory, defaults to SDK import")
    parser.add_argument("--xrai", type=Path, help="xrLevelBuilder with --headless-ai support")
    parser.add_argument("--timeout", type=int, default=120)
    args = parser.parse_args()
    editor, fsltx = args.editor.resolve(), args.fsltx.resolve()
    macros = (args.macros or fsltx.parent / "import").resolve()
    if not editor.is_file() or not fsltx.is_file() or args.timeout <= 0:
        parser.error(f"Expected an existing editor ({editor}), fs.ltx ({fsltx}) and a positive timeout")
    if args.macros is None:
        try:
            macros.mkdir(exist_ok=True)
        except OSError as error:
            parser.error(f"Cannot create macro directory {macros}: {error}")
    if not macros.is_dir():
        parser.error(f"Macro directory does not exist: {macros}")
    xrai = args.xrai.resolve() if args.xrai else None
    if xrai is not None and not xrai.is_file():
        parser.error("--xrai must point to an existing compiler")
    create_server(editor, fsltx, macros, args.timeout, xrai).run(transport="stdio")


if __name__ == "__main__":
    main()
