"""Run CPU regression tests against extracted production methods and real engine math.

Requires a configured Windows build and built RelWithDebInfo xrCore/xrEngineCore.
Run from any directory: python tests/visual_scale/run_tests.py
"""

import json
import os
from pathlib import Path
import subprocess
import xml.etree.ElementTree as ET


ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / "build"
HERE = Path(__file__).resolve().parent


def extract_method(path, name):
    source = (ROOT / path).read_text(encoding="utf-8-sig")
    start = source.index(name)
    start = source.rfind("\n", 0, start) + 1
    end = source.index("{", start) + 1
    depth = 1
    while depth:
        depth += (source[end] == "{") - (source[end] == "}")
        end += 1
    return source[start:end]


def main():
    fixture = (HERE / "fixture.cpp.in").read_text(encoding="utf-8")
    for index, (path, name) in enumerate(json.loads((HERE / "methods.json").read_text())):
        fixture = fixture.replace(f"@METHOD_{index}@", extract_method(path, name))
    fixture = fixture.replace('"../src/', '"' + ROOT.as_posix() + '/src/')
    source = BUILD / "visual-scale-test.cpp"
    source.write_text(fixture, encoding="utf-8")

    ns = {"m": "http://schemas.microsoft.com/developer/msbuild/2003"}
    project = ET.parse(BUILD / "src/Layers/xrRenderPC_R4/xrRender_R4.vcxproj").getroot()
    group = next(g for g in project.findall("m:ItemDefinitionGroup", ns)
                 if "RelWithDebInfo" in g.attrib.get("Condition", ""))
    compiler = group.find("m:ClCompile", ns)
    includes = compiler.find("m:AdditionalIncludeDirectories", ns).text.split(";")
    defines = compiler.find("m:PreprocessorDefinitions", ns).text.split(";")
    args = ["/nologo", "/std:c++latest", "/EHsc", "/MD", "/utf-8", "/D_CRT_SECURE_NO_WARNINGS"]
    args += [f'/I"{path}"' for path in includes if not path.startswith("%")]
    args += [f'/D"{value}"' for value in defines
             if not value.startswith(("%", "CMAKE_INTDIR"))
             and value not in {"XRRENDER_R4_EXPORTS", "RENDER_API_EXPORTS", "_USRDLL"}]
    executable = BUILD / "bin/RelWithDebInfo/visual-scale-test.exe"
    args += [f'/Fo"{BUILD / "visual-scale-test.obj"}"', f'/Fe"{executable}"', f'"{source}"',
             "/link", f'/LIBPATH:"{BUILD / "lib/RelWithDebInfo"}"', "xrCore.lib", "xrEngineCore.lib"]
    response = BUILD / "visual-scale-test.rsp"
    response.write_text(" ".join(args), encoding="utf-8")
    vswhere = Path(os.environ["ProgramFiles(x86)"]) / "Microsoft Visual Studio/Installer/vswhere.exe"
    vcvars = subprocess.check_output([str(vswhere), "-latest", "-products", "*", "-find",
                                     "VC/Auxiliary/Build/vcvars64.bat"], text=True).strip().splitlines()[0]
    subprocess.run(f'"{vcvars}" >nul && cl @"{response}"', shell=True, cwd=ROOT, check=True)
    subprocess.run([str(executable)], cwd=ROOT, check=True)


if __name__ == "__main__":
    main()
