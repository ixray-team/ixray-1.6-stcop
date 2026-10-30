"""Compile regression tests using production threat selection and behavior dispatch methods."""
import importlib.util
import os
from pathlib import Path
import subprocess
ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("extract", ROOT / "tests/visual_scale/run_tests.py")
extract = importlib.util.module_from_spec(spec)
spec.loader.exec_module(extract)
fixture = Path(__file__).with_name("fixture.cpp.in").read_text(encoding="utf-8")
targets = (ROOT / "src/FFxExtendedEntityesLogic/Perceptions/CELSharedTargets.cpp").read_text(encoding="utf-8-sig")
fixture = fixture.replace("@TARGETS@", "\n".join(line for line in targets.splitlines() if not line.startswith("#include")))
for token, path, name in [
    ("SELECT", "CoreComponent/CELComponentBase.cpp", "CELComponentBase::SelectTarget()"),
    ("DAMAGE", "CoreComponent/CELComponentBase.cpp", "CELComponentBase::ObserveDamage("),
    ("GROUND", "EntityesCategory/Ground/Behaviors/CELGroundBehaviorManager.cpp", "CELGroundBehaviorManager::UpdateGround("),
    ("FLIGHT", "EntityesCategory/Flying/Behaviors/CELFlyingBehaviorManager.cpp", "CELFlyingBehaviorManager::UpdateFlight("),
    ("COMMITTED", "EntityesCategory/Flying/Entityes/Aggressive/CELFlyingAttackBase.cpp", "CELFlyingAttackBase::SelectTarget()"),
    ("IGNORED", "EntryPoint.cpp", "bool ExtendedEntityes::IsIgnored("),
]:
    fixture = fixture.replace("@" + token + "@", extract.extract_method("src/FFxExtendedEntityesLogic/" + path, name))
build = ROOT / "build"
source = build / "monster-threat-test.cpp"
source.write_text(fixture, encoding="utf-8")
exe = build / "monster-threat-test.exe"
vswhere = Path(os.environ["ProgramFiles(x86)"]) / "Microsoft Visual Studio/Installer/vswhere.exe"
vcvars = subprocess.check_output([str(vswhere), "-latest", "-products", "*", "-find", "VC/Auxiliary/Build/vcvars64.bat"], text=True).strip().splitlines()[0]
subprocess.run(f'"{vcvars}" >nul && cl /nologo /std:c++latest /EHsc /utf-8 /Fo"{build / "monster-threat-test.obj"}" /Fe"{exe}" "{source}"', shell=True, cwd=ROOT, check=True)
subprocess.run([str(exe)], cwd=ROOT, check=True)
