import subprocess


subprocess.check_call(["black", "src"])
subprocess.check_call(["mypy", "src"])
subprocess.check_call(["pytest", "src/test"])
