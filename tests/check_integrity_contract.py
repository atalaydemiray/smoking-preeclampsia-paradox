"""Synthetic release-gate fixtures. No study inputs, records, or network access."""
import csv
import hashlib
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

source = Path(__file__).resolve().parent
def require(condition, message):
    if not condition:
        raise RuntimeError(message)

def run(checker, expected_success):
    result = subprocess.run([sys.executable, '-O', str(checker)], capture_output=True, text=True)
    require((result.returncode == 0) == expected_success, f'Unexpected gate status: {checker.name}')

with tempfile.TemporaryDirectory(prefix='sep_integrity_fixture_') as directory:
    root = Path(directory)
    (root/'tests').mkdir()
    for name in ('check_source_integrity.py', 'check_release_contents.py'):
        shutil.copy2(source/name, root/'tests'/name)
    refs = root/'publication/reference_tables'
    refs.mkdir(parents=True)
    for index in range(27):
        (refs/f'Table_fixture_{index}.csv').write_text('label,value\nsynthetic,1\n')
    data = root/'publication/input.csv'
    data.write_text('synthetic fixture,not study data\n')
    manifest = root/'publication/source_manifest.csv'
    with manifest.open('w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=['path','bytes','sha256','source'])
        writer.writeheader()
        writer.writerow(dict(path='publication/input.csv', bytes=data.stat().st_size,
                             sha256=hashlib.sha256(data.read_bytes()).hexdigest(), source='synthetic'))
    checker = root/'tests/check_source_integrity.py'
    run(checker, True)
    data.write_text('intentionally changed synthetic fixture\n')
    run(checker, False)
    # Minimal temporary Git index; no commit, push or modification of the real repo.
    subprocess.run(['git','init','-q',str(root)], check=True, capture_output=True)
    checker = root/'tests/check_release_contents.py'
    run(checker, True)
    excluded = root/'data'
    excluded.mkdir()
    (excluded/'unapproved.csv').write_text('synthetic record fixture\n')
    run(checker, False)
print('PASS: optimized-Python integrity checks reject tampered sources and new unapproved files.')
