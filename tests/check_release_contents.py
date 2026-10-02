"""Check tracked and nonignored candidate release files; print no secret values.

Ignored local work/output is not a GitHub release input. The dated release audit
reviews locally reachable history separately; this test does not certify it.
"""
from pathlib import Path
import re
import subprocess

root = Path(__file__).resolve().parents[1]
names = subprocess.check_output(
    ['git', 'ls-files', '--cached', '--others', '--exclude-standard', '-z'], cwd=root
).decode().split('\0')
forbidden_parts = {'.claude', '.codex', '.DS_Store', '__pycache__', 'archive', '_maintainer',
                   'data', 'derived', 'cache', 'library', 'output', 'outputs', 'work'}
forbidden_suffixes = {'.rds', '.rda', '.rdata', '.parquet', '.feather', '.docx', '.pdf',
                      '.png', '.jpg', '.jpeg', '.zip', '.gz', '.pyc', '.log'}
obsolete = ('model-fitting/R/', 'model-fitting/scripts/', 'model-fitting/reference/',
            'model-fitting/vendor/', 'results/bias/')
patterns = {
    'private key': re.compile(r'-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----'),
    'GitHub token': re.compile(r'\b(?:gh[pousr]_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{40,})\b'),
    'AWS access key': re.compile(r'\b(?:AKIA|ASIA)[A-Z0-9]{16}\b'),
    'OpenAI key': re.compile(r'\bsk-(?:proj-|svcacct-)?[A-Za-z0-9_-]{40,}\b'),
    'private home path': re.compile(r'/(?:Users|home)/[A-Za-z0-9_.-]+/'),
}
checked = 0
def require(condition, message):
    if not condition:
        raise ValueError(message)

for name in sorted(set(filter(None, names))):
    p = root/name
    require(not set(Path(name).parts) & forbidden_parts, f'Excluded path: {name}')
    require(not name.startswith(obsolete), f'Obsolete pipeline: {name}')
    require(p.suffix.lower() not in forbidden_suffixes, f'Excluded file type: {name}')
    require(p.is_file() and not p.is_symlink(), f'Non-file or symlink: {name}')
    data = p.read_bytes()
    require(len(data) < 15_000_000 and b'\x00' not in data, f'Large/binary file: {name}')
    content = data.decode('utf-8')
    for label, pattern in patterns.items():
        require(not pattern.search(content), f'{label} candidate: {name}')
    checked += 1
print(f'PASS: {checked} candidate public files; no excluded paths, binary data, home paths or recognized secret patterns.')
