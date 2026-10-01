"""Fail closed on private or obsolete files in the Git index; print no secret values."""
from pathlib import Path
import re
import subprocess

root = Path(__file__).resolve().parents[1]
names = subprocess.check_output(['git', 'ls-files', '-z'], cwd=root).decode().split('\0')
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
for name in filter(None, names):
    p = root/name
    assert not set(Path(name).parts) & forbidden_parts, f'Excluded path: {name}'
    assert not name.startswith(obsolete), f'Obsolete pipeline: {name}'
    assert p.suffix.lower() not in forbidden_suffixes, f'Excluded file type: {name}'
    assert p.is_file() and not p.is_symlink(), f'Non-file or symlink: {name}'
    data = p.read_bytes()
    assert len(data) < 15_000_000 and b'\x00' not in data, f'Large/binary file: {name}'
    content = data.decode('utf-8')
    for label, pattern in patterns.items():
        assert not pattern.search(content), f'{label} candidate: {name}'
    checked += 1
print(f'PASS: {checked} indexed public files; no excluded paths, binary data, home paths or recognized secret patterns.')
