"""Portable standard-library checks of imported sources and publication-only inputs."""
import csv
import hashlib
from pathlib import Path

root = Path(__file__).resolve().parents[1]

def require(condition, message):
    # Integrity gates must remain active under `python -O`, which removes assert.
    if not condition:
        raise ValueError(message)

rows = list(csv.DictReader((root/'publication/source_manifest.csv').open()))
require(len({row['path'] for row in rows}) == len(rows), 'Duplicate source-manifest paths')
for row in rows:
    path = root/row['path']
    require(not Path(row['path']).is_absolute() and '..' not in Path(row['path']).parts,
            'Source-manifest paths must stay inside the repository')
    require(path.is_file() and not path.is_symlink(), f'Missing source or symlink: {row["path"]}')
    require(hashlib.sha256(path.read_bytes()).hexdigest() == row['sha256'], f'SHA256 mismatch: {row["path"]}')
    require(path.stat().st_size == int(row['bytes']), f'Size mismatch: {row["path"]}')
for directory in (root/'publication', root/'model-fitting/current'):
    for path in directory.rglob('*'):
        if not path.is_file():
            continue
        require(path.suffix.lower() not in ('.rds','.rda','.rdata','.parquet','.feather','.docx','.pdf','.zip'),
                f'Excluded release input type: {path.relative_to(root)}')
        require(path.stat().st_size < 15_000_000, f'Oversized release input: {path.relative_to(root)}')
require(len(list((root/'publication/reference_tables').glob('Table_*.csv'))) == 27, 'Expected 27 reference tables')
print(f'PASS: {len(rows)} imported-file SHA256 checks; no record-level binary files or manuscripts in current release inputs.')
