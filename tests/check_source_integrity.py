"""Portable standard-library checks of imported sources and publication-only inputs."""
import csv
import hashlib
from pathlib import Path

root = Path(__file__).resolve().parents[1]
rows = list(csv.DictReader((root/'publication/source_manifest.csv').open()))
for row in rows:
    path = root/row['path']
    assert path.is_file(), row['path']
    assert hashlib.sha256(path.read_bytes()).hexdigest() == row['sha256'], row['path']
    assert path.stat().st_size == int(row['bytes']), row['path']
for directory in (root/'publication', root/'model-fitting/current'):
    for path in directory.rglob('*'):
        if not path.is_file():
            continue
        assert path.suffix.lower() not in ('.rds','.rda','.rdata','.parquet','.feather','.docx','.pdf','.zip'), str(path)
        assert path.stat().st_size < 15_000_000, str(path)
assert len(list((root/'publication/reference_tables').glob('Table_*.csv'))) == 27
print(f'PASS: {len(rows)} imported-file SHA256 checks; no record-level binary files or manuscripts in current release inputs.')
