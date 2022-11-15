# adapted from https://gitlab.haskell.org/bgamari/ghc-utils/-/blob/master/rel-eng/fetch-gitlab-artifacts/fetch_gitlab.py
import logging
from pathlib import Path
import subprocess
import gitlab

logging.basicConfig(level=logging.INFO)

def strip_prefix(s, prefix):
    if s.startswith(prefix):
        return s[len(prefix):]
    else:
        return None

def fetch_artifacts(release: str, pipeline_id: int,
                    dest_dir: Path, gl: gitlab.Gitlab):
    dest_dir.mkdir(exist_ok=True)
    proj = gl.projects.get('haskell/haskell-language-server')
    pipeline = proj.pipelines.get(pipeline_id)
    tmpdir = Path("fetch-gitlab")
    tmpdir.mkdir(exist_ok=True)
    for pipeline_job in pipeline.jobs.list(all=True):
        if len(pipeline_job.artifacts) == 0:
            logging.info(f'job {pipeline_job.name} ({pipeline_job.id}) has no artifacts')
            continue

        job = proj.jobs.get(pipeline_job.id)
        platform = strip_prefix(job.name, 'tar-')
        if not platform:
            logging.info(f'Skipping {job.name} (not a tar job)')
            continue
        try:
            destdir = tmpdir / job.name
            zip_name = Path(f"{tmpdir}/{job.name}.zip")
            if not zip_name.exists() or zip_name.stat().st_size == 0:
                logging.info(f'downloading archive {zip_name} for job {job.name} (job {job.id})...')
                with open(zip_name, 'wb') as f:
                    job.artifacts(streamed=True, action=f.write)

            if zip_name.stat().st_size == 0:
                logging.info(f'artifact archive for job {job.name} (job {job.id}) is empty')
                continue

            extension = 'zip' if job.name.endswith('windows') else 'tar.xz'
            dest = dest_dir / f'haskell-language-server-{release}-{platform}.{extension}'
            if dest.exists():
                logging.info(f'bindist {dest} already exists')
                continue

            subprocess.run(['unzip', '-bo', zip_name, '-d', destdir])
            bindist_files = list(destdir.glob(f'*/haskell-language-server*.{extension}'))
            if len(bindist_files) == 0:
                logging.warn(f'Bindist does not exist')
                continue

            bindist = bindist_files[0]
            logging.info(f'extracted {job.name} to {dest}')
            bindist.replace(dest)
        except Exception as e:
            logging.error(f'Error fetching job {job.name}: {e}')
            pass

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--pipeline', '-p', required=True, type=int, help="pipeline id")
    parser.add_argument('--release', '-r', required=True, type=str, help="release name")
    parser.add_argument('--output', '-o', type=Path, default=Path.cwd(), help="output directory")
    parser.add_argument('--profile', '-P', default='haskell',
                        help='python-gitlab.cfg profile name')
    args = parser.parse_args()
    gl = gitlab.Gitlab.from_config(args.profile)
    fetch_artifacts(args.release, args.pipeline,
                    dest_dir=args.output, gl=gl)

if __name__ == '__main__':
  main()
