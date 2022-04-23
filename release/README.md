# Making and uploading the Gitlab release to downloads.haskell.org

1. Run the gitlab release pipeline using https://gitlab.haskell.org/haskell/haskell-language-server/-/pipelines/new
2. Once the pipeline has completed, download the artifacts using `fetch_gitlab.py`
   - For example for the `1.7.0.0` release: `python fetch_gitlab.py -p <pipeline_id> --output haskell-language-server-1.7.0.0 -r 1.7.0.0`
   - Ensure all the artifacts in the output directory are accurate and add any missing/extra artifacts
3. `cd` to the output directory created in the previous step, and run `SIGNING_KEY=<your signing key> ../upload.sh`
