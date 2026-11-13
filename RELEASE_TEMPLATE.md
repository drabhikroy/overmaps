# Release checklist

Steps for cutting an Overmaps release, in order.

## Before tagging

- [ ] `OVERMAPS_VERSION` in `app.R` matches the version you are cutting.
- [ ] `DESCRIPTION` and `CITATION.cff` carry the same version.
- [ ] `CHANGELOG.md` has a section headed with that version.
- [ ] `Rscript tests/run_tests.R` passes.
- [ ] The app opens, a map draws, and a download produces a file.

## Tag and push

```sh
git add -A
git commit -m "Overmaps <version>: <one line summary>"
git push
git tag -a v<version> -m "Overmaps <version>"
git push origin v<version>
```

Pushing the tag starts the release workflow, which checks the version, extracts the matching `CHANGELOG.md` section, and drafts the release page. Nothing is published until you review the draft and publish it.

```sh
gh release edit v<version> --draft=false
```
