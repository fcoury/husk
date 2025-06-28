# Husk Release Process

This document outlines the process for creating releases for the Husk programming language.

## Overview

Releases are automated through GitHub Actions when a version tag is pushed. The workflow handles:
- Changelog generation
- Cross-platform binary builds
- GitHub release creation
- Publishing to crates.io

## Prerequisites

Before creating your first release, ensure:

1. **Crates.io Token**: Add your crates.io API token to GitHub repository secrets
   - Go to [crates.io](https://crates.io) → Account Settings → API Tokens
   - Create a new token with `publish-new` and `publish-update` scopes
   - Add to GitHub: Settings → Secrets and variables → Actions → New repository secret
   - Name: `CRATES_IO_TOKEN`

2. **Commit Convention**: Follow [Conventional Commits](https://www.conventionalcommits.org/) for automatic changelog generation
   - `feat:` for new features
   - `fix:` for bug fixes
   - `docs:` for documentation
   - `chore:` for maintenance tasks
   - `refactor:` for code refactoring
   - `test:` for test additions/changes

## Release Steps

### 1. Update Version

Edit `Cargo.toml` to update the version number:

```toml
[package]
name = "husk-lang"
version = "0.2.0"  # Update this
```

### 2. Update Lockfile

```bash
cargo update
cargo build
```

### 3. Commit Version Bump

```bash
git add Cargo.toml Cargo.lock
git commit -m "chore(release): prepare for v0.2.0"
git push origin master
```

### 4. Create and Push Tag

```bash
git tag v0.2.0
git push origin v0.2.0
```

## What Happens Next

The GitHub Actions workflow (`release.yml`) automatically:

1. **Generates Changelog** using git-cliff from commit messages
2. **Builds Binaries** for multiple platforms:
   - Linux x64 (`husk-lang-x86_64-linux.tar.gz`)
   - Windows x64 (`husk-lang-x86_64-windows.exe.zip`)
   - macOS x64 (`husk-lang-x86_64-macos.tar.gz`)
   - macOS ARM64 (`husk-lang-aarch64-macos.tar.gz`)
3. **Creates GitHub Release** with:
   - Generated changelog as release notes
   - All platform binaries as downloadable assets
4. **Publishes to crates.io** making it available via `cargo install husk-lang`

## Release Workflow Configuration

The release process is configured in `.github/workflows/release.yml` and uses:
- `git-cliff` for changelog generation (configured in `cliff.toml`)
- Cross-platform builds using GitHub Actions matrix strategy
- `softprops/action-gh-release` for GitHub release creation
- Native `cargo publish` for crates.io publishing

## Version Numbering

Husk follows [Semantic Versioning](https://semver.org/):
- **MAJOR** version for incompatible API changes
- **MINOR** version for backwards-compatible functionality additions
- **PATCH** version for backwards-compatible bug fixes

Pre-1.0.0, the API is considered unstable and minor versions may include breaking changes.

## Troubleshooting

### Release Workflow Fails

1. **Permission Error**: Ensure the workflow has `contents: write` permission
2. **Crates.io Error**: Verify the `CRATES_IO_TOKEN` secret is set correctly
3. **Build Failures**: Check platform-specific build logs in GitHub Actions

### Manual Release

If automation fails, you can release manually:

```bash
# Build and test locally
cargo build --release
cargo test --release

# Publish to crates.io
cargo publish

# Create GitHub release manually with binaries
```

## Website Deployment

The website is automatically deployed to GitHub Pages when changes are pushed to the `website/` directory on the master branch. See `.github/workflows/deploy-website.yml` for configuration.

## Best Practices

1. **Test Before Release**: Run the full test suite before creating a release
2. **Update Documentation**: Ensure README and docs reflect new features
3. **Release Notes**: The changelog is auto-generated, but you can edit the GitHub release for additional context
4. **Announce**: Consider announcing major releases in:
   - GitHub Discussions
   - Project website
   - Social media channels

## Related Files

- `.github/workflows/release.yml` - Release automation workflow
- `cliff.toml` - Changelog generation configuration
- `.github/workflows/deploy-website.yml` - Website deployment workflow
- `Cargo.toml` - Package metadata and version