# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

SSC Sales Application Renewal - Sales reform project for Sakata Seed Corporation (株式会社サカタのタネ) Domestic Sales Division 2, involving NAVS system abolition and migration.

## MANDATORY RULES (MUST FOLLOW)

### 1. File/Directory Creation Restrictions
- **DO NOT create any files or directories in the project root** except `CLAUDE.md`, `README.md`, and git-related files
- All work files MUST be created under appropriate subdirectories

### 2. README.md Verification Requirement
- **MUST read and understand `README.md` at the start of each new conversation**
- Always select the appropriate directory for each task

### 3. /tmp Usage Prohibited
- **NEVER use the system `/tmp` directory**
- Use the `temp/` directory for temporary files

### 4. Git Operation Rules
- **Commit after each work session** (git commit)
- **Push only when user gives approval** (e.g., "GJ", "Good Job", or explicit push request)

## Directory Structure

```
├── src/              # Source code
├── lib/              # External libraries (symlinks)
├── material/         # Materials/Documents
├── database/         # Database related
├── source/           # Original data/sources
├── work/             # Working files
├── temp/             # Temporary files (use instead of /tmp)
├── analysis/         # Analysis
└── repository-navs/  # NAVS repository related
```

## Commands

*To be added after project setup*

## Architecture

*To be added after project setup*
