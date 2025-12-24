# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

SSC Sales Application Renewal - Sales reform project for Sakata Seed Corporation (株式会社サカタのタネ) Domestic Sales Division 2, involving NAVS system abolition and migration.

## Copyright & Authorship

- **Copyright**: Sakata Seed Corporation (株式会社サカタのタネ)
- **Created/Updated by**: REGAZE Corporation (REGAZE株式会社)

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

### 5. Use Temporary Files Over Context Memory
- **Actively create temporary files** in `temp/` or `work/` during task execution
- **DO NOT rely on ambiguous context memory** - persist important intermediate data, notes, and state to files
- This ensures accuracy and prevents information loss across long conversations

### 6. Internet Research for Missing Information
- **When information is not available in the project directory, MUST collect factual information from the internet**
- **Use `curl` with appropriate User-Agent** instead of fetch tools (which have Agent specification issues)
  ```bash
  curl -A "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36" -s "URL"
  ```
- **Save raw responses to `{topic}/raw/`** directory for reference and traceability

## Directory Structure Concept

### Design Principles
1. **Separation of Concerns**: Clear boundaries between webapp/api/database/infrastructure
2. **Flat Hierarchy**: Minimize nesting depth for LLM operation efficiency
3. **Unique Naming**: Avoid ambiguous names (e.g., `db-definitions` vs `source/database`)
4. **Scalability**: Easy to add new modules without restructuring

### Project Root Structure

```
project-root/
├── src/                 # Application source code (see below)
├── lib/                 # External libraries (symlinks)
├── material/            # Design documents & specifications
├── source/              # Migration source data (NAVS/REGAZE/SSC)
│   └── database/        # Legacy database exports
├── pjm/                 # Project management
├── work/                # Working files
├── temp/                # Temporary files
├── analysis/            # Analysis & research
└── repository-navs/     # NAVS repository related
```

### src/ Structure (Application Code)

```
src/
├── webapp/              # React frontend
│   ├── components/      # UI components
│   │   ├── common/      # Reusable components
│   │   └── layout/      # Layout components
│   ├── features/        # Feature modules
│   ├── hooks/           # Custom hooks
│   ├── pages/           # Page components
│   ├── routes/          # Routing definitions
│   ├── services/        # API communication
│   ├── store/           # State management
│   ├── styles/          # CSS/SCSS
│   ├── types/           # TypeScript types
│   └── assets/          # Static resources (fonts, images)
│
├── api/                 # Backend API
│   ├── controllers/     # Request handlers
│   ├── routes/          # Route definitions
│   ├── middleware/      # Auth, logging, error handling
│   ├── services/        # Business logic
│   ├── repositories/    # Data access layer
│   └── validators/      # Input validation
│
├── db-definitions/      # NEW system database definitions
│   ├── schema/          # DDL (tables, indexes, constraints)
│   ├── migrations/      # Migration files
│   ├── seeds/           # Initial data
│   │   ├── master/      # Master data
│   │   └── demo/        # Demo data
│   ├── functions/       # Stored procedures
│   └── views/           # Database views
│
├── gateway/             # External integrations (TBD)
│   ├── adapters/        # External system adapters
│   └── config/          # Gateway configuration
│
├── ops/                 # Operations tools
│   ├── scripts/         # Operation scripts
│   ├── batch/           # Batch processing
│   └── maintenance/     # Maintenance tools
│
├── shared/              # Shared modules
│   ├── types/           # Common type definitions
│   ├── constants/       # Common constants
│   └── utils/           # Common utilities
│
└── infrastructure/      # Infrastructure as Code
    ├── terraform/       # Terraform (AWS)
    │   ├── environments/  # dev/stg/prod
    │   ├── modules/       # vpc/rds/ecs/lambda/s3
    │   └── variables/
    ├── docker/          # Docker configurations
    ├── kubernetes/      # K8s manifests
    ├── cicd/            # CI/CD pipelines
    └── config/          # Environment configs
```

### Directory Selection Guide

| Task | Directory |
|------|-----------|
| React component development | `src/webapp/` |
| API endpoint implementation | `src/api/` |
| Database schema changes | `src/db-definitions/` |
| External API integration | `src/gateway/` |
| Batch job creation | `src/ops/batch/` |
| Shared type definitions | `src/shared/types/` |
| Terraform/Docker | `src/infrastructure/` |
| Legacy data analysis | `source/` |
| Design documents | `material/` |
| Temporary work | `temp/` or `work/` |

### Important Distinctions

| Directory | Purpose |
|-----------|---------|
| `src/db-definitions/` | **NEW** system database definitions |
| `source/database/` | **Legacy** database exports for migration |

## Commands

*To be added after project setup*

## Architecture

*To be added after project setup*
