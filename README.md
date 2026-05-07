# Content Collection Gallery Extension

A Quarto dashboard + Shiny configurator for creating curated content collections on Posit Connect — no server changes required.

## Component

### `configurator/` — Collection Configurator (only deployment)

A Shiny app deployed to Posit Connect that lets publishers create and edit content collections. Each collection is itself a Connect content item — a Quarto document — published by the configurator using the `rsconnect` package.

**Features:**
- Create new collections or edit existing ones in place
- Search and select Connect content, or pick a tag for dynamic resolution
- Set title, description, intro markdown, and a visual theme
- Settings travel with the published bundle (`collection.json`); no pins
- Async publish with progress + success/error toasts

The Quarto template ships inside the configurator at `configurator/dashboard_template/` and is bundled into each published collection.

## Setup

### 1. Deploy the Configurator

Deploy `configurator/` to Connect as a Shiny app. The configurator uses the auto-injected `CONNECT_SERVER` and `CONNECT_API_KEY` environment variables.

### 2. Create or Edit a Collection

1. Open the Configurator.
2. Choose **Create new collection...** (or pick an existing collection from the dropdown).
3. Search and select content (or pick a tag).
4. Set title, description, intro, theme.
5. Click **Save & Publish**. The configurator stages a bundle, deploys via `rsconnect`, and shows a progress toast.
6. When the toast switches to "Your collection is ready!", click the link to open the new (or updated) Connect content item.

## How It Works

The configurator copies `dashboard_template/` into a tempdir, writes a generated `collection.json` next to `index.qmd`, and calls `rsconnect::deployApp()` in a `callr` background process. On CREATE, it sets the Connect content `name` to `__content-collection__-<uuid>` (used as the discovery marker). On UPDATE, it passes `appId = <guid>` so Connect updates the same content item — preserving its URL, ACLs, schedule, and thumbnail.

The Quarto template at render time reads `collection.json` from the bundle and resolves content metadata (and tag-based queries) via the Connect API.
