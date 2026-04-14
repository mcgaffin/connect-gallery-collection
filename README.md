# Content Collection Gallery Extension

A Quarto dashboard + Shiny configurator for creating curated content collections on Posit Connect — no server changes required.

## Components

### `dashboard/` — Collection Dashboard

A Quarto document that renders a themed collection of content items. It reads its configuration from the `COLLECTION_CONFIG` environment variable and fetches content metadata from the Connect API at render time.

**Features:**
- Card grid layout with content type, description, owner, and date
- 6 visual themes (warm, cool, minimal, fun, bold, earth)
- Markdown introduction section
- Tag-based dynamic collections (resolves tagged content at render time)
- Manual collections (explicit list of content GUIDs)

### `configurator/` — Collection Configurator

A Shiny app that lets publishers create and edit collection configurations. It writes the config as an environment variable on the target dashboard and triggers a re-render.

**Features:**
- Search and select content from Connect
- Select a tag for dynamic collections
- Set title, description, and intro markdown
- Pick a visual theme
- Load and edit existing collections
- Save config and trigger re-render

## Setup

### 1. Deploy the Dashboard

Deploy `dashboard/index.qmd` to Connect as a Quarto document. Note the content GUID after deployment.

### 2. Deploy the Configurator

Deploy `configurator/app.R` to Connect as a Shiny app. The configurator uses the Connect API with the auto-injected `CONNECT_SERVER` and `CONNECT_API_KEY` environment variables.

### 3. Create a Collection

1. Open the Configurator
2. Enter the Dashboard's GUID
3. Search and select content (or choose a tag)
4. Set title, description, theme
5. Click "Save & Render"
6. Share the Dashboard URL with your team

### 4. Edit a Collection

1. Open the Configurator
2. Enter the Dashboard's GUID
3. Click "Load Existing Config"
4. Make changes
5. Click "Save & Render"

## How It Works

The Configurator stores collection configuration as a JSON environment variable (`COLLECTION_CONFIG`) on the Dashboard content item. When the Dashboard renders, it reads this variable and fetches content metadata from the Connect API to build the collection view.

Tag-based collections resolve dynamically — when new content is tagged, it appears in the collection on the next render.
