# ADR-002: Use Vue 3 + Tailwind CSS

## Status

Accepted

## Context

We need a frontend framework for the banking UI that provides:
- Modern, reactive user interface
- Component-based architecture
- Easy styling with responsive design
- Good developer experience
- Testing capabilities

Options considered:
1. **Vue 3 + Tailwind CSS** - Lightweight, approachable
2. **React + Tailwind CSS** - Most popular, large ecosystem
3. **Angular + Material** - Enterprise, opinionated
4. **Svelte + Tailwind** - Compile-time, smallest bundle

## Decision

Use **Vue 3 with Vite and Tailwind CSS v4**.

Rationale:
- Vue 3 Composition API is clean and intuitive
- Vite provides fastest development experience
- Tailwind CSS enables rapid UI development without custom CSS
- Smaller learning curve than React/Angular
- Excellent single-file component (SFC) support

## Consequences

### Positive
- Fast development with hot module replacement
- Utility-first CSS reduces context switching
- Small bundle size (~77KB gzipped)
- Easy to test with Vitest and Vue Test Utils
- Playwright for E2E testing

### Negative
- Smaller ecosystem than React
- Tailwind v4 is newer (less documentation)
- No built-in state management (Pinia needed for complex apps)

### Mitigations
- Simple `ref()` state in App.vue sufficient for demo scope
- Component-level state management keeps complexity low
