# Copilot Instructions for SimplerTimes

## Project overview
- Java project using Gradle and JavaFX.
- Primary module: `simplerTimesFx`.
- Packages are under `org.hansib.simplertimes`.
- Uses custom `org.hansib.*` support libraries for JavaFX helpers, logging, and app utilities.
- UI is built with FXML controllers and JavaFX controls.

## Coding style
- Use 4-space indentation.
- Use `private final` fields for immutable state.
- Prefer `final` for local variables when it improves readability.
- Group imports logically and use static imports for localized UI strings where appropriate.
- Put a GPL file header at top of source files in this module.
- Use `LogManager.getLogger()` with `private static final Logger log` for logging.
- Keep classes small and focused; prefer package-private visibility for internal helpers.

## JavaFX conventions
- Annotate UI fields with `@FXML` and keep them `private`.
- Use a no-arg `@FXML void initialize()` method for controller setup.
- Use `Platform.runLater(...)` for UI updates from non-JavaFX threads.
- Use `ControllerLoader` and `StageToggle` helpers from `org.hansib.sundries.fx` for FXML loading and stage handling.
- Use builder-style APIs (`ButtonBuilder`, `ButtonBuilder(graphic(...)).onAction(...).build()`) consistently.

## Naming and structure
- Use descriptive names for controllers, managers, and UI components, e.g. `TimesMainController`, `ExitManager`, `TrayIconMenu`.
- Keep domain model classes under `fx.data` and UI controllers in `fx` root package.
- Keep event-handling lambda bodies concise and delegate state changes to small helper methods.

## Patterns and architecture
- Prefer a single `main` application class extending `Application`.
- Centralize app shutdown logic with an `ExitManager` singleton and `addPreExitAction(...)` callbacks.
- Use `ObservableData` and `FxProject` for shared UI state.
- Use `SpanRecorder` to encapsulate recording behavior and bind button state to recording state.
- Use `Duration` / `java.time` APIs instead of raw primitives for time handling.
- Use `FilteredList` and property bindings for reactive UI state.

## UI and platform integration
- Use `SystemTray` integration only when supported; otherwise fall back to normal window close handling.
- Keep platform-specific UI code isolated in classes like `TrayIconMenu`.
- Use localized menu labels via `MenuItems` and `General` enums.

## Best guidance for Copilot suggestions
- Suggest code aligned with JavaFX idioms and `org.hansib.sundries.fx` helper classes.
- Prefer lambda expressions and method references for event handlers.
- Avoid introducing external frameworks or styles not already present in the repository.
- Keep generated code consistent with the project’s focus on small helper classes and declarative UI wiring.
