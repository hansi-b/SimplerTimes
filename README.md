# JavaFxTemplate
Convenience project layout template for [JavaFX](https://openjfx.io/) with modules and [Gradle](https://gradle.org/). Makes use of the magnificent [Badass JLink Plugin](https://badass-jlink-plugin.beryx.org/) for bundle creation.

Contains a minimal example "hello world" application which opens a window with a button and logs to the console.

## Usage

This call should suffice to create an executable bundle:

`./gradlew :fxApp:jpackageImage`

An executable `appFx` should be created in `fxApp/build/jpackage/appFx/bin/`

## Limitations

- Works with the command line and in IntelliJ, but not in Eclipse. (Most likely related to [this problem](https://github.com/eclipse/buildship/issues/658). It seems to work in Eclipse if you remove the `utilities` sub-project and dependency.)
