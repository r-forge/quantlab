@echo off

REM package name
set PACKAGE_NAME=QuantLab

REM package version
set PACKAGE_VERSION=1.3

REM locale variables
set HTML_HELP_COMPILER_HOME=C:\Programme\HTML Help Workshop
set LATEX_HOME=C:\Programme\MiKTeX\miktex
set PERL_HOME=C:\Programme\Perl
set R_HOME=C:\Programme\R\R-2.8.1
set RTOOLS_HOME=C:\Programme\rtools

REM adapt PATH variable appropriately
rem set PATH=%HTML_HELP_COMPILER_HOME%;%LATEX_HOME%\bin;%PERL_HOME%\bin;%R_HOME%\bin;%RTOOLS_HOME%\MinGW\bin;%RTOOLS_HOME%\bin;.;%PATH%
set PATH=%HTML_HELP_COMPILER_HOME%;%RTOOLS_HOME%\MinGW\bin;%RTOOLS_HOME%\perl\bin;%RTOOLS_HOME%\bin;%R_HOME%\bin;%PATH%

REM ----------------------------------------------------------------------------

REM handle build case
if /I %1 EQU build (

@echo.
@echo Building "%PACKAGE_NAME%" ...
@echo.

REM start build
Rcmd build pkg
)

REM handle check case
if /I %1 EQU check (

@echo.
@echo Checking "%PACKAGE_NAME%" version %PACKAGE_VERSION% ...
@echo.

REM start check
Rcmd check %PACKAGE_NAME%_%PACKAGE_VERSION%.tar.gz
)

REM handle install case
if /I %1 EQU install (

@echo.
@echo Installing "%PACKAGE_NAME%" version %PACKAGE_VERSION% ...
@echo.

REM start install
Rcmd install %PACKAGE_NAME%_%PACKAGE_VERSION%.tar.gz
)

@echo on