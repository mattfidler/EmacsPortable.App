:: See http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/percent.mspx?mfr=true
@set PATH=%~dp0\..\ezwin\bin;%~dp0\..\gw32\bin;%PATH%
@if EXIST %~dn1.org GOTO SKIPORG
@IF EXIST %1c GOTO COMPRESS
@echo %1 exists, but %1c does not exist, not comressing.
@GOTO SKIP

:COMPRESS
@gzip -9 -v %1
@GOTO SKIP

:SKIPORG
@echo I'm assuming that %1 is a literate .org intermediary file.

:SKIP

