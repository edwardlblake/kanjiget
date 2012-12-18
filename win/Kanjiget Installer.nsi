
!include MUI2.nsh

!define APP_NAME    "Kanjiget"
!define APP_VERSION "0.2"

SetCompressor LZMA
Name "${APP_NAME} ${APP_VERSION}"
OutFile "KanjigetSetup-${APP_VERSION}-win32.exe"
Caption "${APP_NAME} Setup"
InstallDir "$PROGRAMFILES\${APP_NAME}"
InstallDirRegKey HKLM "Software\${APP_NAME}\Install" "InstallLocation"
RequestExecutionLevel highest

!define MUI_ABORTWARNING

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "..\LICENSE.txt"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Kanjiget" SectionKanjigetCore
	SectionIn RO
	SetOutPath "$INSTDIR"
	File "out\kanjiget.exe"
	File "..\BUILD.txt"
	File "..\LICENSE.txt"
	File "..\README.md"

	SetOutPath "$INSTDIR\lib"
	File "out\lib\iconv.dll"
	File "out\lib\libracket3m_8bh220.dll"
	
	SetOutPath "$INSTDIR\lib\plt\kanjiget\collects"
	;; empty
	
	SetOutPath "$INSTDIR\lib\plt\kanjiget\exts\ert"
	File "out\lib\plt\kanjiget\exts\ert\freetype6.dll"
	File "out\lib\plt\kanjiget\exts\ert\libcairo-2.dll"
	File "out\lib\plt\kanjiget\exts\ert\libexpat-1.dll"
	File "out\lib\plt\kanjiget\exts\ert\libfontconfig-1.dll"
	File "out\lib\plt\kanjiget\exts\ert\libglib-2.0-0.dll"
	File "out\lib\plt\kanjiget\exts\ert\libgmodule-2.0-0.dll"
	File "out\lib\plt\kanjiget\exts\ert\libgobject-2.0-0.dll"
	File "out\lib\plt\kanjiget\exts\ert\libjpeg-7.dll"
	File "out\lib\plt\kanjiget\exts\ert\libpango-1.0-0.dll"
	File "out\lib\plt\kanjiget\exts\ert\libpangocairo-1.0-0.dll"
	File "out\lib\plt\kanjiget\exts\ert\libpangoft2-1.0-0.dll"
	File "out\lib\plt\kanjiget\exts\ert\libpangowin32-1.0-0.dll"
	File "out\lib\plt\kanjiget\exts\ert\libpng14-14.dll"
	File "out\lib\plt\kanjiget\exts\ert\zlib1.dll"
	
	SetOutPath "$INSTDIR\data"
	File "..\data\kanjimtx.dat"
	File "..\data\knjidxl0.dat"
	File "..\data\knjirdc0.dat"
	File "..\data\LICENSE.txt"
	File "..\data\wiktdata.dat"
	File "..\data\wiktindx.dat"
	File "..\data\wiktlkup.dat"
	
SectionEnd
