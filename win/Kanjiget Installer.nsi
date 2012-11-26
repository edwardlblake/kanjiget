
!include MUI2.nsh

!define APP_NAME		"Kanjiget"
!define APP_VERSION "0.5"

SetCompressor LZMA
Name "${APP_NAME} (${APP_VERSION})"
OutFile "KanjigetSetup-${APP_VERSION}.exe"
Caption "${APP_NAME} Setup"
InstallDir "$PROGRAMFILES\${APP_NAME}"
InstallDirRegKey HKLM "Software\${APP_NAME}\Install" "InstallLocation"
RequestExecutionLevel highest

!define MUI_ABORTWARNING

!insertmacro MUI_PAGE_LICENSE "${NSISDIR}\Docs\Modern UI\License.txt"
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

	SetOutPath "$INSTDIR\lib"
	File "out\lib\iconv.dll"
	File "out\lib\libracket3m_8avmg8.dll"
	
	SetOutPath "$INSTDIR\lib\plt\kanjiget\collects"
	;; empty
	
	;; TODO
	;; SetOutPath "$INSTDIR\lib\plt\kanjiget\exts\ert\r0\Program Files (x86)\Racket\collects\icons\"
	;; File "out\lib\plt\kanjiget\exts\ert\r0\Program Files (x86)\Racket\collects\icons\anchor.gif"
	;; SetOutPath "$INSTDIR\lib\plt\kanjiget\exts\ert\r0\Program Files (x86)\Racket\lib\"
	;; File "out\lib\plt\kanjiget\exts\ert\r0\Program Files (x86)\Racket\lib\freetype6.dll"
	;; SetOutPath "$INSTDIR\lib\plt\kanjiget\exts\ert\r1\program files (x86)\racket\collects\scribble"
	;; File "out\lib\plt\kanjiget\exts\ert\r1\program files (x86)\racket\collects\scribble\scribble.css"
SectionEnd
