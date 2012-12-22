
!include MUI2.nsh

!define APP_NAME    "Kanjiget"
!define APP_VERSION "0.1"

CRCCheck On
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
;; !insertmacro MUI_PAGE_COMPONENTS ;; No components yet
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Install" KanjigetCoreFiles
	SectionIn RO
	SetOutPath "$INSTDIR"
	File "out\Kanjiget.exe"
	File "..\BUILD.txt"
	File "..\LICENSE.txt"
	File "..\README.md"

	SetOutPath "$INSTDIR\lib"
	File /r "out\lib\*.*"
	
	SetOutPath "$INSTDIR\data"
	File "..\data\*.dat"
	File "..\data\LICENSE.txt"
	
	CreateShortcut "$DESKTOP\${APP_NAME}.lnk" "$INSTDIR\Kanjiget.exe" ""
	CreateDirectory "$SMPROGRAMS\${APP_NAME}"
	CreateShortCut "$SMPROGRAMS\${APP_NAME}\Uninstall.lnk" "$INSTDIR\Uninstall.exe" "" "$INSTDIR\Uninstall.exe" 0
	CreateShortCut "$SMPROGRAMS\${APP_NAME}\${APP_NAME}.lnk" "$INSTDIR\Kanjiget.exe" "" "$INSTDIR\Kanjiget.exe" 0
	
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}-${APP_VERSION}" "DisplayName" "${APP_NAME} ${APP_VERSION} (remove only)"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}-${APP_VERSION}" "UninstallString" "$INSTDIR\uninstall.exe"
	WriteUninstaller "$INSTDIR\Uninstall.exe"
SectionEnd

Section "Uninstall"
	SetShellVarContext all
	
	RMDir /r "$INSTDIR\*.*"    
	RMDir "$INSTDIR"	
	Delete $INSTDIR\Uninstall.exe  
	DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}-${APP_VERSION}"
	
	Delete "$DESKTOP\${APP_NAME}.lnk"
	Delete "$SMPROGRAMS\${APP_NAME}\*.*"
	RmDir  "$SMPROGRAMS\${APP_NAME}"
SectionEnd
