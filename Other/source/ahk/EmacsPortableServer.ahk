;;; EmacsPortableServer.ahk --- EmacsPortable Server AutoHotkey
;; 
;; Filename: EmacsPortableServer.ahk
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Jan 13 17:00:56 2011 (-0600)
;; Version: 
;; Last-Updated: Tue Jun 12 10:53:09 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 20
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  Psudo-server auto-hot-key script.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 13-Jan-2011    Matthew L. Fidler  
;;    Last-Updated: Thu Jan 13 17:01:04 2011 (-0600) #1 (Matthew L. Fidler)
;;    Removed Remember.  Now org-capture is where its at.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

Gui -Caption +ToolWindow

DetectHiddenWindows, on
  
  OnExit showdaemonexit
  ;OnExit xt
  
  WinHide ___EmacsPortableDaemon_%1%___
  WinHide \mingw\bin\gdb.exe
  WinHide \MinGW\bin\gdb.exe
  WinHide \cygwin\bin\gdb.exe
  
  Menu, Tray, NoStandard
  Menu, Tray, Add, New Emacs %1% Frame, newemacs
  Menu, Tray, Add,
  Menu, Tray, Add, Show Emacs %1% daemon window, showdaemon
  Menu, Tray, Add, Hide Emacs %1% daemon window, hidedaemon
  Menu, Tray, Add,
  Menu, Tray, Add, Show Emacs %1% daemon and remove tray icon, xt
  Menu, Tray, Add, E&xit Emacs %1%, showdaemonexit  
  Menu, Tray, Default, New Emacs %1% Frame
  
  Process, wait, emacs-%1%.exe, 300
  EmacsPID = %ErrorLevel%
  IniWrite "%EmacsPID%", "%TEMP%\ep-reg.ini", "pid", "%1%"
  HideDaemon = 1
  ExitDaemon = 0
  
  Loop {    
    If HideDaemon {
      SetTitleMatchMode 3
      WinHide ___EmacsPortableDaemon_%1%___
      SetTitleMatchMode 2
      WinHide \mingw\bin\gdb.exe
      WinHide \MinGW\bin\gdb.exe
      WinHide \cygwin\bin\gdb.exe
    } Else {
      SetTitleMatchMode 3
      WinShow ___EmacsPortableDaemon_%1%___
      SetTitleMatchMode 2
      WinShow \mingw\bin\gdb.exe
      WinShow \MinGW\bin\gdb.exe
      WinShow \cygwin\bin\gdb.exe
    }
    Process, exist, %EmacsPID%
    If %ErrorLevel%
    Sleep 50
    Else
    ExitApp
    ;; Transparent menus
    
    ;IfWinExist, ahk_class #32768
    ;  WinSet, Transparent, 220  ; Uses the window found by the above line.
  }


xt:
SetTitleMatchMode 3
WinShow ___EmacsPortableDaemon_%1%___
SetTitleMatchMode 2
WinShow \MinGW\bin\gdb.exe
WinShow \cygwin\bin\gdb.exe
ExitApp
Return

hidedaemon:
HideDaemon = 1
  Return
  
  showdaemon:
  HideDaemon = 0
  Return
  
  stopd:
  SetTitleMatchMode 3
  WinShow ___EmacsPortableDaemon_%1%___
  SetTitleMatchMode 2
  WinShow \mingw\bin\gdb.exe
  WinShow \MinGW\bin\gdb.exe
  WinShow \cygwin\bin\gdb.exe
  ExitDaemon = 1
  ExitApp
  Return
  
  newemacs:
  If FileExist("../../EmacsPortableApp.exe /VERSION %1%")
  Run "../../EmacsPortableApp.exe /VERSION %1%"
  Else
  Run %EPEXE%
  Return
  
  showdaemonexit:
  If ExitDaemon {
  
} Else {
  SetTitleMatchMode 1
    WinGet, ID, List, EmacsPortable
    Loop, %id%
    {
      StringTrimRight, this_id, id%a_index%, 0
        WinGetTitle, this_title, ahk_id %this_id%
        winclose,%this_title%
        }
  WinGet, ID, List, EmacsLocal
    Loop, %id%
    {
      StringTrimRight, this_id, id%a_index%, 0
        WinGetTitle, this_title, ahk_id %this_id%
        winclose,%this_title%
        }
  WinGet, ID, List, EmacsPortableDaemon
    Loop, %id%
    {
      StringTrimRight, this_id, id%a_index%, 0
        WinGetTitle, this_title, ahk_id %this_id%
        WinClose,%this_title%
        }
  SetTitleMatchMode 3
    WinShow, ___EmacsPortableDaemon_%1%___
    SetTitleMatchMode 2
    WinShow \mingw\bin\gdb.exe
    WinShow \MinGW\bin\gdb.exe
    WinShow \cygwin\bin\gdb.exe
    SetTitleMatchMode 1
    WinGet, ID, List, ___EmacsPortableDaemon_%1%___
    Loop, %id%
    {
      StringTrimRight, this_id, id%a_index%, 0
        WinGetTitle, this_title, ahk_id %this_id%
        WinClose,%this_title% 
        }
  WinClose, ___EmacsPortableDaemon_%1%___
    Process, WaitClose, %EmacsPID%, 10
    Run %EPRMREG%
    ;If ErrorLevel ; The PID still exists.
    ;  MsgBox "EmacsPortableApp did not close within 10 seconds, Exiting daemon"
    }
ExitApp
Return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EmacsPortableDaemon.ahk ends here

