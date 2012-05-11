;;; EmacsShortcuts.el --- Code to make basic NSIS shortcuts
;; 
;; Filename: EmacsShortcuts.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Jan 20 08:11:13 2012 (-0600)
;; Version: 
;; Last-Updated: Fri May 11 15:56:42 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 19
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   Cannot open load file: EmacsShortcuts.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
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

(defun ep-shortcut (output icon switch &optional name)
  "Creates an EmacsPortable.App shortcut"
  (with-temp-file (concat "./" (or name output) ".nsi")
    (insert (format "SetCompress Auto
SetCompressor /SOLID lzma
SetCompressorDictSize 32
SetDatablockOptimize On
Icon \"..\\img\\ico\\%s.ico\"
OutFile \"..\\..\\..\\%s.exe\"

CRCCheck On
WindowIcon Off
SilentInstall Silent
AutoCloseWindow True
RequestExecutionLevel user
!include \"FileFunc.nsh\"

Section
  Var /GLOBAL cmdLineParams
  ${GetParameters} $cmdLineParams
  IfFileExists \"$EXEDIR\\EmacsPortableApp.exe\" same_dir different_dir
  same_dir:
    Exec '\"$EXEDIR\\EmacsPortableApp.exe\" %s  $cmdLineParams'
    Goto end
  different_dir:
    IfFileExists \"$EXEDIR\\..\\..\\EmacsPortableApp.exe\" 0 end
    GetFullPathName $R0 \"$EXEDIR\\..\\..\"
    SetOutPath $R0
    Exec '\"$R0\\EmacsPortableApp.exe\" %s  $cmdLineParams'
  end:
    Clearerrors
SectionEnd" icon output switch switch)))
  (concat "./" output ".nsi"))

(ep-shortcut "EmacsDos" "Gnome-terminal-non-nuvola" "/DOS")
(ep-shortcut "Emacs-Q" "bug" "/Q")
(ep-shortcut "EmacsDebug" "bug" "/DEBUG")
(ep-shortcut "App\\eps\\EmacsDoc" "..\\..\\..\\..\\App\\document" "" "EmacsDoc")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EmacsShortcuts.el ends here
