;;; EmacsShortcuts.el --- Code to make basic NSIS shortcuts
;; 
;; Filename: EmacsShortcuts.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Jan 20 08:11:13 2012 (-0600)
;; Version: 
;; Last-Updated: Fri Jan 20 08:14:13 2012 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 4
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

(defun ep-shortcut (output icon switch)
  "Creates an EmacsPortable.App shortcut"
 (with-temp-file (concat "./" output ".nsi")
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

Section 
  Exec '\"$EXEDIR\\EmacsPortableApp.exe\" %s'
SectionEnd" icon output switch)))
 (concat "./" output ".nsi"))

(ep-shortcut "EmacsDos" "Gnome-terminal-non-nuvola" "/DOS")
(ep-shortcut "Emacs-Q" "kbugbuster" "/Q")
(ep-shortcut "EmacsDebug" "kbugbuster" "/DEBUG")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EmacsShortcuts.el ends here
