;;; build-plist.el --- Builds plist and other things needed for Apple
;; 
;; Filename: build-plist.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Tue Apr 10 15:25:37 2012 (-0500)
;; Version: 
;; Last-Updated: Wed Apr 11 14:38:09 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 39
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   Cannot open load file: build-plist.
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

(defvar emacs-portable-release-num "0")

(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(require 'cl)

(unless (fboundp 'remove-from-list)
  (defalias 'remove-from-list 'remove))

(setq usb-app-dir (expand-file-name (concat (file-name-directory (or
                                                                  load-file-name
                                                                  buffer-file-name)) "../")))
(defun build-app-info ()
  "Builds Mac Contents and Portable Apps AppInfo Directory"
  (interactive)
  (let ((ret "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
  <dict>
    <key>CFBundleDevelopmentRegion</key>
    <string>English</string>
    <key>CFBundleDocumentTypes</key>
    <array>")
        ext name)
    ;; Create Appinfo.ini
    (unless (file-exists-p (concat usb-app-dir "AppInfo"))
      (make-directory (concat usb-app-dir "AppInfo") t))
    
    (with-temp-file (concat usb-app-dir "AppInfo/appinfo.ini")
      (insert "[Format]\n")
      (insert "Type=PortableApps.comFormat\n")
      (insert "Version=3.0\n\n")
      
      (insert "[Details]\n")
      (insert "Name=Emacs Portable\n")
      (insert "AppID=EmacsPortable.App\n")
      (insert "Publisher=Matthew Fidler & Free Software Foundation & PortableApps.com\n")
      (insert "Homepage=http://github.com/mlf176f2/EmacsPortable.App\n")
      (insert "Category=Development\n")
      (insert "Description=Emacs Portable is is an extensible, customizable text editor—and more.\n")
      (insert "Language=Multilingual\n\n")
      
      (insert "[License]\n")
      (insert "Shareable=true\n")
      (insert "OpenSource=true\n")
      (insert "Freeware=true\n")
      (insert "CommercialUse=true\n")
      (insert "EULAVersion=3\n\n")
      
      (insert "[Version]\n")
      (insert "PackageVersion=")
      (let (ver)
        (with-temp-buffer
          (insert emacs-version)
          (insert ".")
          (goto-char (point-min))
          (setq ver (concat
                     (if (not (re-search-forward "\\=[0-9]+[.]" nil t))
                         "0.0.0."
                       (if (not (re-search-forward "\\=[0-9]+[.]" nil t))
                           (concat (buffer-substring (point-min) (point)) "0.0.")
                         (if (not (re-search-forward "\\=[0-9]+[.]" nil t))
                             (concat (buffer-substring (point-min) (point)) "0.")
                           (concat (buffer-substring (point-min) (point))))))
                     emacs-portable-release-num)))
        (insert ver)
        (insert "\nDisplayVersion=")
        (when (string-match "[.]\\([0-9]+\\)$" ver)
          (setq ver (replace-match " Launcher \\1" nil nil ver)))
        (insert ver)
        (insert "\n\n"))
      
      (insert "[SpecialPaths]\n")
      (insert "Plugins=NONE\n\n")
      (insert "[Dependencies]\n")
      (insert "UsesJava=optional\n\n")
      
      (insert "[Control]\n")
      (let ((nicons 2)
            (lst (append
                  '(
                    ("EmacsDos.exe" "Emacs Portable DOS mode")
                    ("Emacs-Q.exe" "Emacs Portable -Q")
                    ("EmacsDebug.exe" "Emacs Portable Debugging Startup"))
                  (mapcar (lambda(x)
                            (list x (concat "Emacs Portable "
                                            (replace-regexp-in-string "EmacsPortableApp-\\([0-9.]+\\)[.]exe" "\\1" x))))
                          (directory-files (concat usb-app-dir "../") nil "EmacsPortableApp-[0-9.]+[.]exe")))))
        (insert "Start=EmacsPortableApp.exe\n")
        (insert "Start1=EmacsPortableApp.exe\n")
        (insert "Name1=Emacs Portable")
        (insert "Start2=EmacsOptions.exe\n")
        (insert "Name2=Emacs Portable Options\n")
        (insert
         (mapconcat
          (lambda(x)
            (message "%s" (nth 0 x))
            (when (file-exists-p (concat usb-app-dir "../" (nth 0 x)))
              (setq nicons (+ nicons 1))
              (format "Start%s=%s\nName%s=%s\n" nicons (nth 0 x) nicons (nth 1 x))))
          lst ""))
        (save-excursion
          (when (re-search-backward "\\[Control\\]" nil t)
            (end-of-line)
            (insert (format"\nIcons=%s" nicons)))))
      (insert "[Associations]\n")
      (insert "SendTo=true\n")
      (let ((ft ""))
        (with-temp-buffer
          (insert-file-contents (concat usb-app-dir "ini/assoc.ini"))
          (goto-char (point-min))
          (when (re-search-forward "\\[assocs\\]" nil t)
            (replace-match ""))
          (delete-region (point-min) (point))
          (when (re-search-forward "\\[.*?\\]" nil t)
            (delete-region (point-at-bol) (point-max)))
          (goto-char (point-min))
          (while (re-search-forward "=\\(.*?\\)" nil t)
            (setq ft (concat ft "," (match-string 1)))))
        (insert "FileTypes=html,htm,xhtml,xhtm,xht,shtml\n"))
      (insert "Protocols=mailto,org-protocol\n"))

    (unless (file-exists-p (concat usb-app-dir "AppInfo/appicon.ico"))
      (when (file-exists-p (concat usb-app-dir "../Other/source/img/ico/appicon.ico"))
        (copy-file (concat usb-app-dir "../Other/source/img/ico/appicon.ico")
                   (concat usb-app-dir "AppInfo/appicon.ico"))))

    (unless (file-exists-p (concat usb-app-dir "AppInfo/appicon_16.png"))
      (when (file-exists-p (concat usb-app-dir "../Other/source/img/appicon_16.png"))
        (copy-file (concat usb-app-dir "../Other/source/img/appicon_16.png")
                   (concat usb-app-dir "AppInfo/appicon_16.png"))))

    (unless (file-exists-p (concat usb-app-dir "AppInfo/appicon_32.png"))
      (when (file-exists-p (concat usb-app-dir "../Other/source/img/appicon_32.png"))
        (copy-file (concat usb-app-dir "../Other/source/img/appicon_32.png")
                   (concat usb-app-dir "AppInfo/appicon_32.png"))))

    (unless (file-exists-p (concat usb-app-dir "AppInfo/appicon_128.png"))
      (when (file-exists-p (concat usb-app-dir "../Other/source/img/appicon_128.png"))
        (copy-file (concat usb-app-dir "../Other/source/img/appicon_128.png")
                   (concat usb-app-dir "AppInfo/appicon_128.png"))))
    
    (with-temp-buffer
      (insert-file-contents (concat usb-app-dir "ini/assoc.ini"))
      (goto-char (point-min))
      (when (re-search-forward "\\[assocs\\]" nil t)
        (replace-match ""))
      (delete-region (point-min) (point))
      (when (re-search-forward "\\[.*?\\]" nil t)
        (delete-region (point-at-bol) (point-max)))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\([^=\n]*?\\)[ \t]*=[ \t]*\\(.*?\\)[ \t]*$" nil t)
        (setq name (match-string 1))
        (setq ext (match-string 2))
        (setq ret
              (concat ret "<dict>\n<key>CFBundleTypeExtensions</key>\n<array>\n"
                      (mapconcat
                       (lambda(x)
                         (concat "<string>" x "</string>"))
                       (split-string ext ",") "\n")
                      "\n</array>
                      <key>CFBundleTypeIconFile</key>
                      <string>document.icns</string>
                      <key>CFBundleTypeName</key>
                      <string>"
                      name "</string>\n"
                      (cond
                       ((string-match "[Pp]lain text" name)
                        "<key>CFBundleTypeOSTypes</key>
                        <array>
                        <string>TEXT</string>
                        <string>utxt</string>
                        </array>")
                       ((string-match "\\<HTML" name)
                        "<key>CFBundleTypeOSTypes</key>
                        <array>
                        <string>HTML</string>
                        </array>")
                       
                       (t ""))
                      "\n<key>CFBundleTypeRole</key>\n<string>Editor</string>\n</dict>\n" ))))
    (setq ret
          (concat ret
                  "<dict>
                  <key>CFBundleTypeExtensions</key>
                  <array>
                  <string>*</string>
                  </array>
                  <key>CFBundleTypeName</key>
                  <string>All</string>
                  <key>CFBundleTypeOSTypes</key>
                  <array>
                  <string>****</string>
                  </array>
                  <key>CFBundleTypeRole</key>
                  <string>Viewer</string>
                  </dict>
                  </array>
                  <key>NSServices</key>
                  <array>
                  <dict>
                  <key>NSMenuItem</key>
                  <dict>
                  <key>default</key>
                  <string>EmacsPortable.app/New Buffer Containing Selection</string>
                  </dict>
                  <key>NSMessage</key>
                  <string>requestService</string>
                  <key>NSUserData</key>
                  <string>open-selection</string>
                  <key>NSPortName</key>
                  <string>Emacs</string>
                  <key>NSSendTypes</key>
                  <array>
                  <string>NSStringPboardType</string>
                  </array>
                  </dict>
                  <dict>
                  <key>NSMenuItem</key>
                  <dict>
                  <key>default</key>
                  <string>EmacsPortable.app/Open Selected File</string>
                  </dict>
                  <key>NSMessage</key>
                  <string>requestService</string>
                  <key>NSUserData</key>
                  <string>open-file</string>
                  <key>NSPortName</key>
                  <string>Emacs</string>
                  <key>NSSendTypes</key>
                  <array>
                  <string>NSStringPboardType</string>
                  </array>
                  </dict>
                  <dict>
                  <key>NSMenuItem</key>
                  <dict>
                  <key>default</key>
                  <string>EmacsPortable.app/Email Selection</string>
                  </dict>
                  <key>NSMessage</key>
                  <string>requestService</string>
                  <key>NSUserData</key>
                  <string>mail-selection</string>
                  <key>NSPortName</key>
                  <string>Emacs</string>
                  <key>NSSendTypes</key>
                  <array>
                  <string>NSStringPboardType</string>
                  </array>
                  </dict>
                  <dict>
                  <key>NSMenuItem</key>
                  <dict>
                  <key>default</key>
                  <string>EmacsPortable.app/Send Email to Selected Address</string>
                  </dict>
                  <key>NSMessage</key>
                  <string>requestService</string>
                  <key>NSUserData</key>
                  <string>mail-to</string>
                  <key>NSPortName</key>
                  <string>Emacs</string>
                  <key>NSSendTypes</key>
                  <array>
                  <string>NSStringPboardType</string>
                  </array>
                  </dict>
                  </array>
                  
                  <key>CFBundleExecutable</key>
                  <string>EmacsPortable</string>
                  <key>CFBundleGetInfoString</key>
                  <string>EmacsPortable " emacs-version " Copyright (C) " (format-time-string "%Y") " Matthew Fidler &amp; Free Software Foundation, Inc.</string>
                  <key>CFBundleIconFile</key>
                  <string>Emacs.icns</string>
                  <key>CFBundleIdentifier</key>
                  <string>org.gnu.Emacs</string>
                  <key>CFBundleInfoDictionaryVersion</key>
                  <string>6.0</string>
                  <key>CFBundleName</key>
                  <string>EmacsPortable</string>
                  <key>CFBundlePackageType</key>
                  <string>APPL</string>
                  <key>CFBundleShortVersionString</key>
                  <string>" emacs-version "</string>
                  <key>CFBundleSignature</key>
                  <string>EMAx</string>
                  <key>CFBundleVersion</key>
                  <string>1.0</string>
                  <key>NSPrincipalClass</key>
                  <string>EmacsApp</string>
                  <key>CFBundleURLTypes</key>
                  <array>
                  <dict>
                  <key>CFBundleURLName</key>
                  <string>Email Address URL</string>
                  <key>CFBundleURLSchemes</key>
                  <array>
                  <string>mailto</string>
                  </array>
                  </dict>
                  </array>
                  </dict>
                  </plist>
                  "))
    (unless (file-exists-p (concat usb-app-dir "../Contents/Resources/English.lproj"))
      (make-directory  (concat usb-app-dir "../Contents/Resources/English.lproj") t))
    (with-temp-file (concat usb-app-dir "../Contents/Info.plist")
      (insert ret)
      (nxml-mode)
      (indent-region (point-min) (point-max)))
    (with-temp-file (concat usb-app-dir "../Contents/Resources/English.lproj/InfoPlist.strings")
      (insert "CFBundleName = \"EmacsPortable.App\";\nCFBundleShortVersionString = \"Version "
              emacs-version "\";\n CFBundleGetInfoString = \"EmacsPortable.App version " emacs-version
              ", NS Windowing\";\n NSHumanReadableCopyright = \"Copyright (C) " (format-time-string "%Y")
              "Matthew Fidler &amp; Free Software Foundation, Inc.\";\n"))
    (with-temp-file (concat usb-app-dir "../Contents/PkgInfo")
      (insert "APPLEMAx"))
    (unless (file-exists-p (concat usb-app-dir "../Contents/Resources/Emacs.icns"))
      (when (file-exists-p (concat usb-app-dir "../Other/source/img/icns/Emacs.icns"))
        (copy-file (concat usb-app-dir "../Other/source/img/icns/Emacs.icns")
                   (concat usb-app-dir "../Contents/Resources/Emacs.icns"))))
    (unless (file-exists-p (concat usb-app-dir "../Contents/Resources/document.icns"))
      (when (file-exists-p (concat usb-app-dir "../Other/source/img/icns/document.icns"))
        (copy-file (concat usb-app-dir "../Other/source/img/icns/document.icns")
                   (concat usb-app-dir "../Contents/Resources/document.icns"))))
    (unless (file-exists-p (concat usb-app-dir "../Contents/MacOS"))
      (make-directory  (concat usb-app-dir "../Contents/MacOS") t))
    (unless (file-exists-p (concat usb-app-dir "../Contents/MacOS/EmacsPortable"))
      (when (file-exists-p (concat usb-app-dir "MacOS/EmacsPortable"))
        (copy-file (concat usb-app-dir "MacOS/EmacsPortable")
                   (concat usb-app-dir "../Contents/MacOS/EmacsPortable"))))
    (unless (file-exists-p (concat usb-app-dir "../Contents/MacOS/flip.osx"))
      (when (file-exists-p (concat usb-app-dir "MacOS/flip.osx"))
        (copy-file (concat usb-app-dir "MacOS/flip.osx")
                   (concat usb-app-dir "../Contents/MacOS/flip.osx"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; build-plist.el ends here
