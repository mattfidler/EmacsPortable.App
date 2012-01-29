#!/bin/sh
#cd `basename $0`
git clone git@github.com:mlf176f2/emacs-portable-starter-kit.git
mv emacs-portable-starter-kit ../portable-starter-kit
git clone git@github.com:mlf176f2/my-emacs-startup.git
rm -rf ../../Data/init/shared
mv my-emacs-startup ../../Data/init/shared

git clone git@github.com:mlf176f2/Emacs-Speaks-NONMEM.git
mv Emacs-Speaks-NONMEM ../../Data/src/

git clone git@github.com:mlf176f2/auto-indent-mode.el.git
mv auto-indent-mode.el ../../Data/src/

git clone git@github.com:mlf176f2/nsis-mode.git
mv nsis-mode ../../Data/src/


git clone git@github.com:mlf176f2/org-outlook.el.git
mv org-outlook.el ../../Data/src/

git clone git@github.com:mlf176f2/guess-tex-master.el.git
mv guess-tex-master.el ../../Data/src/

git clone git@github.com:mlf176f2/yas-jit.el.git
mv yas-jit.el ../../Data/src/

git clone git@github.com:mlf176f2/org-cua-dwim.el.git
mv org-cua-dwim.el ../../Data/src/

git clone git@github.com:mlf176f2/r-autoyas.el.git
mv r-autoyas.el ../../Data/src/

git clone git@github.com:mlf176f2/fold-dwim-org.git
mv fold-dwim-org ../../Data/src/

git clone git@github.com:mlf176f2/ac-R.git
mv ac-R ../../Data/src/

git clone git@github.com:mlf176f2/el-autoyas.el.git
mv el-autoyas.el ../../Data/src/

git clone git@github.com:mlf176f2/textmate-to-yas.el.git
mv textmate-to-yas.el ../../Data/src/

git clone git@github.com:mlf176f2/ess-smart-underscore.el.git
mv ess-smart-underscore.el ../../Data/src/

git clone git@github.com:mlf176f2/org-table-comment.el.git
mv org-table-comment.el ../../Data/src/
