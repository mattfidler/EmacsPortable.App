mkdir usr/local/lib
VER=`ls usr/local/bin/emacs-* | sed 's/.*emacs-\(.*\)/\1/'`
for lib in `ldd usr/local/bin/emacs-* | sed 's/.*=> *\(.*\) (.*)/\1/g'  | sed 's/.* *(.*) *//' | sed '/^$/d'`; do
    cp -v ${lib} usr/local/lib
done

cd usr/local
zip -9 -r ../../emacs-linux-${VER}.zip bin lib libexec 
