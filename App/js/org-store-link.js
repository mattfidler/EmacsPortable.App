// from http://article.gmane.org/gmane.emacs.orgmode/6810
app.addMenuItem({cName:"org-store-link", cParent:"Tools",
                 cExec:"app.launchURL('org-protocol://store-link://' + encodeURIComponent(this.URL) + '/' + encodeURIComponent(this.info.Title));"});
