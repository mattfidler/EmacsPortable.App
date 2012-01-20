// from http://article.gmane.org/gmane.emacs.orgmode/6810
app.addMenuItem({cName:"org-capture", cParent:"Tools",
                 cExec:"app.launchURL('org-protocol://capture://' + encodeURIComponent(this.URL) + '/' + encodeURIComponent(this.info.Title) + '/');"});
