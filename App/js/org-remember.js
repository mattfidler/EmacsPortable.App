// from http://article.gmane.org/gmane.emacs.orgmode/6810
app.addMenuItem({cName:"org-remember", cParent:"Tools",
                 cExec:"app.launchURL('org-protocol://remember://' + encodeURIComponent(this.URL) + '/' + encodeURIComponent(this.info.Title) + '/');"});
