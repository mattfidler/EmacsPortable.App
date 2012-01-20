' See http://www.justskins.com/forums/wsh-equivalent-of-server-38778.html
Function encodeit(str)
        ' Utility function to encode character entities
        str = Replace(str, "%", "%25")
        str = Replace(str, "/", "%2F")
        str = Replace(str, "|", "%7C")
        str = Replace(str, "?", "%3F")
        str = Replace(str, "!", "%21")
        str = Replace(str, "@", "%40")
        str = Replace(str, "\", "%5C")
        str = Replace(str, "#", "%23")
        str = Replace(str, "$", "%24")
        str = Replace(str, "^", "%5E")
        str = Replace(str, "&", "%26")
        str = Replace(str, "*", "%2A")
        str = Replace(str, "(", "%28")
        str = Replace(str, ")", "%29")
        str = Replace(str, "}", "%7D")
        str = Replace(str, ":", "%3A")
        str = Replace(str, ",", "%2C")
        str = Replace(str, "{", "%7B")
        str = Replace(str, "+", "%2B")
        str = Replace(str, ".", "%2E")
        str = Replace(str, "-", "%2D")
        str = Replace(str, "~", "%7E")
        str = Replace(str, "-", "%2D")
        str = Replace(str, "[", "%5B")
        str = Replace(str, "_", "%5F")
        str = Replace(str, "]", "%5D")
        str = Replace(str, "`", "%60")
        str = Replace(str, "=", "%3D")
        str = Replace(str, "'", "%27")
        str = Replace(str, " ", "%20")
        str = Replace(str, Chr(34), "%22")
        encodeit = str
End Function

If WScript.Arguments.Count >= 1 Then
        Set fso = CreateObject("Scripting.FileSystemObject")
        For Each arg in Wscript.Arguments
                If fso.FileExists(arg) Then
                        PROT = "org-protocol://store-link://" +_
                          encodeit("file://" + fso.GetAbsolutePathName(arg)) _
                             + "/" + escape(fso.GetFileName(arg))
                        CreateObject("Wscript.Shell").Run PROT, 0, False
                End If
        Next
End If
