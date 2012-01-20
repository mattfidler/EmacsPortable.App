    !ifmacrondef _BlowFish
        !define BlowFish_Decrypt `!insertmacro _BlowFish Decrypt`
        !define BlowFish_Encrypt `!insertmacro _BlowFish Encrypt`
        !macro _BlowFish _cmd _retVar _Data _Key
            !if `${_retVar}` != `$8`
                Push $8
            !endif      
            BlowFish::${_cmd} `${_Data}` `${_Key}`
            !if `${_retVar}` != `$8`
                Exch $8
                !if `${_retVar}` != ``
                    Pop ${_retVar}
                !endif
            !endif
        !macroend
    !endif
 