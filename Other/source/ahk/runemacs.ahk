; From http://www.autohotkey.com/forum/topic7897.html

GetEnv( p_name, p_mode="" )
{
  static   table, fields
    
    if ( p_mode = "clear" )
      {
        table = |
          fields = |
   
          pTable := DllCall( "GetEnvironmentStrings" )
          if ( ErrorLevel or !pTable )
            {
              MsgBox, [GetEnv: GetEnvironmentStrings] failed: EL = %ErrorLevel%
              return
            }
      
        index = 1
          offset = 0
          loop,
          {
          len := ReadString( record, pTable, offset, 32767 )
          
          if ( len = 0 )
            break
            else if ( Asc( record ) != 61 )
              {
                StringSplit, field, record, =
         
                DllCall( "SetEnvironmentVariable", "str", field1, "uint", 0 )
         
                table := table record
                fields := fields "|" index "," StrLen( field1 ) "," StrLen( field2 )
            
                index += len
              }

          offset += len+1
          }
      
        StringTrimLeft, table, table, 1
          StringTrimLeft, fields, fields, 2

          DllCall( "FreeEnvironmentStrings", "uint", pTable )
          
          return
          }
    else if ( p_mode = "restore" )
      {
        loop, parse, fields, |
          {
            StringSplit, field, A_LoopField, `,
            
              StringMid, name, table, field1, field2
              StringMid, value, table, field1+field2+1, field3
            
              EnvSet, %name%, %value%
              }
      
        return
          }
   
 old_StringCaseSense := A_StringCaseSense
   StringCaseSense, off
   
   loop, parse, fields, |
   {
     StringSplit, field, A_LoopField, `,
     
     StringMid, name, table, field1, field2
     if ( name = p_name )
       {
         StringMid, value, table, field1+field2+1, field3
         break
       }
   }

 StringCaseSense, %old_StringCaseSense%
   
   return, value
   }

ReadString( byref r_string, p_address, p_offset, p_size )
{
  r_string = |
   
    p_address += p_offset

    loop, %p_size%
    {
      if ( *p_address = 0 )
        break
          
          r_string := r_string Chr( *p_address )
          
          p_address++
          }
  
  StringTrimLeft, r_string, r_string, 1
    
    return, StrLen( r_string )
    }

exe = 
  
Loop %0% {
  parm := %A_Index%
 exe = %exe% %parm%
}
Run, %exe%,GetEnv("HOME") , Hide
  
  
  
  
  
  
  
  
  
    
  
