# PTBLCNV

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PTBLCNV.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    変換テーブルコンバート               *  ./
/. *   JOB-ID      :    PTBLCNV                              *  ./
/. *   JOB-NAME    :    変換テーブルコンバート               *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER
    VAR ?PGMECX   ,STRING*11
    VAR ?PGMEM    ,STRING*99
    VAR ?MSG      ,STRING*99(6)
    VAR ?MSGX     ,STRING*99
    VAR ?PGMID    ,STRING*8,VALUE-'PTBLCNV'
    VAR ?STEP     ,STRING*8
    VAR ?TTORICD  ,STRING*8,VALUE-'00321717'
    VAR ?HTORICD  ,STRING*8,VALUE-'00000007'
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##変換ﾃｰﾌﾞﾙ出力##./
CVSHOTBL:

    ?STEP :=   'CVSHOTBL'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHOTBLWK,TOFILE-SHOTBLWK.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-CVSHOTBL.TOKELIB,PARA-(?TTORICD,?HTORICD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##変換ﾃｰﾌﾞﾙｾｯﾄ##./
STSHOTBL:

    ?STEP :=   'STSHOTBL'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHOTBLWK,TOFILE-SHOTBLWK.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-STSHOTBL.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC

```
