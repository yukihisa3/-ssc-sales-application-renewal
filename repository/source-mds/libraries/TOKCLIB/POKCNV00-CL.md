# POKCNV00

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/POKCNV00.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    POKCNV00                             *  ./
/. *   JOB-NAME    :    倉庫コード変換　　　　　             *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'POKCNV00'
    VAR ?STEP     ,STRING*8
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##取引先マスタ変換##./
SOKCNV01:

    ?STEP :=   'SOKCNV01'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HTOKMS,TOFILE-HTOKMS.TOKFLIB
    CALL      PGM-SOKCNV01.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##商品変換テーブル変換##./
SOKCNV02:

    ?STEP :=   'SOKCNV02'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SOKCNV02.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##ルート条件マスタ変換##./
SOKCNV03:

    ?STEP :=   'SOKCNV03'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    CALL      PGM-SOKCNV03.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##売上伝票ファイル変換##./
SOKCNV04:

    ?STEP :=   'SOKCNV04'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    CALL      PGM-SOKCNV04.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##発注ヘッダファイル変換##./
SOKCNV05:

    ?STEP :=   'SOKCNV05'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HACHEDL1,TOFILE-HACHEDL1.TOKFLIB
    CALL      PGM-SOKCNV05.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##入庫ファイル変換##./
SOKCNV06:

    ?STEP :=   'SOKCNV06'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NYKFILL1,TOFILE-NYKFILL1.TOKFLIB
    CALL      PGM-SOKCNV06.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##入出庫ファイル変換##./
SOKCNV07:

    ?STEP :=   'SOKCNV07'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NYSFILL1,TOFILE-NYSFILL1.TOKFLIB
    CALL      PGM-SOKCNV07.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##作業実績ファイル変換##./
SOKCNV08:

    ?STEP :=   'SOKCNV08'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SGYFILL1,TOFILE-SGYFILL1.TOKFLIB
    CALL      PGM-SOKCNV08.TOKELIB
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
