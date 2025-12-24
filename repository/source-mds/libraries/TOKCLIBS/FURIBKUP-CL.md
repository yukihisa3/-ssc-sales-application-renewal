# FURIBKUP

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/FURIBKUP.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＨＧシステム                         *  ./
/. *   JOB-ID      :    FURIBKUP                             *  ./
/. *   JOB-NAME    :    振替データバックアップ               *  ./
/. ***********************************************************  ./
     PGM
     VAR  ?PGMEC ,INTEGER
     VAR  ?PGMECX,STRING*11
     VAR  ?PGMEM ,STRING*99
     VAR  ?MSG   ,STRING*99(6)
     VAR  ?MSGX  ,STRING*99
     VAR  ?PGMID ,STRING*8,VALUE-'PSE00101'
     VAR  ?STEP  ,STRING*8
     VAR  ?SYSDATE,STRING*13
     VAR  ?YY     ,STRING*4
     VAR  ?MM     ,STRING*4
     VAR  ?DD     ,STRING*4
     VAR  ?DATE   ,STRING*6
     VAR  ?FILNM  ,STRING*8,VALUE-'        '
     VAR  ?LIBNM  ,STRING*8,VALUE-'FURIBKUP'
     VAR  ?FILID  ,NAME
     VAR  ?LIBID  ,NAME
     VAR  ?FURIBK ,NAME!MOD
     VAR  ?FURIBKN,STRING*17

     SNDMSG MSG-'##振替ＤＴﾊﾞｯｸｱｯﾌﾟ開始##',TO-XCTL

STEP010: /.##システム日付取得##./

    ?STEP :=  'STEP010 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'##    ｼｽﾃﾑ日付取得    ##',TO-XCTL

    ?SYSDATE  :=    @SCDATED
    ?YY       :=    %SBSTR(?SYSDATE,5,2)
    ?MM       :=    %SBSTR(?SYSDATE,3,2)
    ?DD       :=    %SBSTR(?SYSDATE,1,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?DATE     :=    ?YY && ?MM && ?DD
    ?MSGX  :=  '## 本日日付 = ' && ?DATE && ' ##'
    SNDMSG    ?MSGX,TO-XCTL

STEP020: /.##バックアップＦ名生成##./

    ?STEP :=  'STEP020 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'##   ﾊﾞｯｸｱｯﾌﾟ名取得   ##',TO-XCTL

    ?FILNM    :=    'F1' && ?DATE
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?FURIBK   :=    %NCAT(?FILID,?LIBID)
    ?FURIBKN  :=    %STRING(?FURIBK)
    ?MSGX  :=  '## ﾊﾞｯｸｱｯﾌﾟF名1 = ' && ?FURIBKN  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

STEP030: /.##バックアップ開始##./

    ?STEP :=  'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'##   ﾊﾞｯｸｱｯﾌﾟ開始 1   ##',TO-XCTL

    CPYFILE FILE-TOKFRR.TOKFLIB,TOFILE-?FURIBK,CRTFILE-@YES
    IF        @PGMEC    ^=   0   THEN
              IF  @PGMEC =   542 THEN
                  GOTO STEP040
              ELSE
                  GOTO ABEND
              END
    ELSE
              GOTO RTN
    END

STEP040: /.##ﾊﾞｯｸｱｯﾌﾟ開始(重複あり２)##./

    ?STEP :=  'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'##   ﾊﾞｯｸｱｯﾌﾟ開始 2   ##',TO-XCTL

    ?FILNM    :=    'F2' && ?DATE
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?FURIBK   :=    %NCAT(?FILID,?LIBID)
    ?FURIBKN  :=    %STRING(?FURIBK)
    ?MSGX     :=    '## ﾊﾞｯｸｱｯﾌﾟF名2 = ' && ?FURIBKN  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CPYFILE FILE-TOKFRR.TOKFLIB,TOFILE-?FURIBK,CRTFILE-@YES
    IF        @PGMEC    ^=   0   THEN
              IF  @PGMEC =   542 THEN
                  GOTO STEP050
              ELSE
                  GOTO ABEND
              END
    ELSE
              GOTO RTN
    END

STEP050: /.##ﾊﾞｯｸｱｯﾌﾟ開始(重複あり３)##./

    ?STEP :=  'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'##   ﾊﾞｯｸｱｯﾌﾟ開始 3   ##',TO-XCTL

    ?FILNM    :=    'F3' && ?DATE
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?FURIBK   :=    %NCAT(?FILID,?LIBID)
    ?FURIBKN  :=    %STRING(?FURIBK)
    ?MSGX     :=    '## ﾊﾞｯｸｱｯﾌﾟF名3 = ' && ?FURIBKN  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CPYFILE FILE-TOKFRR.TOKFLIB,TOFILE-?FURIBK,CRTFILE-@YES
    IF        @PGMEC    ^=   0   THEN
              ?MSGX := '## ﾊﾞｯｸｱｯﾌﾟ処理に失敗]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX := '## ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名にｺﾋﾟｰ]] ##'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX := '## ﾌｧｲﾙ名=TOKFRR.FURIBKUP#'
              SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              CNVFILE FILE-TOKFRR.TOKFLIB,
                      TOFILE-TOKFRR.FURIBKUP,ADD-@NO,BF-1
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
/.*****  ＴＥＸＴ　ＥＮＤ  ****************************************./

```
