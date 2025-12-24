# PHGLOGIN

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PHGLOGIN.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＩＴ統制　　                         *  ./
/. *   JOB-ID      :    PIT00100                             *  ./
/. *   JOB-NAME    :    ログイン画面　　　　　               *  ./
/. *   UPDATE      :    2021.05.10  フラワーロジテム対応     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PHGLOGIN'
    VAR       ?STEP     ,STRING*8
    VAR       ?USERID   ,STRING*8,VALUE-'        ' /.ﾕｰｻﾞｰID./
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門名./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者CD./
    VAR       ?ERRST    ,STRING*2,VALUE-'  '       /.ｴﾗｰCD./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WS6      ,STRING*6,VALUE-'      '
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?P01      ,STRING*6,VALUE-'      '   /.担当者ﾏｽﾀﾊﾟﾗ./
    VAR       ?PROFCD   ,NAME
    VAR       ?PROFNM   ,STRING*8,VALUE-'        '
    VAR       ?PROFNM4  ,STRING*4,VALUE-'    '

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKKLIB/TOKDLIB/TOKELIBO/ZAIEXCEL


/.## ﾌﾟﾛﾌｨｰﾙ名取得 ##./
    ?PROFCD  :=  @JOBPROF
/.## ﾌﾟﾛﾌｨｰﾙ名名前変換 ##./
    ?PROFNM  :=  %STRING(?PROFCD)
    /.TEST   ?PROFNM  :=  'FLKN0001'  ./
    ?PROFNM4 :=  %SBSTR(?PROFNM,1,4)
/.## ﾌﾟﾛﾌｨｰﾙ名表示 ##./
    ?MSGX    :=  '## ﾌﾟﾛﾌｨｰﾙ名:'  &&  ?PROFNM
                 &&  '(' && ?PROFNM4 && ')' && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES


/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    /.TEST   ?WS      :=  'WKSTNF01'  ./
    ?WS6     :=  %SBSTR(?WS,1,6)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' &&  ?WS
                 &&  '(' && ?WS6 && ')' && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.## WS PROF 判定 ##./
    IF ?WS6 = 'WKSTNF' THEN
       IF   ?PROFNM4 ^= 'FLKN' THEN
            MENU LOGINER1.TOKMLIBO
            GOTO ABEND
       END
    END
    IF ?WS6 ^= 'WKSTNF' THEN
       IF   ?PROFNM4  = 'FLKN' THEN
            MENU LOGINER2.TOKMLIBO
            GOTO ABEND
       END
    END

/.## ｽﾌﾟｰﾙﾌｧｲﾙ名変更(WS名に変更) ##./
/.  CHGCMVAR '@OUTDSN',?WKSTN  ./

/.##ログインユーザーファイル作成##./
CRTLOGON:

    ?STEP :=   'CRTLOGON'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DLTFILE LOGINUSR.@TEMP
    CRTFILE FILE-LOGINUSR.@TEMP,SIZE-10,ESIZE-10,ORG-@SF,
            USED-@YES,RL-20
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

/.##ログイン画面##./
HGLOGON:

    ?STEP :=   'HGLOGON '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF     FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF     FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF     FILE-JHMLOGL1,TOFILE-JHMLOGL1.TOKKLIB
    OVRF     FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL PGM-HGLOGIN.TOKELIBO ,PARA-(?USERID,?BUMON,?TANCD,?ERRST)
    IF @PGMEC    ^=   0    THEN
       GOTO ABEND END
    ?MSGX := 'STATUS = ' && ?ERRST
    SNDMSG    ?MSGX,TO-XCTL
    IF ?ERRST  =  '01'  THEN
       ?MSGX := 'LOGIN-ERR 強制終了 WKSTN:' && ?WS && ' ID:' &&
                 ?USERID && ' 担CD:' && ?TANCD
        SNDMSG ?MSGX,TO-XCTL
        LOGMSG ?MSGX,JLOG-@YES,SLOG-@YES
        MENU ERRLOGON.TOKMLIB
    END
    IF ?ERRST  =  '02'  THEN
       ?MSGX := 'LOGIN-ERR *ﾛｯｸｱｳﾄ* WKSTN:' && ?WS && ' ID:' &&
                 ?USERID && ' 担CD:' && ?TANCD
        SNDMSG ?MSGX,TO-XCTL
        LOGMSG ?MSGX,JLOG-@YES,SLOG-@YES
        MENU ERRLOGON.TOKMLIB
    END
    IF ?ERRST  =  '03'  THEN
       ?MSGX := 'LOGIN-ERR 期間ｴﾗｰ  WKSTN:' && ?WS && ' ID:' &&
                 ?USERID && ' 担CD:' && ?TANCD
        SNDMSG ?MSGX,TO-XCTL
        LOGMSG ?MSGX,JLOG-@YES,SLOG-@YES
        MENU ERRLOGON.TOKMLIB
    END
    IF ?ERRST  =  '00'  THEN
       ?MSGX := 'LOGIN-OK  【正常】 WKSTN:' && ?WS && ' ID:' &&
                 ?USERID && ' 担CD:' && ?TANCD
        SNDMSG ?MSGX,TO-XCTL
        LOGMSG ?MSGX,JLOG-@YES,SLOG-@YES
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
    IF ?ERRST  =  '04'  THEN
       ?MSGX := 'ﾛｸﾞｲﾝ画面でｴﾗｰ発生です。'
       SNDMSG ?MSGX,TO-XCTL
       LOGMSG ?MSGX,JLOG-@YES,SLOG-@YES
       ?MSGX := 'LOGON-FILE-ERR FILE-ST = ' && %SBSTR(?PGMECX,8,4)
       SNDMSG ?MSGX,TO-XCTL
       LOGMSG ?MSGX,JLOG-@YES,SLOG-@YES
       MENU ERRLOGON.TOKMLIB
    END

    RETURN    PGMEC-@PGMEC

```
