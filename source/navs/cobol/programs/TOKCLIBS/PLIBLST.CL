/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＩＴ統制　　　　　　　　　           *  ./
/. *   JOB-ID      :    PLIBLST                              *  ./
/. *   JOB-NAME    :    ライブラリリスト出力                 *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM

/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PLIBLST'
    VAR       ?STEP   ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
    VAR       ?WS     ,STRING*8,VALUE-'        '  /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN  ,NAME!MOD                   /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PRF    ,STRING*8,VALUE-'        '  /.ﾌﾟﾛﾌｨｰﾙ文字./
    VAR       ?PROFL  ,NAME!MOD                   /.ﾌﾟﾛﾌｨｰﾙ名前./
    VAR       ?SYSDATE,STRING*13
    VAR       ?DATE   ,STRING*6
    VAR       ?SPLNM  ,NAME
    VAR       ?SPL    ,STRING*8
    VAR       ?YY     ,STRING*2
    VAR       ?MM     ,STRING*2
    VAR       ?DD     ,STRING*2
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

    ?MSGX    :=  '## ﾗｲﾌﾞﾗﾘﾘｽﾄ 発行指示 ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ｼｽﾃﾑ日付取得##./
    ?SYSDATE  :=    @SCDATED
    ?DATE     :=    %SBSTR(?SYSDATE,1,6)
    ?YY       :=    %SBSTR(?DATE,5,2)
    ?MM       :=    %SBSTR(?DATE,3,2)
    ?DD       :=    %SBSTR(?DATE,1,2)
    ?DATE     :=    ?YY && ?MM && ?DD
    ?SPL      :=    'WS' && ?DATE
    ?SPLNM    :=    %NAME(?SPL)
    ?MSGX    :=  '## ﾛｸﾞ発行日付    = ' && ?SPL
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
/.##ｽﾌﾟｰﾙﾌｧｲﾙ名変更##./
    CHGCMVAR '@OUTDSN',?SPLNM

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?PROFL   :=  @JOBPROF
    ?PRF     :=  %STRING(?PROFL)
    ?MSGX    :=  '## LOGINﾌﾟﾛﾌｨｰﾙ名 = ' && ?PRF
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名    = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##確認画面##./
INIT000:

    ?OPR1  :=  '　＃＃＃＃　　ライブラリリスト出力　　　＃＃＃＃'
    ?OPR2  :=  '　ライブラリリストの出力を行ないます。'
    ?OPR3  :=  '　プリンタは使用可能でしょうか？'
    ?OPR4  :=  '　ＯＫの場合は、ＥＮＴＥＲを押して下さい。'
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKSLIB)##./
MAIN010:

    ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKSLIB) ##'
    SNDMSG    ?MSGX,TO-XCTL

    DSPLIB LIB-TOKSLIB,OUTPUT-@LIST
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKSLIB)   異常'
              SNDMSG    ?MSGX,,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
              GOTO ABEND
    END

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKCLIB)##./
MAIN020:

    ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKCLIB) ##'
    SNDMSG    ?MSGX,TO-XCTL

    DSPLIB LIB-TOKCLIB,OUTPUT-@LIST
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKCLIB)   異常'
              SNDMSG    ?MSGX,,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
              GOTO ABEND
    END

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKSLIBS)##./
MAIN030:

    ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKSLIBS)##'
    SNDMSG    ?MSGX,TO-XCTL

    DSPLIB LIB-TOKSLIBS,OUTPUT-@LIST
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKSLIBS)   異常'
              SNDMSG    ?MSGX,,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
              GOTO ABEND
    END

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKCLIBS)##./
MAIN040:

    ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKCLIBS)##'
    SNDMSG    ?MSGX,TO-XCTL

    DSPLIB LIB-TOKCLIBS,OUTPUT-@LIST
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKCLIBS)   異常'
              SNDMSG    ?MSGX,,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
              GOTO ABEND
    END

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(OSKSLIB)##./
MAIN050:

    ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(OSKSLIB) ##'
    SNDMSG    ?MSGX,TO-XCTL

    DSPLIB LIB-OSKSLIB,OUTPUT-@LIST
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(OSKSLIB)   異常'
              SNDMSG    ?MSGX,,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
              GOTO ABEND
    END

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(OSKCLIB)##./
MAIN060:

    ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(OSKCLIB) ##'
    SNDMSG    ?MSGX,TO-XCTL

    DSPLIB LIB-OSKCLIB,OUTPUT-@LIST
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(OSKCLIB)   異常'
              SNDMSG    ?MSGX,,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
              GOTO ABEND
    END

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKCLLIB)##./
MAIN070:

    ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKCLLIB) ##'
    SNDMSG    ?MSGX,TO-XCTL

    DSPLIB LIB-TOKCLLIB,OUTPUT-@LIST
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKCLLIB)  異常'
              SNDMSG    ?MSGX,,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
              GOTO ABEND
    END

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKSRLIB)##./
MAIN080:

    ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKSRLIB) ##'
    SNDMSG    ?MSGX,TO-XCTL

    DSPLIB LIB-TOKSRLIB,OUTPUT-@LIST
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '##ﾗｲﾌﾞﾗﾘﾘｽﾄ発行(TOKSRLIB)  異常'
              SNDMSG    ?MSGX,,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
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

