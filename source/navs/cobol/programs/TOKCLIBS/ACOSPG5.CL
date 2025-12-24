/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    ACOSPG5                              *  ./
/. *   JOB-NAME    :    売上／仕入計上データ退避             *  ./
/. *   UPDATE      :    2011/11/24 MIURA MOからLTOへ変更     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'ACOSPG5 '
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./

    ?PGNM :=  '日次データ退避処理'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##実行端末取得##./
    ?WKSTN    :=      %STRING(@ORGWS)
    SNDMSG ?WKSTN,TO-XCTL
/.##実行端末チェック##./
ASS:

    DEFLIBL TOKELIB/TOKFLIB

/.##退避処理（売上／仕入ＤＴ）##./
SAVFILE:

    ?MSGX := '## 計上ﾃﾞｰﾀ退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    ?STEP :=   'SAVFILE '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX := '## 本社ﾃﾞｰﾀ(2900)退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    SAVFILE FILE-TOKUHOU.TOKBLIB/TOKUHOS.TOKBLIB,TODEV-LTO,
            REWIND-@BEFORE,MODE-@USED,COMPRESS-@YES
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 福岡ﾃﾞｰﾀ(2910)退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    SAVFILE FILE-TOKUFKU.TOKBLIB/TOKUFKS.TOKBLIB,TODEV-LTO,
            REWIND-@NO,MODE-@USED,COMPRESS-@YES
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 全社ﾃﾞｰﾀ      退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    SAVFILE FILE-ACOSFILE.TOKWLIB/ACOSWK.TOKWLIB,TODEV-LTO,
            REWIND-@AFTER,MODE-@USED,COMPRESS-@YES
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '## 日次データ退避バックアップ　正常 ##'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?MSGX :=  '## 日次データ退避バックアップ　異常 ##'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
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
