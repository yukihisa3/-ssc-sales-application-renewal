/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    ACOSPG4                                 *  ./
/. *   JOB-NAME    :    売上／仕入計上データ退避             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'ACOSPG4 '
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

    ?PGNM :=  '日次データＭＯ退避処理'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##実行端末取得##./
    ?WKSTN    :=      %STRING(@ORGWS)
    SNDMSG ?WKSTN,TO-XCTL
/.##実行端末チェック##./
ASS:

    DEFLIBL TOKELIB/TOKFLIB

    IF     ?WKSTN     ^=    'WKSTNNAV'
        THEN
        IF ?WKSTN ^= 'WKSTN003'
           THEN
            SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
            SNDMSG '＊　　　　　　　　　　　　　　　＊',TO-XCTL
            SNDMSG '＊この端末からは処理できません　＊',TO-XCTL
            SNDMSG '＊　　　　　　　　　　　　　　　＊',TO-XCTL
            SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
            GOTO   RTN
        END
    END

/.##退避処理（売上／仕入ＤＴ）##./
CNVFILE:

    ?STEP :=   'CNVFILE '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＃＃＃＃＃　日次データ作成　＃＃＃＃＃＃＃'
    ?OPR2  :=  ''
    ?OPR3  :=  '　前日の日次用ＭＯはセットされていますか？'
    ?OPR4  :=  ''
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##退避処理（売上／仕入ＤＴ）##./
SAVFILE:

    ?MSGX := '## 計上ﾃﾞｰﾀＭＯ退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    ?STEP :=   'SAVFILE '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX := '## 本社ﾃﾞｰﾀ(2900)退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    SAVFILE FILE-TOKUHOU.TOKBLIB/TOKUHOS.TOKBLIB,TODEV-MO,
            MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 福岡ﾃﾞｰﾀ(2910)退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    SAVFILE FILE-TOKUFKU.TOKBLIB/TOKUFKS.TOKBLIB,TODEV-MO,
            MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 仙台ﾃﾞｰﾀ(2920)退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    SAVFILE FILE-TOKUSEU.TOKBLIB/TOKUSES.TOKBLIB,TODEV-MO,
            MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 岡山ﾃﾞｰﾀ(2940)退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    SAVFILE FILE-TOKUOKU.TOKBLIB/TOKUOKS.TOKBLIB,TODEV-MO,
            MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 北海道ﾃﾞｰﾀ(2950)退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    SAVFILE FILE-TOKUHKU.TOKBLIB/TOKUHKS.TOKBLIB,TODEV-MO,
            MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 大阪ﾃﾞｰﾀ(2990)退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    SAVFILE FILE-TOKUOSU.TOKBLIB/TOKUOSS.TOKBLIB,TODEV-MO,
            MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 全社ﾃﾞｰﾀ      退避中 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    SAVFILE FILE-ACOSFILE.TOKWLIB/ACOSWK.TOKWLIB,TODEV-MO,
            MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '退避処理が正常終了しました。'
    ?KEKA2 :=  '確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '退避処理が異常終了しました。'
    ?KEKA2 :=  '確認の為，ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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