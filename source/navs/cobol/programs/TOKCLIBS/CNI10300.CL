/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＨＧシステム                         *  ./
/. *   JOB-ID      :    CNI10300                             *  ./
/. *   JOB-NAME    :    月次更新前データバックアップ         *  ./
/. *   UPDATE      :    20120301　基幹サーバ統合             *  ./
/. ***********************************************************  ./
    PGM  (P1-?KBN)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA ?KBN     ,STRING*2,OUT,VALUE-'  ' /.OK,NG./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'CNI10300'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?OK     ,STRING*2,VALUE-'OK'        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?NG     ,STRING*2,VALUE-'NG'        /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
/.-----------------------------------------------------------./
    VAR       ?CLID   ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./
    VAR       ?SYSDATE,STRING*13                  /.ｼｽﾃﾑ日付   ./
    VAR       ?YOUBI  ,STRING*1                   /.曜日       ./

    DEFLIBL TOKELIB/TOKFLIB
/.##ｼｽﾃﾑ日付取得##./
    ?SYSDATE  :=    @SCDATED
/.##曜日取得##./
    ?YOUBI    :=    %SBSTR(?SYSDATE,13,1)

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '月次データ退避'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP040:
    DEFLIBL TOKELIB/TOKFLIB/TOKWLIB

/.##データバックアップ##./
STEP080:

    ?STEP :=   'STEP080 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**ﾗｲﾌﾞﾗﾘﾊﾞｯｸｱｯﾌﾟ',TO-XCTL
 /.20120301  DAT -> LTO  ./
 /. SAVLIB LIB-TOKFLIB/TOKWLIB/OSKFLIB/TOKKLIB,TODEV-DAT,
           REWIND-@AFTER,MODE-@USED  ./
    SAVLIB LIB-TOKFLIB/TOKWLIB/OSKFLIB/TOKKLIB,TODEV-LTO,
           REWIND-@BOTH,MODE-@USED,COMPRESS-@YES
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【ライブラリバックアップ】'
         GOTO ABEND
    END

/.##ＤＡＴエジュクト##./
STEP095:
/.20120301   削除  ./
/.  ?STEP :=   'STEP095 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**ｴｼﾞｭｸﾄ',TO-XCTL
    CTLMGZN DEV-DAT,MODE-@EJECT
    IF   @PGMEC    ^=   0    THEN
         ?KEKA4 := '【ＤＡＴエジェクト】'
         GOTO ABEND
    END        ./

/.##プログラム正常終了（資源の開放）##./
RTN:

/.##正常セット##./
    ?KBN   :=  ?OK
    ?KEKA1 :=  '月次データバックアップが正常終了'
    ?KEKA2 :=  'しました。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

/.##異常セット##./
    ?KBN   :=  ?NG
    ?KEKA1 :=  '月次データ退避処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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