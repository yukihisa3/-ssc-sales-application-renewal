/. ***********************************************************  ./
/. *   (株)サカタのタネ　ホームガーデン部　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＨＧシステム                         *  ./
/. *   JOB-ID      :    PDL00100                             *  ./
/. *   JOB-NAME    :    在庫マスタ０削除                     *  ./
/. *   UPDATE      :    2011/11/24 MIURA MOからLTOへ変更     *  ./
/. *   UPDATE      :    2017/05/12 TAKAHASHI 多階層へ変更    *  ./
/. *   UPDATE      :    2017/05/16 TAKAHASHI ASSIGN追加      *  ./
/. *   UPDATE      :    2020/01/20 INOUE S11301570           *  ./
/. *                                     オフコンクラウド    *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PDL00100'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
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

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '在庫マスタ＝０削除'

/.##資源の獲得##./
ASS:
     ASSIGN FILE-ZAMZAIF.TOKFLIB!@XCL
     IF     @PGMEC ^=  0  THEN
       ?OPR1 := '　＃＃＃＃　　在庫マスタ＝０削除処理　　＃＃＃＃　'
       ?OPR2 := '　＃在庫マスタが他の端末で使用されております。＃　'
       ?OPR3 := '　＃端末使用状況を確認して下さい。　　　　　　＃　'
       ?OPR4 := '　＃　再実行＝ＥＮＴＥＲ　取消＝ＰＦ９　　　　＃　'
       ?OPR5 := '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
       CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
       GOTO   ASS     END

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面表示##./
STEP010:

    ?JBID    :=   'PDL00100'
    ?CLID    :=   'PDL00100'
    ?MSG1    := '<<< ' && ?CLID && ' START '
               && %SBSTR(@SDATEY,1,2) && '/'
               && %SBSTR(@SDATEY,3,2) && '/'
               && %SBSTR(@SDATEY,5,2) && ' '
               && %SBSTR(@STIME,1,2)  && ':'
               && %SBSTR(@STIME,3,2)  && ':'
               && %SBSTR(@STIME,5,2)  && ' >>>'
     SNDMSG  MSG-?MSG1,TOWS-@ORGWS,JLOG-@YES

    ?OPR1  :=  '　＃＃＃＃＃＃＃　在庫マスタ削除　＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　在庫マスタ　レコード＝０のデータの削除を行ない'
    ?OPR3  :=  '　　ます。確認して下さい。'
    ?OPR4  :=  '　　尚、障害発生時は再度実行しないでＮＡＶ連絡！！'
    ?OPR5  :=  '　　（多階層ライブラリバックアップ）'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB

/.##在庫マスタ　データ退避##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 在庫ﾏｽﾀ ﾊﾞｯｸｱｯﾌﾟ ##'
    SNDMSG    ?MSGX,TO-XCTL

/.##SAVFILE FILE-ZAMZAIF.TOKFLIB,TODEV-LTO,MODE-@USED   ##./
/.##SAVFILE FILE-ZAMZAIF.TOKFLIB,TODEV-@NONE,MODE-@USED,##./
/.##        TOPATH-'/NAVPC/GETUJI/GETZAI0',REP-@YES,    ##./
/.##        COMPRESS-@YES                               ##./

    SAVFILE FILE-ZAMZAIF.TOKFLIB,TODEV-@NONE,MODE-@USED,
            REP-@YES,COMPRESS-@YES,
     /.社内 TOPATH-'/NAVPC/PDL00100/' ./
            TOPATH-'/HGNAS/PDL00100/'

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【在庫マスタ退避】'
              GOTO ABEND END

/.##在庫マスタ　レコード＝０削除##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 在庫ﾏｽﾀ０削除 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-WKZAIF,TOFILE-WKZAIF.TOKWLIB
    CALL      PGM-SDL0010B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【在庫マスタ削除】'
              GOTO ABEND END


/.##プログラム正常終了（資源の開放）##./
RTN:

    ?KEKA1 :=  '削除処理が正常終了しました。'
    ?KEKA2 :=  '確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

    ?KEKA1 :=  '削除処理が異常終了しました。'
    ?KEKA2 :=  '早急にＮＡＶまで連絡して下さい。'
    ?KEKA3 :=  ''
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