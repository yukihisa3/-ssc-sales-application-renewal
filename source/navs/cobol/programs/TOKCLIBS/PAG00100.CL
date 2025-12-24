/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　九州甘木　　　      *  ./
/. *   SYSTEM-NAME :    グッデイ売上計上　　　　　　　       *  ./
/. *   JOB-ID      :    PAG00100                             *  ./
/. *   JOB-NAME    :    グッデイ売上計上　　　　　　　　     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PAG00100'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK3   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?OK1    ,STRING*2,VALUE-'  '        /.実行確認1./
    VAR       ?OK2    ,STRING*2,VALUE-'  '        /.実行確認2./
    VAR       ?OK3    ,STRING*2,VALUE-'  '        /.実行確認3./
    VAR       ?HIDUKE ,STRING*8,VALUE-'        '  /.バッチ日付./
    VAR       ?JIKAN  ,STRING*4,VALUE-'    '      /.バッチ時間./
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
    VAR       ?PGCHK1 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK2 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK3 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'

    DEFLIBL TOKELIB/TOKFLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'グッデイ　ウタネ分　売上計上処理'
    ?SYORINM1 :=  'グッデイ　ウタネ分　売上計上処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【グッデイ　ウタネ分　売上計上処理】'
    ?OPR2  :=  'ウタネ出荷データの売上計上処理を行ないます？'
    ?OPR3  :=  'ＰＣ側での出力処理は完了していますか？　　　'
    ?OPR4  :=  '未完了の場合は、ＰＣ側の処理を実施願います。'
    ?OPR5  :=  '完了の場合は、ＥＮＴＥＲを押して下さい。★　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB

/.##グッデイＤＴ処理##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-YURURISF.TOKWLIB,MODE-@REP,PARA-UTANE2,
            UNIT-1
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ取込処理】'
                    GOTO ABEND
    END

/.##バッチ_採番処理##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL  SAG0010B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【バッチ_採番処理】'
                    GOTO ABEND
    END

/.##ＴＸＴより中間ファイル作成##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-YURURISF,TOFILE-YURURISF.TOKWLIB
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF FILE-AKADENF,TOFILE-AKADENF.TOKWLIB
    CALL SAG0020B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【売上中間ファイル作成】'
                    GOTO ABEND
    END

/.##売上中間ファイル→売上ファイル更新処理##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF FILE-AKADENF,TOFILE-AKADENF.TOKWLIB
    CALL SAG0030B.TOKELIBO,PARA-(?SYORIKN1,?SYORIKN2,?SYORIKN3)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【売上計上処理】'
                    GOTO ABEND
    END

/.##件数判定##./
STEP065:

    ?STEP :=   'STEP065 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF ?SYORIKN1 = '0000000'  THEN
       OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
       ?KEKA1 :=  'データ作成件数が、０件でです。'
       ?KEKA2 :=  'ＰＣからの取込処理で不具合がある可能性'
       ?KEKA3 :=  'があります。ＮＡＶへ連絡して下さい。'
       ?KEKA4 :=  ''
       CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
       DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
       OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                   /.１２３４５６７８９０１２３４５６７８９０１２'./
       ?SYORINM2 := 'データ作成件数が異常です。ＰＣ取込処理に'
       ?SYORINM3 := 'おいて不具合発生の可能性があります。　　'
       CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
       RETURN
    END

/.##グッデイＤＴ処理（初期化）##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-ZEROF.TOKFLIB,PARA-UTANE2,UNIT-2
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ初期化処理】'
                    GOTO ABEND
    END

/.##プログラム正常終了（資源の開放）##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'グッデイ　ウタネ分　売上計上処理が'
    ?KEKA2 :=  '正常終了しました。確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := 'グッデイ　ウタネ分　売上計上処理が正常終了しま　'
    ?SYORINM3 := 'した。確認して下さい。　　　　　　　　　　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'グッデイ　ウタネ分　売上計上処理が異常'
    ?KEKA2 :=  '終了しました。ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := 'グッデイ　ウタネ分　売上計上処理が異常終了しま　'
    ?SYORINM3 := 'した。ログを採取して、ＮＡＶへ連絡して下さい。　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)

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