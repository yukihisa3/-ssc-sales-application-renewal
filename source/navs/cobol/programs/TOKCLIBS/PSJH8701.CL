/. ***********************************************************  ./
/. *   ■サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ対応   *  ./
/. *   JOB-ID      :    PSJH8700                             *  ./
/. *   JOB-NAME    :    ＤＣＭＪＡＰＡＮ　Ｗｅｂデータ取込   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PSJH8700'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK3   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK4   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK5   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK6   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK7   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK8   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK9   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
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
    VAR       ?PGCHK4 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK5 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK6 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK7 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK8 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK9 ,STRING*1,VALUE-' '         /.      5    ./

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＤＣＭＪＡＰＡＮ　取込変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    /.認画面##./
STEP010:

    ?OPR1  :=  '【ＤＣＭＪＡＰＡＮ　取込変換処理】'
    ?OPR2  :=  'インターネットよりダウンロード完了済ですか？'
    ?OPR3  :=  '　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR4  :=  'この処理で、各事業所のデータが作成されます。'
    ?OPR5  :=  '完了後、件数等を確認して下さい。　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

    /.信ＤＴ取込処理##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-JEDICOS.ONLBLIB,MODE-@REP,PARA-DCMJAPAN,
            UNIT-1
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ取込処理】'
                    GOTO ABEND
    END

    /.ッチ■採番処理##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL  SJH8399B.TOKOBJ,PARA-(?HIDUKE,?JIKAN)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【バッチ■採番処理】'
                    GOTO ABEND
    END

     /.ＤＩＣＯＳデータ取込変換処理##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-JEDICOS,TOFILE-JEDICOS.ONLBLIB
    OVRF FILE-DJJOHOL1,TOFILE-DJJOHOL1.TOKKLIB
    OVRF FILE-JCAFILE1,TOFILE-JCAFILE1.ONLBLIB
    OVRF FILE-JCAFILE2,TOFILE-JCAFILE2.ONLBLIB
    OVRF FILE-JCAFILE3,TOFILE-JCAFILE3.ONLBLIB
    OVRF FILE-JCAFILE4,TOFILE-JCAFILE4.ONLBLIB
    OVRF FILE-JCAKAMA1,TOFILE-JCAKAMA1.ONLBLIB
    OVRF FILE-JCAFILE5,TOFILE-JCAFILE5.ONLBLIB
    OVRF FILE-JCAFILE6,TOFILE-JCAFILE6.ONLBLIB
    OVRF FILE-JCAFILE7,TOFILE-JCAFILE7.ONLBLIB
    OVRF FILE-JCAKAMA2,TOFILE-JCAKAMA2.ONLBLIB
    OVRF FILE-JCADAIKI,TOFILE-JCADAIKI.ONLBLIB
    OVRF FILE-JCAFILE8,TOFILE-JCAFILE8.ONLBLIB
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    CALL SJH8700B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【ＪＥＤＩＣＯＳデータ変換】'
                    GOTO ABEND
    END


    /.信件数リスト##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-HCSZHOK,TOFILE-JCAFILE1.ONLBLIB
    OVRF FILE-HCSZTOH,TOFILE-JCAFILE2.ONLBLIB
    OVRF FILE-HCSZKNT,TOFILE-JCAFILE3.ONLBLIB
    OVRF FILE-KAHMASZ,TOFILE-JCAKAMA1.ONLBLIB
    OVRF FILE-HCSKHOK,TOFILE-JCAFILE5.ONLBLIB
    OVRF FILE-HCSKTOH,TOFILE-JCAFILE6.ONLBLIB
    OVRF FILE-HCSKKNT,TOFILE-JCAFILE7.ONLBLIB
    OVRF FILE-KAHMASK,TOFILE-JCAKAMA2.ONLBLIB
    OVRF FILE-DAIKI,TOFILE-JCADAIKI.ONLBLIB
    OVRF FILE-ERRORF,TOFILE-JCAFILE8.ONLBLIB
    CALL SJH8702L.TOKELIBO,PARA-(?CHK1,?CHK2,?CHK3,?CHK4,?CHK5
                                ,?CHK6,?CHK7,?CHK8,?CHK9)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信結果リスト】'
                    GOTO ABEND
    END


    /.事業部変換チェック##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF   ?CHK1  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック北海道（資材）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PSJH8303.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK1)
         IF   ?PGCHK1  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ホーマック北海道資材　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック北海道（資材）変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK2  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック東北　（資材）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PSJH8304.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK2)
         IF   ?PGCHK2  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ホーマック東北　資材　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック東北　（資材）変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK3  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック関東　（資材）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PSJH8305.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK3)
         IF   ?PGCHK3  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ホーマック関東　資材　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック関東　（資材）変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK4  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## カーマ　　　　　（資材）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PSJH3101.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK4)
         IF   ?PGCHK3  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## カーマ　　　　　資材　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## カーマ　　　　　（資材）変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK5  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック北海道（資材）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PSJH8306.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK5)
         IF   ?PGCHK1  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ホーマック北海道資材　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック北海道（資材）変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK6  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック東北　（資材）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PSJH8307.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK6)
         IF   ?PGCHK2  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ホーマック東北　資材　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック東北　（資材）変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK7  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック関東　（資材）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PSJH8308.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK7)
         IF   ?PGCHK3  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ホーマック関東　資材　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック関東　（資材）変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK8  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## カーマ　　　　　（植物）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PSJH8401.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK8)
         IF   ?PGCHK3  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## カーマ　　　　　植物　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## カーマ　　　　　（植物）変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END


    IF   ?CHK9  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ダイキ　　　　　　　　　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PSJH7902.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK9)
         IF   ?PGCHK3  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ダイキ　　　　　　　　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ダイキ　　　　　　　　　変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END



    /.ログラム正常終了（資源の開放）##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ＤＣＭＪＡＰＡＮの連続変換処理が正常終了'
    ?KEKA2 :=  'しました。件数リスト等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

    /.ログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ＤＣＭＪＡＰＡＮ資材の連続変換処理が異常終了'
    ?KEKA2 :=  'しました。ログ採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
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