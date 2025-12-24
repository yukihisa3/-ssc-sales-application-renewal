/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    リックＷｅｂＥＤＩ対応　　　　　　   *  ./
/. *   JOB-ID      :    PSJH8901                             *  ./
/. *   JOB-NAME    :    リックＷｅｂＥＤＩ対応　　　　　　   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PSJH8901'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
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

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'リックＷｅｂデータ取込変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【リックＷｅｂデータ取込変換処理】'
    ?OPR2  :=  'インターネットよりダウンロード完了済ですか？'
    ?OPR3  :=  '　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR4  :=  'この処理で、各事業所のデータが作成されます。'
    ?OPR5  :=  '完了後、件数等を確認して下さい。　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##受信ＤＴ初期化##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-LICFILE.ONLBLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ初期化】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-LICFILE.ONLBLIB,MODE-@REP,PARA-LIC,
            UNIT-1
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ取込処理】'
                    GOTO ABEND
    END

/.##バッチ_採番処理##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL  SJH8999B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【バッチ_採番処理】'
                    GOTO ABEND
    END

/.##ＪＥＤＩＣＯＳデータ取込変換処理##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-JEDICOS,TOFILE-LICFILE.ONLBLIB
    OVRF FILE-LICFILE1,TOFILE-LICFILE1.ONLBLIB
    OVRF FILE-LICFILE2,TOFILE-LICFILE2.ONLBLIB
    OVRF FILE-LICFILE3,TOFILE-LICFILE3.ONLBLIB
    CALL SJH8900B.TOKELIBO
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ振分】'
                    GOTO ABEND
    END

/.##受信件数リスト##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-LICFILE1,TOFILE-LICFILE1.ONLBLIB
    OVRF FILE-LICFILE2,TOFILE-LICFILE2.ONLBLIB
    OVRF FILE-LICFILE3,TOFILE-LICFILE3.ONLBLIB
    CALL SJH8702L.TOKELIBO,PARA-(?CHK1,?CHK2)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信結果リスト】'
                    GOTO ABEND
    END


/.##各事業部変換チェック##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF   ?CHK1  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## リック（資材）　　　　　変換開始 ##',TO-XCTL
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
         CALL PSJH3102.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK4)
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
         CALL PSJH8402.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK8)
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

./
    IF   ?CHK9  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ダイキ　　　　　　資材　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PSJH7904.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK9)
         IF   ?PGCHK3  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ダイキ　　　　　資材　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ダイキ　　　　　　資材　変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK10 =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ダイキ　　　　　　植物　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PSJH7905.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK9)
         IF   ?PGCHK3  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ダイキ　　　　　植物　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ダイキ　　　　　　植物　変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

/.##受信ＤＴクリア処理##./
STEP080:

    ?STEP :=   'STEP080 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3149.TOKKLIB,PARA-DCMJAPAN,UNIT-8
    FIMPORT FILE-CLR3149.TOKKLIB,PARA-DCMJAPAN,UNIT-9
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データクリア処理】'
                    GOTO ABEND
    END


/.##プログラム正常終了（資源の開放）##./
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

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
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