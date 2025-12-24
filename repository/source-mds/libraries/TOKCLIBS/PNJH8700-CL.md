# PNJH8700

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNJH8700.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   ■サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ対応   *  ./
/. *   JOB-ID      :    PNJH8700                             *  ./
/. *   JOB-NAME    :    ＤＣＭＪＡＰＡＮ　Ｗｅｂデータ取込   *  ./
/. *                                                         *  ./
/. *<修正履歴>************************************************  ./
/. *   2018/02/15 NAV TAKAHASHI ケーヨーＤＣＭ統合           *  ./
/. *   2019/09/04 NAV TAKAHASHI 発注種別区分マスタ対応       *  ./
/. *   XXXX/XX/XX XXXXXXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮ *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PNJH8700'
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
    VAR       ?CHK10  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK11  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK12  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK13  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK14  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
/.2018/02/15 NAV ST./
    VAR       ?CHK15  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK16  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK17  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK18  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
/.2018/02/15 NAV ED./
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
    VAR       ?PGCHK10,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK11,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK12,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK13,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK14,STRING*1,VALUE-' '         /.      5    ./
    /.2018/02/15 NAV ST./
    VAR       ?PGCHK15,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK16,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK17,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK18,STRING*1,VALUE-' '         /.      5    ./
    /.2018/02/15 NAV ED./

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKDTLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＤＣＭＪＡＰＡＮ　取込変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
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
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKDLIB/TOKDTLIB

/.##受信ＤＴ初期化##./
STEP025:

    ?STEP :=   'STEP025 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-JEDIHAC.ONLBLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ初期化】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理（資材）##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-JEDIHACS.ONLBLIB,MODE-@REP,PARA-DCMJAPAN,
            UNIT-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ取込処理（資材）】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理（植物）##./
STEP035:

    ?STEP :=   'STEP035 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-JEDIHACK.ONLBLIB,MODE-@REP,PARA-DCMJAPAN,
            UNIT-7
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ取込処理（植物）】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理（資材）##./
STEP036:

    ?STEP :=   'STEP036 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-JEDIHACS.ONLBLIB,TOFILE-JEDIHAC.ONLBLIB,
            ADD-@NO,BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データコピー（資材）】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理（植物）##./
STEP037:

    ?STEP :=   'STEP037 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-JEDIHACK.ONLBLIB,TOFILE-JEDIHAC.ONLBLIB,
            ADD-@YES,BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データコピー（資材）】'
                    GOTO ABEND
    END

/.##バッチ番号採番処理##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL  SJH8399B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【バッチ番号採番処理】'
                    GOTO ABEND
    END

/.##ＪＥＤＩＣＯＳデータ取込変換処理##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-JEDICOS,TOFILE-JEDIHAC.ONLBLIB
    OVRF FILE-DJJOHOL1,TOFILE-DJJOHOL1.TOKKLIB
    OVRF FILE-JCAFILE1,TOFILE-JCAFILE1.ONLBLIB
    OVRF FILE-JCAFILE2,TOFILE-JCAFILE2.ONLBLIB
    OVRF FILE-JCAFILE3,TOFILE-JCAFILE3.ONLBLIB
    OVRF FILE-JCAFILE4,TOFILE-JCAFILE4.ONLBLIB
    OVRF FILE-JCAFILE5,TOFILE-JCAFILE5.ONLBLIB
    OVRF FILE-JCAFILE6,TOFILE-JCAFILE6.ONLBLIB
    OVRF FILE-JCAFILE7,TOFILE-JCAFILE7.ONLBLIB
    OVRF FILE-JCAKAMA1,TOFILE-KMSHAC.ONLBLIB
    OVRF FILE-JCAKAMA2,TOFILE-KSSHAC.ONLBLIB
    OVRF FILE-JCAKAMA3,TOFILE-KMSHAC2.ONLBLIB
    OVRF FILE-JCAKAMA4,TOFILE-KSSHAC2.ONLBLIB
    OVRF FILE-JCADAIK1,TOFILE-DAIKI1.ONLBLIB
    OVRF FILE-JCADAIK2,TOFILE-DAIKI2.ONLBLIB
    OVRF FILE-JCADAIK3,TOFILE-DAIKI3.ONLBLIB
    OVRF FILE-JCADAIK4,TOFILE-DAIKI4.ONLBLIB
    OVRF FILE-JCAFILE8,TOFILE-JCAFILE8.ONLBLIB
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    /.2018/02/15 NAV ST./
    OVRF FILE-JCAKEYO1,TOFILE-KEYOHAZ1.ONLBLIB
    OVRF FILE-JCAKEYO2,TOFILE-KEYOHAZ2.ONLBLIB
    OVRF FILE-JCAKEYO3,TOFILE-KEYOHAS1.ONLBLIB
    OVRF FILE-JCAKEYO4,TOFILE-KEYOHAS2.ONLBLIB
    /.2018/02/15 NAV ED./
    CALL SJH8700B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【ＪＥＤＩＣＯＳデータ変換】'
                    GOTO ABEND
    END


/.##受信件数リスト##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-HCSZHOK,TOFILE-JCAFILE1.ONLBLIB
    OVRF FILE-HCSZTOH,TOFILE-JCAFILE2.ONLBLIB
    OVRF FILE-HCSZKNT,TOFILE-JCAFILE3.ONLBLIB
    OVRF FILE-HCSKHOK,TOFILE-JCAFILE5.ONLBLIB
    OVRF FILE-HCSKTOH,TOFILE-JCAFILE6.ONLBLIB
    OVRF FILE-HCSKKNT,TOFILE-JCAFILE7.ONLBLIB
    OVRF FILE-KAHMASZ,TOFILE-KMSHAC.ONLBLIB
    OVRF FILE-KAHMASK,TOFILE-KSSHAC.ONLBLIB
    OVRF FILE-KAHMASZ1,TOFILE-KMSHAC2.ONLBLIB
    OVRF FILE-KAHMASK1,TOFILE-KSSHAC2.ONLBLIB
    OVRF FILE-DAIK1,TOFILE-DAIKI1.ONLBLIB
    OVRF FILE-DAIK2,TOFILE-DAIKI2.ONLBLIB
    OVRF FILE-DAIK3,TOFILE-DAIKI3.ONLBLIB
    OVRF FILE-DAIK4,TOFILE-DAIKI4.ONLBLIB
    /.2018/02/15 NAV ST./
    OVRF FILE-KEIYOSZ1,TOFILE-KEYOHAZ1.ONLBLIB
    OVRF FILE-KEIYOSZ2,TOFILE-KEYOHAZ2.ONLBLIB
    OVRF FILE-KEIYOSK1,TOFILE-KEYOHAS1.ONLBLIB
    OVRF FILE-KEIYOSK2,TOFILE-KEYOHAS2.ONLBLIB
    /.2018/02/15 NAV ED./
    OVRF FILE-ERRORF,TOFILE-JCAFILE8.ONLBLIB
    CALL SJH8702L.TOKELIBO,PARA-(?CHK1,?CHK2,?CHK3,?CHK4,?CHK5
                                ,?CHK6,?CHK7,?CHK8,?CHK9,?CHK10
                            /.2018/02/15 NAV ST
                                ,?CHK11,?CHK12,?CHK13,?CHK14)./
                                ,?CHK11,?CHK12,?CHK13,?CHK14
                                ,?CHK15,?CHK16,?CHK17,?CHK18)
                            /.2018/02/15 NAV ED./
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
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
         SNDMSG '## ホーマック北海道（資材）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH8303.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK1)
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
         CALL PNJH8304.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK2)
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
         CALL PNJH8305.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK3)
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
         CALL PNJH3103.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK4)
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
         SNDMSG '## ホーマック北海道（植物）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH8306.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK5)
         IF   ?PGCHK1  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ホーマック北海道植物　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック北海道（植物）変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK6  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック東北　（植物）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH8307.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK6)
         IF   ?PGCHK2  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ホーマック東北　植物　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック東北　（植物）変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK7  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック関東　（植物）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH8308.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK7)
         IF   ?PGCHK3  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ホーマック関東　植物　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ホーマック関東　（植物）変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK8  =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## カーマ　　　　　（植物）変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH8403.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK8)
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
         SNDMSG '## ダイキ　　　　　　資材　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH7906.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK9)
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
         CALL PNJH7905.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK10)
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

    IF   ?CHK11 =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ダイキ（九州）　　資材　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH7907.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK11)
         IF   ?PGCHK3  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ダイキ（九州）　資材　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ダイキ（九州）　　資材　変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK12 =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ダイキ（九州）　　植物　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH7908.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK12)
         IF   ?PGCHK3  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ダイキ（九州）　植物　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ダイキ（九州）　　植物　変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END


    IF   ?CHK13 =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## カーマ資材２　　　　　　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH3104.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK13)
         IF   ?PGCHK13  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## カーマ資材２　　　　　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## カーマ資材２　　　　　　変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END

    IF   ?CHK14 =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## カーマ植物２　　　　　　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH8404.TOKCLIBO,PARA-(?HIDUKE,?JIKAN,?PGCHK14)
         IF   ?PGCHK14  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## カーマ植物２　　　　　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## カーマ植物２　　　　　　変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END
/.2018/02/15 NAV ST ケーヨー追加./
    IF   ?CHK15 =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ケーヨー資材　東日本　　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH1110.TOKCOLIB,PARA-('1',?HIDUKE,?JIKAN,?PGCHK15)
         IF   ?PGCHK15  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ケーヨー資材　東日本　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ケーヨー資材　東日本　　変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END
    IF   ?CHK16 =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ケーヨー資材　西日本　　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH1110.TOKCOLIB,PARA-('2',?HIDUKE,?JIKAN,?PGCHK16)
         IF   ?PGCHK16  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ケーヨー資材　西日本　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ケーヨー資材　西日本　　変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END
    IF   ?CHK17 =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ケーヨー植物　東日本　　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH1110.TOKCOLIB,PARA-('3',?HIDUKE,?JIKAN,?PGCHK17)
         IF   ?PGCHK17  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ケーヨー植物　東日本　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ケーヨー植物　東日本　　変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END
    IF   ?CHK18 =  '1'  THEN
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ケーヨー植物　西日本　　変換開始 ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
         CALL PNJH1110.TOKCOLIB,PARA-('4',?HIDUKE,?JIKAN,?PGCHK18)
         IF   ?PGCHK18  = '1'    THEN
              SNDMSG '######################################',TO-XCTL
              SNDMSG '## ケーヨー植物　西日本　変換エラー ##',TO-XCTL
              SNDMSG '######################################',TO-XCTL
         END
    ELSE
         SNDMSG '######################################',TO-XCTL
         SNDMSG '## ケーヨー植物　西日本　　変換無し ##',TO-XCTL
         SNDMSG '######################################',TO-XCTL
    END
/.2018/02/15 NAV ED ケーヨー追加./

/.##ホーマック植物基本情報データ変更処理##./
STEP071:

    ?STEP :=   'STEP071 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF FILE-DJJOHOL3,TOFILE-DJJOHOL3.TOKKLIB
    CALL  SSY8870B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,'00001427')
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【　１４２７　倉庫チェック】'
                    GOTO ABEND
    END

/.##ホーマック植物基本情報データ変更処理##./
STEP072:

    ?STEP :=   'STEP072 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF FILE-DJJOHOL3,TOFILE-DJJOHOL3.TOKKLIB
    CALL  SSY8870B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,'00014272')
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【１４２７２　倉庫チェック】'
                    GOTO ABEND
    END

/.##ホーマック植物基本情報データ変更処理##./
STEP073:

    ?STEP :=   'STEP073 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF FILE-DJJOHOL3,TOFILE-DJJOHOL3.TOKKLIB
    CALL  SSY8870B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,'00014273')
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【１４２７３　倉庫チェック】'
                    GOTO ABEND
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

```
