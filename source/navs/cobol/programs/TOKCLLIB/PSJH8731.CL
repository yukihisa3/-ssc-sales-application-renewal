/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ対応   *  ./
/. *   JOB-ID      :    PSJH8731                             *  ./
/. *   JOB-NAME    :    ＤＣＭＪＡＰＡＮ　受領データ取込処理 *  ./
/. *   UPDATE      :    2017.10.06 S11270540                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PSJH8731'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?OK1    ,STRING*2,VALUE-'  '        /.実行確認1./
    VAR       ?OK2    ,STRING*2,VALUE-'  '        /.実行確認2./
    VAR       ?OK3    ,STRING*2,VALUE-'  '        /.実行確認3./
    VAR       ?HIDUKE ,STRING*8,VALUE-'        '  /.バッチ日付./
    VAR       ?JIKAN  ,STRING*4,VALUE-'    '      /.バッチ時間./
    VAR       ?TOKCD  ,STRING*8,VALUE-'00000000'  /.バッチ得意先./
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

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＤＣＭＪＡＰＡＮ　受領変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【ＤＣＭＪＡＰＡＮ　受領変換処理】'
    ?OPR2  :=  'インターネットよりダウンロード完了済ですか？'
    ?OPR3  :=  '　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR4  :=  'この処理で、各事業所のデータが作成されます。'
    ?OPR5  :=  '完了後、件数等を確認して下さい。　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO/TOKJLIB/BMSFLIB/
            TOKDTLIB/TOKDLIB/TOKWLIB/TOKSOLIB

/.##資材ファイル初期化##./
STEP021:

    ?STEP :=   'STEP021 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-JEDICOJ1.ONLBLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【資材ファイル初期化】'
                    GOTO ABEND
    END

/.##植物ファイル初期化##./
STEP022:

    ?STEP :=   'STEP022 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-JEDICOJ2.ONLBLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【植物ファイル初期化】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理（資材）##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-JEDICOJ1.ONLBLIB,MODE-@ADD,PARA-DCMJAPAN,
            UNIT-5
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ取込処理（資材）】'
                    GOTO ABEND
    END

/.##受信ＤＴ取込処理（植物）##./
STEP035:

    ?STEP :=   'STEP035 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-JEDICOJ2.ONLBLIB,MODE-@ADD,PARA-DCMJAPAN,
            UNIT-6
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データ取込処理（植物）】'
                    GOTO ABEND
    END
/.##統合##./
STEP036:

    ?STEP :=   'STEP036 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-JEDICOJ1.ONLBLIB,TOFILE-JEDICOSJ.ONLBLIB,
            ADD-@NO,BF-1
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【統合（資材）】'
                    GOTO ABEND
    END
/.##統合##./
STEP037:

    ?STEP :=   'STEP037 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-JEDICOJ2.ONLBLIB,TOFILE-JEDICOSJ.ONLBLIB,
            ADD-@YES,BF-1
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【統合（植物）】'
                    GOTO ABEND
    END
/.##バッチ_採番処理##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL


/.##ＪＥＤＩＣＯＳデータ取込変換処理##./
STEP050:

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-JEDICOS,TOFILE-JEDICOSJ.ONLBLIB
    OVRF FILE-JCAFILEA,TOFILE-JCAFILEA.ONLBLIB
    OVRF FILE-JCAFILEB,TOFILE-JCAFILEB.ONLBLIB
    OVRF FILE-JCAFILEC,TOFILE-JCAFILEC.ONLBLIB
    OVRF FILE-JCAKAMAA,TOFILE-JCAKAMAA.ONLBLIB
    OVRF FILE-JCAKAMAB,TOFILE-JCAKAMAB.ONLBLIB
    OVRF FILE-JCAKAMAC,TOFILE-JCAKAMAC.ONLBLIB
    OVRF FILE-JCAKAMAD,TOFILE-JCAKAMAD.ONLBLIB
    OVRF FILE-JCAFILEE,TOFILE-JCAFILEE.ONLBLIB
    OVRF FILE-JCAFILEF,TOFILE-JCAFILEF.ONLBLIB
    OVRF FILE-JCAFILEG,TOFILE-JCAFILEG.ONLBLIB
    OVRF FILE-JCADAIK1,TOFILE-JCADAIK1.ONLBLIB
    OVRF FILE-JCADAIK2,TOFILE-JCADAIK2.ONLBLIB
    OVRF FILE-JCADAIK3,TOFILE-JCADAIK3.ONLBLIB
    OVRF FILE-JCADAIK4,TOFILE-JCADAIK4.ONLBLIB
    OVRF FILE-JCADAIK5,TOFILE-JCADAIK5.ONLBLIB
    OVRF FILE-JCADAIK6,TOFILE-JCADAIK6.ONLBLIB
    OVRF FILE-JCAFILEH,TOFILE-JCAFILEH.ONLBLIB
    OVRF FILE-DCMJYRW1,TOFILE-DCMJYRW1.TOKDTLIB
    CALL SJH8731B.TOKSOLIB
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【ＪＥＤＩＣＯＳデータ変換】'
                    GOTO ABEND
    END

/.##受信件数リスト##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-HCSZHOK,TOFILE-JCAFILEA.ONLBLIB
    OVRF FILE-HCSZTOH,TOFILE-JCAFILEB.ONLBLIB
    OVRF FILE-HCSZKNT,TOFILE-JCAFILEC.ONLBLIB
    OVRF FILE-KAHMASZ,TOFILE-JCAKAMAA.ONLBLIB
    OVRF FILE-KAHMASK,TOFILE-JCAKAMAB.ONLBLIB
    OVRF FILE-KAHMASZ2,TOFILE-JCAKAMAC.ONLBLIB
    OVRF FILE-KAHMASK2,TOFILE-JCAKAMAD.ONLBLIB
    OVRF FILE-HCSKHOK,TOFILE-JCAFILEE.ONLBLIB
    OVRF FILE-HCSKTOH,TOFILE-JCAFILEF.ONLBLIB
    OVRF FILE-HCSKKNT,TOFILE-JCAFILEG.ONLBLIB
    OVRF FILE-DAIK1,TOFILE-JCADAIK1.ONLBLIB
    OVRF FILE-DAIK2,TOFILE-JCADAIK2.ONLBLIB
    OVRF FILE-DAIK3,TOFILE-JCADAIK3.ONLBLIB
    OVRF FILE-DAIK4,TOFILE-JCADAIK4.ONLBLIB
    OVRF FILE-DAIK5,TOFILE-JCADAIK5.ONLBLIB
    OVRF FILE-DAIK6,TOFILE-JCADAIK6.ONLBLIB
    OVRF FILE-ERRORF,TOFILE-JCAFILEH.ONLBLIB
    CALL SJH8732L.TOKELIBO
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信結果リスト】'
                    GOTO ABEND
    END

/.##受信ＤＴクリア処理##./
STEP070:

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLR3149.TOKKLIB,PARA-DCMJAPAN,UNIT-10
    FIMPORT FILE-CLR3149.TOKKLIB,PARA-DCMJAPAN,UNIT-11
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信データクリア処理】'
                    GOTO ABEND
    END

/.##バッチ_採番処理##./
SJH8399B:

    ?STEP :=   'SJH8399B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJH8399B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受領データ共通変換##./
SJR0080B:

    ?STEP :=   'SJR0080B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0080B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信エラーリスト（受領・返品）##./
SJR0220L:

    ?STEP :=   'SJR0220L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0220L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信状況リスト（受領・返品）##./
SJR0210L:

    ?STEP :=   'SJR0210L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0210L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##返品データ抽出##./
SJR0280B:

    ?STEP :=   'SJR0280B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0280B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
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
    ?KEKA1 :=  'ＤＣＭＪＡＰＡＮの連続変換処理が異常終了'
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