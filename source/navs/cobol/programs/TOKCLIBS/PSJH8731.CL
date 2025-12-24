/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ対応   *  ./
/. *   JOB-ID      :    PSJH8731                             *  ./
/. *   JOB-NAME    :    ＤＣＭＪＡＰＡＮ　受領データ取込処理 *  ./
/. *   UPDATE      :    2017.10.06 S11270540                 *  ./
/. *   UPDATE      :    2012.02.22 ケーヨー対応              *  ./
/. *   UPDATE      :    2021.11.08 S22470210 返品自動計上    *  ./
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
/.##返品自動計上パラメタ##./
    VAR       ?JKBN     ,STRING*1,VALUE-' '         /.実行区分./
    VAR       ?BUMCD    ,STRING*4,VALUE-'2920'      /.部門ＣＤ./
  /.VAR       ?TANCD    ,STRING*2,VALUE-'98'./      /.担当者ＣＤ./
    VAR       ?HTOKCD   ,STRING*8,VALUE-'00000000'  /.取引先ＣＤ./
    VAR       ?NDATE    ,STRING*8,VALUE-'        '  /.入力日./
    VAR       ?UPTIME   ,STRING*6,VALUE-'      '    /.更新日./
    VAR       ?DENCNT   ,STRING*7,VALUE-'0000000'   /.売伝追加件数./
    /. リスト出力用パラメタ ./
    VAR       ?KEIKBN   ,STRING*1,VALUE-'3'         /. 計上区分 ./
    VAR       ?TANST    ,STRING*2,VALUE-'98'        /. 担当者開始 ./
    VAR       ?TANED    ,STRING*2,VALUE-'98'        /. 担当者終了 ./
    VAR       ?DKBN     ,STRING*1,VALUE-'2'         /. 日付区分　 ./
    VAR       ?AFROM    ,STRING*8,VALUE-'00000000'  /. 検収日開始 ./
    VAR       ?ATO      ,STRING*8,VALUE-'00000000'  /. 検収日終了 ./
    VAR       ?NFROM    ,STRING*8,VALUE-'00000000'  /. 入力日開始 ./
    VAR       ?NTO      ,STRING*8,VALUE-'00000000'  /. 入力日終了 ./
    VAR       ?KFROM    ,STRING*8,VALUE-'00000000'  /. 計上日開始 ./
    VAR       ?KTO      ,STRING*8,VALUE-'00000000'  /. 計上日終了 ./
    VAR       ?SFROM    ,STRING*8,VALUE-'00000000'  /. 作成日開始 ./
    VAR       ?STO      ,STRING*8,VALUE-'00000000'  /. 作成日終了 ./
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
    VAR       ?BUMON  ,STRING*4,VALUE-'    '        /.部門名./
    VAR       ?TANCD8 ,STRING*8,VALUE-'        '    /.担当者CD./
    VAR       ?TANCD  ,STRING*2,VALUE-'  '          /.担当者CD./

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
            TOKDLIB/TOKWLIB/TOKSOLIB/TOKDTLIB

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END
    ?TANCD := ?TANCD8

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
/.2018/02/22 NAV ST./
    OVRF FILE-DCMKEIY1,TOFILE-DCMKEIY1.ONLBLIB
    OVRF FILE-DCMKEIY2,TOFILE-DCMKEIY2.ONLBLIB
    OVRF FILE-DCMKEIY3,TOFILE-DCMKEIY3.ONLBLIB
    OVRF FILE-DCMKEIY4,TOFILE-DCMKEIY4.ONLBLIB
/.2018/02/22 NAV ED./
    OVRF FILE-JCAFILEH,TOFILE-JCAFILEH.ONLBLIB
    OVRF FILE-DCMJYRW1,TOFILE-DCMJYRW1.TOKDTLIB
    CALL SJH8731B.TOKELIBO
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
/.2018/02/22 NAV ST./
    OVRF FILE-DCMKEIY1,TOFILE-DCMKEIY1.ONLBLIB
    OVRF FILE-DCMKEIY2,TOFILE-DCMKEIY2.ONLBLIB
    OVRF FILE-DCMKEIY3,TOFILE-DCMKEIY3.ONLBLIB
    OVRF FILE-DCMKEIY4,TOFILE-DCMKEIY4.ONLBLIB
/.2018/02/22 NAV ED./
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
              ?KEKA4 :=  'バッチ_採番処理'
              GOTO ABEND
    END

/.##受領データ共通変換##./
SJR0080B:

    ?STEP :=   'SJR0080B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0080B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '受領データ共通変換'
              GOTO ABEND
    END

/.##受信エラーリスト（受領・返品）##./
SJR0220L:

    ?STEP :=   'SJR0220L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0220L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '受信エラーリスト（受領・返品）'
              GOTO ABEND
    END

/.##受信状況リスト（受領・返品）##./
SJR0210L:

    ?STEP :=   'SJR0210L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0210L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '受信状況リスト（受領・返品）'
              GOTO ABEND
    END

/.##返品データ抽出##./
SJR0280B:

    ?STEP :=   'SJR0280B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0280B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '返品データ抽出'
              GOTO ABEND
    END

/.##纏め返品データ作成##./
SJR0370A:

    ?STEP :=   'SJR0370A'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0370A.TOKSOLIB,PARA-(?BUMON,?TANCD,
                                          ?HIDUKE,?JIKAN,'00000000')
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '纏め返品データ作成'
              GOTO ABEND
    END

/.↓ 以下追加 2021.11.08 ./
/.##返品自動計上用に　担当者ＣＤ　制御変更##./

    ?TANCD := '98'

/.##返品自動計上判定処理（通常）##./
SJR0430B:

    ?STEP :=   'SJR0430B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF        ?JKBN  =  '1'  THEN
              OVRF  FILE-COMRHEL2,TOFILE-COMRMHL7.TOKDTLIB
    ELSE
              OVRF  FILE-COMRHEL2,TOFILE-COMRHEL2.TOKDTLIB
    END

/.##CALL      PGM-SJR0430B.TOKSOLIB,
              PARA-(?JKBN,?BUMCD,?TANCD,?HIDUKE,?JIKAN,?HTOKCD,
                    ?NDATE) ##./   /.計上対象の入力日をOUT./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '返品自動計上判定処理（通常）'
              GOTO ABEND
    END

/.##返品自動計上処理（通常）##./
SJR0440B:

    ?STEP :=   'SJR0440B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF        ?JKBN = ' '  THEN
              OVRF  FILE-COMRHELA,TOFILE-COMRHELA.TOKDTLIB
    ELSE
              OVRF  FILE-COMRHELA,TOFILE-COMRMHLA.TOKDTLIB
    END
/.##CALL      PGM-SJR0440B.TOKSOLIB,
              PARA-(?BUMCD,?TANCD,?HIDUKE,?JIKAN,?HTOKCD,?NDATE,
                    ?UPTIME,?DENCNT)##./ /.計上時刻・件数をOUT./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '返品自動計上処理（通常）'
              GOTO ABEND
    END
    IF        ?DENCNT    =   '0000000'    THEN
              GOTO MATOME
    END

/.## 返品自動計上確認データ抽出（通常） ##./
SJR0360C:

    ?STEP :=   'SJR0360C'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?JKBN = ' '  THEN
        OVRF FILE-COMRHEL4,TOFILE-COMRHEL4.TOKDTLIB
        OVRF FILE-COMRHELB,TOFILE-COMRHELB.TOKDTLIB
        OVRF FILE-COMRHEL8,TOFILE-COMRHEL8.TOKDTLIB
        OVRF FILE-COMRHEL9,TOFILE-COMRHEL9.TOKDTLIB
    ELSE
        OVRF FILE-COMRHEL4,TOFILE-COMRMHL4.TOKDTLIB
        OVRF FILE-COMRHELB,TOFILE-COMRMHLB.TOKDTLIB
        OVRF FILE-COMRHEL8,TOFILE-COMRMHL8.TOKDTLIB
        OVRF FILE-COMRHEL9,TOFILE-COMRMHL9.TOKDTLIB
    END
    OVRF FILE-COMWHEL1,TOFILE-AUTWHEL1.TOKDTLIB

    ?NFROM  :=  ?NDATE   /.判定処理からの計上対象入力日./
    ?NTO    :=  ?NDATE   /.判定処理からの計上対象入力日./

/.##CALL PGM-SJR0360C.TOKSOLIB,PARA-(?KEIKBN,?HTOKCD,?TANST,?TANED,
                                     ?DKBN,?AFROM,?ATO,?NFROM,?NTO,
                                     ?KFROM,?KTO,?SFROM,?STO,
                                     ?UPTIME)##.//.計上更新日も条件./
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
        IF ?PGMEC = 4001  THEN
           GOTO  MATOME
        ELSE
           ?KEKA4 :=  '返品自動計上確認データ抽出（通常）'
           GOTO  ABEND
        END
    END

/.## 返品自動計上確認リスト発行（通常）##./
SJR0460L:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL

/.##CALL PGM-SJR0460L.TOKSOLIB,PARA-(?JKBN,?KEIKBN,?HTOKCD,?TANST,
             ?TANED,?DKBN,?AFROM,?ATO,?NFROM,?NTO,?KFROM,?KTO,
             ?SFROM,?STO) ##./
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?KEKA4 :=  '返品自動計上確認リスト発行（通常）'
        GOTO  ABEND
    END

/.##返品自動計上　纏め　に制御変更##./
MATOME:
    ?JKBN := '1'

/.##返品自動計上判定処理（纏め）##./
SJR0430M:

    ?STEP :=   'SJR0430M'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF        ?JKBN  =  '1'  THEN
              OVRF  FILE-COMRHEL2,TOFILE-COMRMHL7.TOKDTLIB
    ELSE
              OVRF  FILE-COMRHEL2,TOFILE-COMRHEL2.TOKDTLIB
    END

/.##CALL      PGM-SJR0430B.TOKSOLIB,
              PARA-(?JKBN,?BUMCD,?TANCD,?HIDUKE,?JIKAN,?HTOKCD,
                    ?NDATE)##./  /.計上対象の入力日をOUT./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '返品自動計上判定処理（纏め）'
              GOTO ABEND
    END

/.##返品自動計上処理（纏め）##./
SJR04400:

    ?STEP :=   'SJR0440M'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF        ?JKBN = ' '  THEN
              OVRF  FILE-COMRHELA,TOFILE-COMRHELA.TOKDTLIB
    ELSE
              OVRF  FILE-COMRHELA,TOFILE-COMRMHLA.TOKDTLIB
    END
/.##CALL      PGM-SJR0440B.TOKSOLIB,
              PARA-(?BUMCD,?TANCD,?HIDUKE,?JIKAN,?HTOKCD,?NDATE,
                    ?UPTIME,?DENCNT)##./ /.計上時刻・件数をOUT./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '返品自動計上処理（纏め）'
              GOTO ABEND
    END
    IF        ?DENCNT    =   '0000000'    THEN
              GOTO RTN
    END

/.## 返品自動計上確認データ抽出（纏め） ##./
SJR0360M:

    ?STEP :=   'SJR0360M'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?JKBN = ' '  THEN
        OVRF FILE-COMRHEL4,TOFILE-COMRHEL4.TOKDTLIB
        OVRF FILE-COMRHELB,TOFILE-COMRHELB.TOKDTLIB
        OVRF FILE-COMRHEL8,TOFILE-COMRHEL8.TOKDTLIB
        OVRF FILE-COMRHEL9,TOFILE-COMRHEL9.TOKDTLIB
    ELSE
        OVRF FILE-COMRHEL4,TOFILE-COMRMHL4.TOKDTLIB
        OVRF FILE-COMRHELB,TOFILE-COMRMHLB.TOKDTLIB
        OVRF FILE-COMRHEL8,TOFILE-COMRMHL8.TOKDTLIB
        OVRF FILE-COMRHEL9,TOFILE-COMRMHL9.TOKDTLIB
    END
    OVRF FILE-COMWHEL1,TOFILE-AUTWHEL1.TOKDTLIB

    ?NFROM  :=  ?NDATE   /.判定処理からの計上対象入力日./
    ?NTO    :=  ?NDATE   /.判定処理からの計上対象入力日./

/.##CALL PGM-SJR0360C.TOKSOLIB,PARA-(?KEIKBN,?HTOKCD,?TANST,?TANED,
                                     ?DKBN,?AFROM,?ATO,?NFROM,?NTO,
                                     ?KFROM,?KTO,?SFROM,?STO,
                                     ?UPTIME)##.//.計上更新日も条件./
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
        IF ?PGMEC = 4001  THEN
           GOTO  RTN
        ELSE
           ?KEKA4 :=  '返品自動計上確認データ抽出（纏め） '
           GOTO  ABEND
        END
    END

/.## 返品自動計上確認リスト発行（纏め）##./
SJR0460M:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL

/.##CALL PGM-SJR0460L.TOKSOLIB,PARA-(?JKBN,?KEIKBN,?HTOKCD,?TANST,
             ?TANED,?DKBN,?AFROM,?ATO,?NFROM,?NTO,?KFROM,?KTO,
             ?SFROM,?STO)##./
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        GOTO  ABEND
        ?KEKA4 :=  '返品自動計上確認リスト発行（纏め） '
    ELSE
        GOTO  RTN
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