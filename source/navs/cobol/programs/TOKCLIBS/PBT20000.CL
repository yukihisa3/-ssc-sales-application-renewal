/. ***********************************************************  ./
/. *     サカタのタネ　本社システム　　　　　　　　          *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム　　　　　　　　　   *  ./
/. *   JOB-ID      :    PBT20000                             *  ./
/. *   JOB-NAME    :    新ナフコ出荷データ抽出　　　　　　   *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS      ,STRING*08,VALUE-'        '      /.ﾜｰｸｽﾃｰｼｮﾝ文字  ./
    VAR ?BUMON   ,STRING*04,VALUE-'    '          /.部門名       ./
    VAR ?TANCD6  ,STRING*06,VALUE-'      '        /.担当者CDWK   ./
    VAR ?TANCD   ,STRING*02,VALUE-'  '            /.担当者CD     ./
    VAR ?TANNO   ,STRING*03,VALUE-'   '           /.端末番号./
  /.VAR ?SOUKO   ,STRING*02,VALUE-'  './          /.倉庫コード     ./
    VAR ?DSOUKO  ,STRING*02,VALUE-'  '            /.代表倉庫コード ./
    VAR ?WKSTN   ,NAME                            /.ﾜｰｸｽﾃｰｼｮﾝ名前  ./
    VAR ?PGMEC   ,INTEGER                         /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ ./
    VAR ?PGMECX  ,STRING*11                       /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ    ./
    VAR ?PGMEM   ,STRING*99                       /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ  ./
    VAR ?MSG     ,STRING*99(6)                    /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX    ,STRING*99                         /.SNDMSG表示用 ./
    VAR ?CLID    ,STRING*08,VALUE-'PBT20000'        /.CLID         ./
    VAR ?STEP    ,STRING*08                         /.STEP-ID      ./
    VAR ?CLNM    ,STRING*40                         /.CL名称       ./
    VAR ?KEKA1   ,STRING*40                         /.ﾒｯｾｰｼﾞ1      ./
    VAR ?KEKA2   ,STRING*40                         /.ﾒｯｾｰｼﾞ2      ./
    VAR ?KEKA3   ,STRING*40                         /.ﾒｯｾｰｼﾞ3      ./
    VAR ?KEKA4   ,STRING*40                         /.ﾒｯｾｰｼﾞ4      ./
    VAR ?OPR1    ,STRING*50                         /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2    ,STRING*50                         /.      2    ./
    VAR ?OPR3    ,STRING*50                         /.      3    ./
    VAR ?OPR4    ,STRING*50                         /.      4    ./
    VAR ?OPR5    ,STRING*50                         /.      5    ./

   /.##パラメタ（個別）##./
    /.抽出件数情報./
    VAR ?KANRI   ,STRING*08,VALUE-'00000000'  /.管理番号./
    VAR ?SOUKO   ,STRING*02,VALUE-'  '        /.倉庫コード./
    VAR ?CYUKBN  ,STRING*01,VALUE-' '         /.抽出区分./
    VAR ?KENSUU1 ,STRING*08,VALUE-'00000000'  /.　件数1./
    VAR ?KENSUU2 ,STRING*08,VALUE-'00000000'  /.　件数2./
    VAR ?KENSUU3 ,STRING*08,VALUE-'00000000'  /.　件数3./
    VAR ?KENSUU4 ,STRING*08,VALUE-'00000000'  /.　件数4./
    VAR ?KENSUU5 ,STRING*08,VALUE-'00000000'  /.　件数5./
    VAR ?KENSUU6 ,STRING*08,VALUE-'00000000'  /.　件数6./

/.------------------------------------------------------------------./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := '新ナフコ出荷データ抽出　　　　　　　　'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKDLIB/TOKKLIB/BMSFLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?CLID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD6)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD6,1,2)

/.##端末番号を取得##./
SBT0620B:

    ?STEP :=   'SBT0620B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SBT0620B.TOKELIBO,PARA-(?WS,?TANNO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '端末番号取得'
              GOTO ABEND END
    ?MSGX := '## 端末NO = ' && ?TANNO
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ナフコ出荷データロック##./
ASSIGN:

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ASSIGN    FILE-NFKGTRF.TOKDLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で抽出処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  'ナフコ出荷データロック'
              GOTO ABEND
    END

    ASSIGN    FILE-NFSYUKA.TOKDLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で抽出処理が行われています',TOWS-@ORGWS
              ?KEKA4 :=  'ナフコ出荷データロック'
              GOTO ABEND
    END

/.##新ナフコ出荷データ抽出指示##./
SBT2000I:

    ?STEP :=   'SBT2000I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIBO
    CALL      PGM-SBT2000I.TOKELIBO,
              PARA-(?KANRI,?SOUKO,?CYUKBN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
         IF   ?PGMEC     =   4010 THEN
           /. RELEASE   BFKGTRF.TOKDLIB!@XCL ←誤 ./
              RELEASE   NFKGTRF.TOKDLIB!@XCL
              RELEASE   NFSYUKA.TOKDLIB!@XCL
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  'ナフコ出荷データ抽出指示'
              GOTO ABEND
         END
    END

    ?MSGX := '# 管理番号　　 = ' && ?KANRI  && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '# 倉庫ＣＤ　　 = ' && ?SOUKO  && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '# 抽出区分　　 = ' && ?CYUKBN && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF        ?CYUKBN   =   '1'    THEN
              GOTO      SBT2020B
    END
    IF        ?CYUKBN   =   '2'    THEN
              GOTO      SBT2020B
    END
    IF        ?CYUKBN   =   '4'    THEN
              GOTO      SBT2020B
    END
    IF        ?CYUKBN   =   '5'    THEN
              GOTO      SBT2020B
    END

/.##新ナフコ箱トレーデータ抽出##./
SBT2010B:

    ?STEP :=   'SBT2010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SBT2010B.TOKELIBO
             ,PARA-(?KANRI,?SOUKO,?CYUKBN,?KENSUU3)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '新ナフコ箱トレーデータ抽出'
              GOTO ABEND
    END
    ?MSGX := '# 抽出件数３　 = ' && ?KENSUU3 && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    GOTO      FIMPORT1

/.##新ナフコ出荷明細データ抽出##./
SBT2020B:

    ?STEP :=   'SBT2020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SBT2020B.TOKELIBO
             ,PARA-(?KANRI,?SOUKO,?CYUKBN,?KENSUU1,?KENSUU2,
                    ?KENSUU4,?KENSUU6)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ナフコ出荷明細データ抽出'
              GOTO ABEND
    END
    ?MSGX := '# 抽出件数１　 = ' && ?KENSUU1 && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '# 抽出件数２　 = ' && ?KENSUU2 && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '# 抽出件数４　 = ' && ?KENSUU4 && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '# 抽出件数５　 = ' && ?KENSUU6 && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    GOTO      FIMPORT2

/.##データ転送##./
FIMPORT1:

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    FIMPORT   FILE-NFKGTRF.TOKDLIB,TYPE-@FILE,
              PARA-NEWLINKS,UNIT-3,OPR-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ナフコ個口トレー数データ転送'
              GOTO ABEND
    END
    GOTO      SBT1050L

FIMPORT2:

    ?STEP :=   'FIMPORT2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF        ?CYUKBN   =   '1'    THEN
       ?MSGX := '# 本発デリバリ以外出荷数Ｄ転送 #'
       SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
       FIMPORT   FILE-NFSYUKA.TOKDLIB,TYPE-@FILE,
                 PARA-NEWLINKS,UNIT-1,OPR-@NO
       ?PGMEC    :=    @PGMEC
       ?PGMEM    :=    @PGMEM
       IF        ?PGMEC    ^=   0    THEN
                 ?KEKA4 :=  '本発デリバリ以外出荷数Ｄ転送'
                 GOTO ABEND
       END
    END
    IF        ?CYUKBN   =   '2'    THEN
       ?MSGX := '# 本発デリバリ　　出荷数Ｄ転送 #'
       SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
       FIMPORT   FILE-NFSYUKA.TOKDLIB,TYPE-@FILE,
                 PARA-NEWLINKS,UNIT-2,OPR-@NO
       ?PGMEC    :=    @PGMEC
       ?PGMEM    :=    @PGMEM
       IF        ?PGMEC    ^=   0    THEN
                 ?KEKA4 :=  '本発デリバリ以外出荷数Ｄ転送'
                 GOTO ABEND
       END
    END
    IF        ?CYUKBN   =   '4'    THEN
       ?MSGX := '# リピート　出荷数Ｄ転送 #'
       SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
       FIMPORT   FILE-NFSYUKA.TOKDLIB,TYPE-@FILE,
                 PARA-NEWLINKS,UNIT-4,OPR-@NO
       ?PGMEC    :=    @PGMEC
       ?PGMEM    :=    @PGMEM
       IF        ?PGMEC    ^=   0    THEN
                 ?KEKA4 :=  'リピート　出荷数Ｄ転送'
                 GOTO ABEND
       END
    END
    IF        ?CYUKBN   =   '5'    THEN
       ?MSGX := '# 本発すべて　出荷数Ｄ転送 #'
       SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
       FIMPORT   FILE-NFSYUKA.TOKDLIB,TYPE-@FILE,
                 PARA-NEWLINKS,UNIT-5,OPR-@NO
       ?PGMEC    :=    @PGMEC
       ?PGMEM    :=    @PGMEM
       IF        ?PGMEC    ^=   0    THEN
                 ?KEKA4 :=  '本発すべて　出荷数Ｄ転送'
                 GOTO ABEND
       END
    END

/.##ＬＩＮＫＳ連携リスト出力##./
SBT1050L:

    ?STEP :=   'SBT1050L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRPRTF FILE-XU04LP,TOFILE-LBPPRTE1.TOKELIBO,MEDLIB-TOKELIBO
    CALL      PGM-SBT2030L.TOKELIBO
                 ,PARA-(?BUMON,
                        ?TANCD,
                        ?CYUKBN,
                        ?SOUKO,
                        ?KENSUU5,
                        ?KENSUU1,
                        ?KENSUU2,
                        ?KENSUU3,
                        ?KENSUU4,
                        ?KENSUU6)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '新ＬＩＮＫＳ連携結果リスト出力'
              GOTO ABEND
    END

/.##正常終了処理##./
RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  '　【ナフコ出荷データ抽出】'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC
/.------------------------------------------------------------------./

/.##異常終了処理##./
ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?CLID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

    RETURN    PGMEC-?PGMEC
