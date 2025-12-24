/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携　　　　　　  　　　     *  ./
/. *   JOB-ID      :    PVD01002                             *  ./
/. *   JOB-NAME    :    入荷予定データ取込＆チェック　　　　 *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾊﾟﾗﾒﾀ 定義####./
/.  PGM (P1-?SIJI,P2-?SRKBN,P3-?JSKBN,P4-?DTKBN,P5-?SIJIDT,
         P6-?SIJITM,P7-?NKFILE,P8-?NKLIB,P9-?BUTAN,P10-?JKEKA)
./  PGM
/.  PARA ?SIJI    ,STRING*10,IN,VALUE-'          './ /. 指示NO./
/.  PARA ?SRKBN   ,STRING*1,IN,VALUE-' '          ./ /. 送受信./
/.  PARA ?JSKBN   ,STRING*1,IN,VALUE-' '          ./ /. 自動手動./
/.  PARA ?DTKBN   ,STRING*2,IN,VALUE-'  '         ./ /. ＤＴ種別./
/.  PARA ?SIJIDT  ,STRING*8,IN,VALUE-'        '   ./ /. 日付./
/.  PARA ?SIJITM  ,STRING*6,IN,VALUE-'      '     ./ /. 時刻./
/.  PARA ?NKFILE  ,STRING*8,IN,VALUE-'        '   ./ /. ファイル名./
/.  PARA ?NKLIB   ,STRING*8,IN,VALUE-'        '   ./ /. ＬＩＢ名./
/.  PARA ?BUTAN   ,STRING*6,IN,VALUE-'      '    ./ /. 部門＋担当者./
/.  PARA ?JKEKA   ,STRING*4,OUT,VALUE-'    '      ./ /. 結果./
    VAR  ?SIJI    ,STRING*10,VALUE-'0000008929'      /.指示NO./
    VAR  ?SRKBN   ,STRING*1,VALUE-'1'                /.送受信./
    VAR  ?JSKBN   ,STRING*1,VALUE-'1'                /.自動手動./
    VAR  ?DTKBN   ,STRING*2,VALUE-'40'               /.ＤＴ種別./
    VAR  ?SIJIDT  ,STRING*8,VALUE-'20230529'         /.日付./
    VAR  ?SIJITM  ,STRING*6,VALUE-'153100'           /.時刻./
    VAR  ?NKFILE  ,STRING*8,VALUE-'YTINYKF '         /.ファイル名./
    VAR  ?NKLIB   ,STRING*8,VALUE-'D365DLIB'         /.ＬＩＢ名./
    VAR  ?BUTAN   ,STRING*6,VALUE-'292029'           /.部門＋担当者./
    VAR  ?JKEKA   ,STRING*4,VALUE-'0000'             /.結果./
/.###ﾜｰｸｴﾘｱ定義###./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PVD01002'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1./
    VAR ?KEKA1    ,STRING*40                    /.      2./
    VAR ?KEKA2    ,STRING*40                    /.      3./
    VAR ?KEKA3    ,STRING*40                    /.      4./
    VAR ?KEKA4    ,STRING*40                    /.      5./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
/.###出力パラメタ###./
    VAR ?DT1      ,STRING*7,VALUE-'0000000'     /.件数(全)./
    VAR ?DT2      ,STRING*7,VALUE-'0000000'     /.件数(OK)./
    VAR ?DT3      ,STRING*7,VALUE-'0000000'     /.件数(NG)./
    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*8,VALUE-'        '
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME
    VAR       ?D365NYK  ,NAME!MOD
    VAR       ?D365NYKW ,STRING*17,VALUE-'                 '
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL D365DLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO/TOKDTLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '入荷予定データ取込＆チェック'

/.##ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    ?BUMON := %SBSTR(?BUTAN,1,4)
    ?TANCD := %SBSTR(?BUTAN,5,2)


/.##確認画面##./
STEP010:

    ?OPR1  :=  '【入荷予定データ取込＆チェック】'
    ?OPR2  :=  'Ｄ３５６からの入荷予定データの取込を行います'
    ?OPR3  :=  '　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR4  :=  '実行ＯＫか確認してください。　　　　　　　　'
    ?OPR5  :=  '完了後、件数等を確認して下さい。　　　　　　'
/.  CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
./
/.##端末別リスト出力ワークファイルＩＤ取得##./
   /.発注書店舗ワーク./
    ?FILNM    :=    ?NKFILE
    ?LIBNM    :=    ?NKLIB
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?D365NYK  :=    %NCAT(?FILID,?LIBID)
    ?D365NYKW :=    %STRING(?D365NYK)
    ?MSGX     :=    '##(受信)入荷予定F =' && ?D365NYKW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES


/.##入荷予定データ重複チェック##./
NVD0100B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-YTINYKF,TOFILE-?D365NYK
    OVRF FILE-YTICHKF,TOFILE-YTICHKF.D365DLIB
    CALL PGM-NVD0102B.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '入荷予定データ重複チェック'
              GOTO ABEND
    END

/.##入荷予定データ取込＆チェック##./
NVD0104B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-YTINYKF,TOFILE-YTICHKF.D365DLIB
    OVRF FILE-HJYOKEN,TOFILE-JYOKEN1.TOKFLIB
    CALL PGM-NVD0104B.TOKSOLIB,PARA-(?SIJI,?SRKBN,?JSKBN,?DTKBN,
                                     ?SIJIDT,?SIJITM,?DT1,?DT2,?DT3)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '入荷予定データ取込＆チェック'
              GOTO ABEND
    END

   /.###ﾊﾟﾗﾒﾀOUT出力###./
    ?MSGX :=  '#出力件数 = ' && ?DT1 && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '#件数ＯＫ = ' && ?DT2 && ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '#件数ＮＧ = ' && ?DT3&& ' #'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##取込件数リスト発行##./
NVD0130L:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-NVD0130L.TOKSOLIB,PARA-(?SIJI,?DTKBN,?SIJIDT,?SIJITM,
                                     ?BUMON,?TANCD,?DT1,?DT2,?DT3,
                                     ?JKEKA)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '取込件数リスト発行'
              GOTO ABEND
    END

/.##取込エラーリスト発行##./
NVD0120L:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-NVD0120L.TOKSOLIB,PARA-(?BUMON,?TANCD,?SIJI)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '取込エラーリスト発行'
              GOTO ABEND
    END

/.##入荷予定データ伝票単位更新##./
NVD0110B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-NVD0110B.TOKSOLIB,PARA-(?SIJI)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '入荷予定データ伝票単位更新'
              GOTO ABEND
    END

/.##発注Ｆ更新処理（入荷＆作業情報作成）##./
NVD0140B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-NVD0140B.TOKSOLIB,PARA-(?SIJI)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '発注Ｆ更新処理（入荷＆作業情報作成）'
              GOTO ABEND
    END

/.##重複分更新上書更新処理##./
NVD0103B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-NVD0103B.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '重複分更新上書更新処理'
              GOTO ABEND
    END

/.##（受信）入荷予定データ初期化##./
PCLRFIL1:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-YTINYKB4.D365DLIB,TOFILE-YTINYKB5.D365DLIB,
            ADD-@NO,BF-1
    CNVFILE FILE-YTINYKB3.D365DLIB,TOFILE-YTINYKB4.D365DLIB,
            ADD-@NO,BF-1
    CNVFILE FILE-YTINYKB2.D365DLIB,TOFILE-YTINYKB3.D365DLIB,
            ADD-@NO,BF-1
    CNVFILE FILE-YTINYKB1.D365DLIB,TOFILE-YTINYKB2.D365DLIB,
            ADD-@NO,BF-1
    CNVFILE FILE-?D365NYK,TOFILE-YTINYKB1.D365DLIB,
            ADD-@NO,BF-1

    CLRFILE FILE-?D365NYK
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '（受信）入荷予定データ初期化'
              GOTO ABEND
    END

/.##入荷予定データ重複初期化##./
PCLRFIL2:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-YTICHKB4.D365DLIB,TOFILE-YTICHKB5.D365DLIB,
            ADD-@NO,BF-1
    CNVFILE FILE-YTICHKB3.D365DLIB,TOFILE-YTICHKB4.D365DLIB,
            ADD-@NO,BF-1
    CNVFILE FILE-YTICHKB2.D365DLIB,TOFILE-YTICHKB3.D365DLIB,
            ADD-@NO,BF-1
    CNVFILE FILE-YTICHKB1.D365DLIB,TOFILE-YTICHKB2.D365DLIB,
            ADD-@NO,BF-1
    CNVFILE FILE-YTICHKF.D365DLIB,TOFILE-YTICHKB1.D365DLIB,
            ADD-@NO,BF-1

    CLRFILE FILE-YTICHKF.D365DLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '（受信）入荷予定データ初期化'
              GOTO ABEND
    END

/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:

    ?KEKA1 :=  'Ｄ３６５入荷予定ＤＴ取込＆チェック'
    ?KEKA2 :=  '処理が正常終了しました。'
    ?KEKA3 :=  '取込内容を確認してください'
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKMDLIB
    CALL SMG0040L.TOKSOLIB,
         PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./
    ?KEKA1 :=  'Ｄ３６５入荷予定ＤＴ取込＆チェック'
    ?KEKA2 :=  '処理が異常終了しました。'
    ?KEKA3 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKMDLIB
    CALL SMG0040L.TOKSOLIB,
         PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO ?MSGX :=   ?MSG(?I)
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC
