/. ***********************************************************  ./
/. *  （株）サカタのタネ                                     *  ./
/. *   SYSTEM-NAME :    在庫ＥＸＣＥＬ連携　　　　　　       *  ./
/. *   JOB-NAME    :    在庫EXCELデータチェック・更新　      *  ./
/. *   JOB-ID      :    PZI00100                             *  ./
/. *   UPDATE      :    2016/07/08                           *  ./
/. *                      処理（作業系２）追加（Ｄ３６５用） *  ./
/. ***********************************************************  ./
    PGM  (P1-?JIKKOU)

/.##INパラメタ##./
    PARA  ?JIKKOU  ,STRING*1,IN,VALUE-' '
            /.実行区分                               ./
            /.  '1'    :   入出庫系_チェックのみ     ./
            /.  '2'    :   入出庫系_チェック・更新   ./
            /.  '3'    :   作業系  _チェックのみ     ./
            /.  '4'    :   作業系  _チェック・更新   ./
            /.  '5'    :   作業系２_チェックのみ     ./
            /.  '6'    :   作業系２_チェック・更新   ./

/.--------------------------------------------------------------./
    VAR  ?WS      ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?WKSTN   ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?PGMEC   ,INTEGER
    VAR  ?PGMES   ,STRING*5,VALUE-'     '
    VAR  ?PGMECX  ,STRING*11
    VAR  ?PGMEM   ,STRING*99
    VAR  ?MSG     ,STRING*99(6)
    VAR  ?MSGX    ,STRING*99
    VAR  ?PGMID   ,STRING*8,VALUE-'PZI00100'
    VAR  ?STEP    ,STRING*8
/.--------------------------------------------------------------./
    VAR  ?BUMON   ,STRING*4,VALUE-'    '     /.部門./
    VAR  ?TANCD8  ,STRING*8,VALUE-'        ' /.部門+担当者./
    VAR  ?TANCD   ,STRING*2,VALUE-'  '       /.担当者     ./
    VAR  ?SOKCD   ,STRING*2,VALUE-'00'       /.実行倉庫   ./
    VAR  ?DSOKCD  ,STRING*2,VALUE-'00'       /.代表倉庫   ./
/.--------------------------------------------------------------./
    VAR  ?CNT1    ,STRING*7,VALUE-'0000000'  /.件数(取込) ./
    VAR  ?CNT2    ,STRING*7,VALUE-'0000000'  /.件数(棚移）./
    VAR  ?CNT3    ,STRING*7,VALUE-'0000000'  /.件数(スト）./
    VAR  ?CNT4    ,STRING*7,VALUE-'0000000'  /.件数(移動）./
    VAR  ?CNT5    ,STRING*7,VALUE-'0000000'  /.件数(廃棄）./
    VAR  ?CNT6    ,STRING*7,VALUE-'0000000'  /.件数(セ組）./
    VAR  ?CNT7    ,STRING*7,VALUE-'0000000'  /.件数(バラ）./
    VAR  ?CNT8    ,STRING*7,VALUE-'0000000'  /.件数(ＥＲ）./
/.--------------------------------------------------------------./
    VAR  ?OPR1    ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?OPR2    ,STRING*50                 /.      2    ./
    VAR  ?OPR3    ,STRING*50                 /.      3    ./
    VAR  ?OPR4    ,STRING*50                 /.      4    ./
    VAR  ?OPR5    ,STRING*50                 /.      5    ./
/.--------------------------------------------------------------./
    VAR  ?MSG1    ,STRING*80                 /.開始終了MSG./
    VAR  ?PGNM    ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?KEKA1   ,STRING*40                 /.      2    ./
    VAR  ?KEKA2   ,STRING*40                 /.      3    ./
    VAR  ?KEKA3   ,STRING*40                 /.      4    ./
    VAR  ?KEKA4   ,STRING*40                 /.      5    ./
/.---------------------------------------------------------------./
   /.##倉庫別ファイルＩＤ編集用##./
    VAR  ?FILNSK  ,STRING*6,VALUE-'ZAINSK'   /.EXCELデータ(入出) ./
    VAR  ?FILSGY  ,STRING*6,VALUE-'ZAISGY'   /.EXCELデータ(作業) ./
    VAR  ?FILSBR  ,STRING*6,VALUE-'ZAISBR'   /.EXCELデータ(作業2)./
    VAR  ?FILWK   ,STRING*5,VALUE-'ZAIWK'    /.EXCEL取込ファイル ./
    VAR  ?LIBNM   ,STRING*8,VALUE-'ZAIEXCEL' /.ライブラリ        ./

    VAR   ?FILNM7  ,STRING*7,VALUE-'       '
    VAR   ?FILNM8  ,STRING*8,VALUE-'        '
    VAR   ?FILID   ,NAME
    VAR   ?LIBID   ,NAME

    VAR   ?ZAINSK  ,NAME!MOD
    VAR   ?ZAINSKX ,STRING*17

    VAR   ?ZAISGY  ,NAME!MOD
    VAR   ?ZAISGYX ,STRING*17

    VAR   ?ZAISBR  ,NAME!MOD
    VAR   ?ZAISBRX ,STRING*17

    VAR   ?ZAIWK   ,NAME!MOD
    VAR   ?ZAIWKX  ,STRING*17

    VAR   ?ZAIWKL1 ,NAME!MOD
    VAR   ?ZAIWKL1X,STRING*17

    VAR   ?ZAIWKL2 ,NAME!MOD
    VAR   ?ZAIWKL2X,STRING*17

    VAR   ?ZAIWKL3 ,NAME!MOD
    VAR   ?ZAIWKL3X,STRING*17

/.-----------------------------------------------------------./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKDLIB/TOKFLIB/TOKKLIB/ZAIEXCEL

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
CLSTART:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    CASE  ?JIKKOU     OF
          #'1'# ?PGNM := 'ＥＸＣＥＬ取込_入出庫チェックのみ'
          #'2'# ?PGNM := 'ＥＸＣＥＬ取込_入出庫チェック・更新'
          #'3'# ?PGNM := 'ＥＸＣＥＬ取込_作業系チェックのみ'
          #'4'# ?PGNM := 'ＥＸＣＥＬ取込_作業系チェック・更新'
          #'5'# ?PGNM := 'ＥＸＣＥＬ取込_作業系２チェックのみ'
          #'6'# ?PGNM := 'ＥＸＣＥＬ取込_作業系２チェック・更新'
          ELSE  SNDMSG   '！！！！！！！！！！！！！！！！！！',
                          TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
                SNDMSG   '！実行区分を指定し、実行して下さい！',
                          TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
                SNDMSG   '！！！！！！！！！！！！！！！！！！',
                          TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
                RETURN
    END

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=  'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              GOTO   ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0   THEN
              ?KEKA4 := '倉庫コード取得'
              GOTO   ABEND
    END

/.##使用ファイルＩＤ編集##./
FILNMCHG:

    ?STEP :=  'FILNMCHG'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.在庫EXCELデータ(入出庫系)./
    ?FILNM8   :=    ?FILNSK && ?SOKCD
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?ZAINSK   :=    %NCAT(?FILID,?LIBID)
    ?ZAINSKX  :=    %STRING(?ZAINSK)
    ?MSGX     :=    '## 連携ﾌｧｲﾙ名 = ' &&  ?ZAINSKX  &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.在庫EXCELデータ(作業系)./
    ?FILNM8   :=    ?FILSGY && ?SOKCD
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?ZAISGY   :=    %NCAT(?FILID,?LIBID)
    ?ZAISGYX  :=    %STRING(?ZAISGY)
    ?MSGX     :=    '## 連携ﾌｧｲﾙ名 = ' &&  ?ZAISGYX  &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.在庫EXCELデータ(作業系2)./
    ?FILNM8   :=    ?FILSBR && ?SOKCD
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?ZAISBR   :=    %NCAT(?FILID,?LIBID)
    ?ZAISBRX  :=    %STRING(?ZAISBR)
    ?MSGX     :=    '## 連携ﾌｧｲﾙ名 = ' &&  ?ZAISBRX  &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.在庫EXCEL取込ファイル(PF)./
    ?FILNM7   :=    ?FILWK  && ?SOKCD
    ?FILID    :=    %NAME(?FILNM7)
    ?LIBID    :=    %NAME(?LIBNM)
    ?ZAIWK    :=    %NCAT(?FILID,?LIBID)
    ?ZAIWKX   :=    %STRING(?ZAIWK)
    ?MSGX     :=    '## 連携ﾌｧｲﾙ名 = ' &&  ?ZAIWKX   &&  '  ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.在庫EXCEL取込ファイル(L1)./
    ?FILNM8   :=    ?FILWK  && ?SOKCD && '1'
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?ZAIWKL1  :=    %NCAT(?FILID,?LIBID)
    ?ZAIWKL1X :=    %STRING(?ZAIWKL1)
    ?MSGX     :=    '## 連携ﾌｧｲﾙ名 = ' &&  ?ZAIWKL1X &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.在庫EXCEL取込ファイル(L2)./
    ?FILNM8   :=    ?FILWK  && ?SOKCD && '2'
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?ZAIWKL2  :=    %NCAT(?FILID,?LIBID)
    ?ZAIWKL2X :=    %STRING(?ZAIWKL2)
    ?MSGX     :=    '## 連携ﾌｧｲﾙ名 = ' &&  ?ZAIWKL2X &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.在庫EXCEL取込ファイル(L3)./
    ?FILNM8   :=    ?FILWK  && ?SOKCD && '3'
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?ZAIWKL3  :=    %NCAT(?FILID,?LIBID)
    ?ZAIWKL3X :=    %STRING(?ZAIWKL3)
    ?MSGX     :=    '## 連携ﾌｧｲﾙ名 = ' &&  ?ZAIWKL3X &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##使用ファイルロック##./
ASSIGN:

    ?STEP :=  'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    /.在庫EXCELデータ./

    CASE  ?JIKKOU    OF
          #'1'# /.入出庫系_チェックのみ　　   ./
                /.　→在庫EXCELデータ(入出庫) ./

                ASSIGN FILE-?ZAINSK!@XCL
                ?PGMEC :=    @PGMEC
                ?PGMEM :=    @PGMEM
                ?PGMES :=    @PGMES
                IF     ?PGMEC ^=   1020 THEN
                   IF  ?PGMEC ^=   0    THEN
                       SNDMSG '他で連携処理が行われています',
                              TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                       ?KEKA4 :=  'ＥＸＣＥＬデータロック(入出庫)'
                       GOTO   ABEND1
                   END
                END

                CLRFILE FILE-?ZAINSK

          #'2'# /.入出庫系_チェック・更新     ./
                /.　→在庫EXCELデータ(入出庫) ./

                ASSIGN FILE-?ZAINSK!@XCL
                ?PGMEC :=    @PGMEC
                ?PGMEM :=    @PGMEM
                ?PGMES :=    @PGMES
                IF     ?PGMEC ^=   1020 THEN
                   IF  ?PGMEC ^=   0    THEN
                       SNDMSG '他で連携処理が行われています',
                              TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                       ?KEKA4 :=  'ＥＸＣＥＬデータロック(入出庫)'
                       GOTO   ABEND1
                   END
                END

                CLRFILE FILE-?ZAINSK

          #'3'# /.作業系_チェックのみ        ./
                /.　→在庫EXCELデータ(作業）　./

                ASSIGN FILE-?ZAISGY!@XCL
                ?PGMEC :=    @PGMEC
                ?PGMEM :=    @PGMEM
                ?PGMES :=    @PGMES
                IF     ?PGMEC ^=   1020 THEN
                   IF  ?PGMEC ^=   0    THEN
                       SNDMSG '他で連携処理が行われています',
                              TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                       ?KEKA4 :=  'ＥＸＣＥＬデータロック(作業)'
                       GOTO   ABEND1
                   END
                END

                CLRFILE FILE-?ZAISGY

          #'4'# /.作業系_チェック・更新      ./
                /.　→在庫EXCELデータ(作業）　./

                ASSIGN FILE-?ZAISGY!@XCL
                ?PGMEC :=    @PGMEC
                ?PGMEM :=    @PGMEM
                ?PGMES :=    @PGMES
                IF     ?PGMEC ^=   1020 THEN
                   IF  ?PGMEC ^=   0    THEN
                       SNDMSG '他で連携処理が行われています',
                              TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                       ?KEKA4 :=  'ＥＸＣＥＬデータロック(作業)'
                       GOTO   ABEND1
                   END
                END

                CLRFILE FILE-?ZAISGY

          #'5'# /.作業系2_チェックのみ        ./
                /.　→在庫EXCELデータ(作業2） ./

                ASSIGN FILE-?ZAISBR!@XCL
                ?PGMEC :=    @PGMEC
                ?PGMEM :=    @PGMEM
                ?PGMES :=    @PGMES
                IF     ?PGMEC ^=   1020 THEN
                   IF  ?PGMEC ^=   0    THEN
                       SNDMSG '他で連携処理が行われています',
                              TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                       ?KEKA4 :=  'ＥＸＣＥＬデータロック(作業２)'
                       GOTO   ABEND1
                   END
                END

                CLRFILE FILE-?ZAISBR

          #'6'# /.作業系2_チェック・更新      ./
                /.　→在庫EXCELデータ(作業2） ./

                ASSIGN FILE-?ZAISBR!@XCL
                ?PGMEC :=    @PGMEC
                ?PGMEM :=    @PGMEM
                ?PGMES :=    @PGMES
                IF     ?PGMEC ^=   1020 THEN
                   IF  ?PGMEC ^=   0    THEN
                       SNDMSG '他で連携処理が行われています',
                              TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                       ?KEKA4 :=  'ＥＸＣＥＬデータロック(作業２)'
                       GOTO   ABEND1
                   END
                END

                CLRFILE FILE-?ZAISBR
    END

    /.在庫EXCEL取込ファイル　全実行区分共通./

    ASSIGN FILE-?ZAIWK!@XCL
    ?PGMEC :=    @PGMEC
    ?PGMEM :=    @PGMEM
    ?PGMES :=    @PGMES
    IF     ?PGMEC ^=   1020 THEN
       IF  ?PGMEC ^=   0    THEN
           SNDMSG '他で連携処理が行われています',
                   TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
           ?KEKA4 :=  'ＥＸＣＥＬ取込ファイルロック'
           GOTO   ABEND1
       END
    END

/.##取込確認画面##./
SHOM0900:

    ?STEP :=  'SHOM0900'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE      ?JIKKOU    OF
       #'1'#
           ?OPR1 := '＜入出庫：ＥＸＣＥＬデータ取込（チェック）＞'
           ?OPR2 := ''
           ?OPR3 := '　在庫ＥＸＣＥＬデータの取り込みを行います。'
           ?OPR4 := '　　・連携するデータは出力済ですか？'
           ?OPR5 := '　　・所定フォルダにセット済ですか？'
       #'2'#
           ?OPR1 := '＜入出庫：ＥＸＣＥＬデータ取込（　更　新　）＞'
           ?OPR2 := '　　　　　！！確認して下さい！！'
           ?OPR3 := '　在庫ＥＸＣＥＬデータの取込＆更新を行います'
           ?OPR4 := '　・在庫更新まで実施されるため注意願います。'
           ?OPR5 := '　・チェック処理はエラーなく終了していますか？'
       #'3'#
           ?OPR1 := '＜作　業：ＥＸＣＥＬデータ取込（チェック）＞'
           ?OPR2 := ''
           ?OPR3 := '　在庫ＥＸＣＥＬデータの取り込みを行います。'
           ?OPR4 := '　　・連携するデータは出力済ですか？'
           ?OPR5 := '　　・所定フォルダにセット済ですか？'
       #'4'#
           ?OPR1 := '＜作　業：ＥＸＣＥＬデータ取込（　更　新　）＞'
           ?OPR2 := '　　　　　！！確認して下さい！！'
           ?OPR3 := '　在庫ＥＸＣＥＬデータの取込＆更新を行います'
           ?OPR4 := '　・在庫更新まで実施されるため注意願います。'
           ?OPR5 := '　・チェック処理はエラーなく終了していますか？'
       #'5'#
           ?OPR1 := '＜作業２：ＥＸＣＥＬデータ取込（チェック）＞'
           ?OPR2 := ''
           ?OPR3 := '　在庫ＥＸＣＥＬデータの取り込みを行います。'
           ?OPR4 := '　　・連携するデータは出力済ですか？'
           ?OPR5 := '　　・所定フォルダにセット済ですか？'
       #'6'#
           ?OPR1 := '＜作業２：ＥＸＣＥＬデータ取込（　更　新　）＞'
           ?OPR2 := '　　　　　！！確認して下さい！！'
           ?OPR3 := '　在庫ＥＸＣＥＬデータの取込＆更新を行います'
           ?OPR4 := '　・在庫更新まで実施されるため注意願います。'
           ?OPR5 := '　・チェック処理はエラーなく終了していますか？'
    END

    CALL   OHOM0900.TOKELIB,
           PARA-(?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '処理確認画面'
              GOTO   ABEND
    END

/.##ＥＸＣＥＬデータ取込処理##./
PFEXPORT:

    ?STEP :=  'PFEXPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE  ?JIKKOU  OF
          #'1'# /.入出庫系_チェックのみ　　   ./
                /.　→在庫EXCELデータ(入出庫) ./
                FEXPORT FILE-?ZAINSK,
                        MODE-@REP,
                        PARA-ZAIEXCEL,
                        UNIT-1
                ?PGMEC  := @PGMEC
                ?PGMES  := @PGMES
                IF      ?PGMEC ^= 0    THEN
                        ?KEKA4 := 'ＥＸＣＥＬ（入出庫）取込異常！！'
                        GOTO   ABEND
                END
          #'2'# /.入出庫系_チェック・更新　   ./
                /.　→在庫EXCELデータ(入出庫) ./
                FEXPORT FILE-?ZAINSK,
                        MODE-@REP,
                        PARA-ZAIEXCEL,
                        UNIT-1
                ?PGMEC  := @PGMEC
                ?PGMES  := @PGMES
                IF      ?PGMEC ^= 0    THEN
                        ?KEKA4 := 'ＥＸＣＥＬ（入出庫）取込異常！！'
                        GOTO   ABEND
                END
          #'3'# /.作業系_チェックのみ　　　   ./
                /.　→在庫EXCELデータ(作業）　./
                FEXPORT FILE-?ZAISGY,
                        MODE-@REP,
                        PARA-ZAIEXCEL,
                        UNIT-2
                ?PGMEC  := @PGMEC
                ?PGMES  := @PGMES
                IF      ?PGMEC ^= 0    THEN
                        ?KEKA4 := 'ＥＸＣＥＬ（作　業）取込異常！！'
                        GOTO   ABEND
                END
          #'4'# /.作業系_チェック・更新　　   ./
                /.　→在庫EXCELデータ(作業)   ./
                FEXPORT FILE-?ZAISGY,
                        MODE-@REP,
                        PARA-ZAIEXCEL,
                        UNIT-2
                ?PGMEC  := @PGMEC
                ?PGMES  := @PGMES
                IF      ?PGMEC ^= 0    THEN
                        ?KEKA4 := 'ＥＸＣＥＬ（作　業）取込異常！！'
                        GOTO   ABEND
                END
          #'5'# /.作業系2_チェックのみ　　　   ./
                /.　→在庫EXCELデータ(作業2）　./
                FEXPORT FILE-?ZAISBR,
                        MODE-@REP,
                        PARA-ZAIEXCEL,
                        UNIT-3
                ?PGMEC  := @PGMEC
                ?PGMES  := @PGMES
                IF      ?PGMEC ^= 0    THEN
                        ?KEKA4 := 'ＥＸＣＥＬ（作業２）取込異常！！'
                        GOTO   ABEND
                END
          #'6'# /.作業系2_チェック・更新　　 ./
                /.　→在庫EXCELデータ(作業2) ./
                FEXPORT FILE-?ZAISBR,
                        MODE-@REP,
                        PARA-ZAIEXCEL,
                        UNIT-3
                ?PGMEC  := @PGMEC
                ?PGMES  := @PGMES
                IF      ?PGMEC ^= 0    THEN
                        ?KEKA4 := 'ＥＸＣＥＬ（作業２）取込異常！！'
                        GOTO   ABEND
                END
    END

/.##在庫ＥＸＣＥＬ取込ファイル　初期化##./
PCLRFILE:

    ?STEP :=  'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CLRFILE FILE-?ZAIWK
    ?PGMEC  :=    @PGMEC
    ?PGMEM  :=    @PGMEM
    ?PGMES  :=    @PGMES
    IF      ?PGMEC ^=   0    THEN
            SNDMSG 'ファイルクリアに失敗しました',TOWS-@ORGWS,
                   JLOG-@YES,SLOG-@YES
            ?KEKA4 :=  'ＥＸＣＥＬ取込ファイルクリア'
            GOTO ABEND
    END

/.##在庫ＥＸＣＥＬデータチェック##./
SZI00XX1:

    ?STEP :=  'SZI00XX1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE  ?JIKKOU  OF
          #'1'# /.入出庫系_チェックのみ　　   ./
                /.　→在庫EXCELデータチェック(入出庫) ./
                OVRF   FILE-ZAINSKXX,TOFILE-?ZAINSK
                OVRF   FILE-ZAIWKXX,TOFILE-?ZAIWK
                CALL   PGM-SZI0010B.TOKELIBO,
                       PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD,
                             ?CNT1,?CNT2,?CNT3,?CNT4,?CNT5,?CNT8)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF        ?PGMEC ^= 0    THEN
                          ?KEKA4 := 'データチェック（入出庫）'
                          GOTO   ABEND
                END
          #'2'# /.入出庫系_チェック・更新　   ./
                /.　→在庫EXCELデータチェック(入出庫) ./
                OVRF   FILE-ZAINSKXX,TOFILE-?ZAINSK
                OVRF   FILE-ZAIWKXX,TOFILE-?ZAIWK
                CALL   PGM-SZI0010B.TOKELIBO,
                       PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD,
                             ?CNT1,?CNT2,?CNT3,?CNT4,?CNT5,?CNT8)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF        ?PGMEC ^= 0    THEN
                          ?KEKA4 := 'データチェック（入出庫）'
                          GOTO   ABEND
                END
          #'3'# /.作業系_チェックのみ　　   ./
                /.　→在庫EXCELデータチェック(作業) ./
                OVRF   FILE-ZAISGYXX,TOFILE-?ZAISGY
                OVRF   FILE-ZAIWKXX,TOFILE-?ZAIWK
                CALL   PGM-SZI0020B.TOKELIBO,
                       PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD,
                             ?CNT1,?CNT2,?CNT3,?CNT4,?CNT5,?CNT8)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF        ?PGMEC ^= 0    THEN
                          ?KEKA4 := 'データチェック（作業）'
                          GOTO   ABEND
                END
          #'4'# /.作業系_チェック・更新　   ./
                /.　→在庫EXCELデータチェック(作業) ./
                OVRF   FILE-ZAISGYXX,TOFILE-?ZAISGY
                OVRF   FILE-ZAIWKXX,TOFILE-?ZAIWK
                CALL   PGM-SZI0020B.TOKELIBO,
                       PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD,
                             ?CNT1,?CNT2,?CNT3,?CNT4,?CNT5,?CNT8)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF        ?PGMEC ^= 0    THEN
                          ?KEKA4 := 'データチェック（作業）'
                          GOTO   ABEND
                END
          #'5'# /.作業系2_チェックのみ　　   ./
                /.　→在庫EXCELデータチェック(作業2) ./
                OVRF   FILE-ZAISBRXX,TOFILE-?ZAISBR
                OVRF   FILE-ZAIWKXX3,TOFILE-?ZAIWKL3
                CALL   PGM-SZI0021B.TOKELIBO,
                       PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD,
                             ?CNT1,?CNT2,?CNT3,?CNT4,?CNT5,
                             ?CNT6,?CNT7,?CNT8)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF        ?PGMEC ^= 0    THEN
                          ?KEKA4 := 'データチェック（作業２）'
                          GOTO   ABEND
                END
          #'6'# /.作業系2_チェック・更新　   ./
                /.　→在庫EXCELデータチェック(作業2) ./
                OVRF   FILE-ZAISBRXX,TOFILE-?ZAISBR
                OVRF   FILE-ZAIWKXX3,TOFILE-?ZAIWKL3
                CALL   PGM-SZI0021B.TOKELIBO,
                       PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD,
                             ?CNT1,?CNT2,?CNT3,?CNT4,?CNT5,
                             ?CNT6,?CNT7,?CNT8)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF        ?PGMEC ^= 0    THEN
                          ?KEKA4 := 'データチェック（作業２）'
                          GOTO   ABEND
                END
    END

/.##データ件数確認##./
SZI0030I:

    ?STEP :=  'SZI0030I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIBO

    CALL      PGM-SZI0030I.TOKELIBO,
              PARA-(?JIKKOU,?BUMON,?TANCD,
                    ?CNT2,?CNT3,?CNT4,?CNT5,?CNT6,?CNT7,?CNT8)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := 'データ件数確認'
              GOTO   ABEND
    END

    IF  ?CNT8 ^=  '0000000'  THEN   /.##エラーがあった場合##./
     /. ?MSGX :=  '##################################'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　エラーチェックにおいてエラー　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　箇所があります。エラーリスト　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　を確認し修正／再取込を行って　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　下さい。　　　　　　　　　　　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '##################################'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES./

        GOTO  SZI0050L

    END

/.##在庫ＥＸＣＥＬ取込件数リスト##./
SZI0040L:

    ?STEP :=  'SZI0040L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CALL      PGM-SZI0040L.TOKELIBO,
              PARA-(?JIKKOU,?BUMON,?TANCD,
                    ?CNT1,?CNT2,?CNT3,?CNT4,?CNT5,?CNT6,?CNT7)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '取込件数リスト'
              GOTO   ABEND
    END

    IF  ?JIKKOU  =  '1'  THEN   /.##実行区分：チェック##./
        ?MSGX :=  '##################################'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　チェックのみを行ないました。　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　チェックＯＫの場合は、更新処　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　理を開始して下さい。　　　　　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '##################################'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        GOTO  RTN
    END

    IF  ?JIKKOU  =  '3'  THEN   /.##実行区分：チェック##./
        ?MSGX :=  '##################################'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　チェックのみを行ないました。　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　チェックＯＫの場合は、更新処　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　理を開始して下さい。　　　　　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '##################################'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        GOTO  RTN
    END

    IF  ?JIKKOU  =  '5'  THEN   /.##実行区分：チェック##./
        ?MSGX :=  '##################################'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　チェックのみを行ないました。　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　チェックＯＫの場合は、更新処　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '#　理を開始して下さい。　　　　　#'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        ?MSGX :=  '##################################'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
        GOTO  RTN
    END

    IF  ?JIKKOU  =  '2'  THEN   /.##実行区分：チェック・更新##./
        GOTO  SZI00XX2
    END

    IF  ?JIKKOU  =  '4'  THEN   /.##実行区分：チェック・更新##./
        GOTO  SZI00XX2
    END

    IF  ?JIKKOU  =  '6'  THEN   /.##実行区分：チェック・更新##./
        GOTO  SZI00XX2
    END

/.##在庫ＥＸＣＥＬ取込エラーリスト##./
SZI0050L:

    ?STEP :=  'SZI0050L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF      FILE-ZAIWKXX1,TOFILE-?ZAIWKL1
    CALL      PGM-SZI0050L.TOKELIBO,
              PARA-(?BUMON,?TANCD)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '取込エラーリスト'
              GOTO   ABEND
    END

    GOTO      RTN       /.エラーありの場合→終了./

/.##データ更新処理##./
SZI00XX2:

    ?STEP :=  'SZI00XX2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE  ?JIKKOU  OF

          #'2'# /.入出庫系_チェック・更新　   ./
                /.　→データ更新(入出庫)      ./
                OVRF   FILE-ZAIWKXX2,TOFILE-?ZAIWKL2
                OVRF FILE-NYSFILL1,TOFILE-NYSFILL1.D365DLIB
                CALL   PGM-SZI0060B.TOKELIBO,
                       PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF     ?PGMEC ^= 0    THEN
                       ?KEKA4 := 'データ更新（入出庫）'
                       GOTO   ABEND
                END

          #'4'# /.作業系_チェック・更新　   ./
                /.　→データ更新(作業)      ./
                OVRF   FILE-ZAIWKXX2,TOFILE-?ZAIWKL2
                CALL   PGM-SZI0070B.TOKELIBO,
                       PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF     ?PGMEC ^= 0    THEN
                       ?KEKA4 := 'データ更新（作業）'
                       GOTO   ABEND
                END

          #'6'# /.作業系2_チェック・更新　   ./
                /.　→データ更新(作業2)      ./
                OVRF   FILE-ZAIWKXX1,TOFILE-?ZAIWKL1
                CALL   PGM-SZI0071B.TOKELIBO,
                       PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF     ?PGMEC ^= 0    THEN
                       ?KEKA4 := 'データ更新（作業）'
                       GOTO   ABEND
                END
    END

RTN:

    RELEASE FILE-?ZAINSK!@XCL/?ZAISGY!@XCL/?ZAISBR!@XCL/?ZAIWK!@XCL

    CASE ?JIKKOU OF
       #'1'#  ?KEKA1 :=  '在庫ＥＸＣＥＬデータ取込処理が'
              ?KEKA2 :=  '正常終了しました。'
              ?KEKA3 :=  '（入出庫：チェックのみ）'
              ?KEKA4 :=  ''
       #'2'#  ?KEKA1 :=  '在庫ＥＸＣＥＬデータ取込処理が'
              ?KEKA2 :=  '正常終了しました。'
              ?KEKA3 :=  '（入出庫：チェック・更新）'
              ?KEKA4 :=  ''
       #'3'#  ?KEKA1 :=  '在庫ＥＸＣＥＬデータ取込処理が'
              ?KEKA2 :=  '正常終了しました。'
              ?KEKA3 :=  '（作業：チェックのみ）'
              ?KEKA4 :=  ''
       #'4'#  ?KEKA1 :=  '在庫ＥＸＣＥＬデータ取込処理が'
              ?KEKA2 :=  '正常終了しました。'
              ?KEKA3 :=  '（作業：チェック・更新）'
              ?KEKA4 :=  ''
       #'5'#  ?KEKA1 :=  '在庫ＥＸＣＥＬデータ取込処理が'
              ?KEKA2 :=  '正常終了しました。'
              ?KEKA3 :=  '（作業２：チェックのみ）'
              ?KEKA4 :=  ''
       #'6'#  ?KEKA1 :=  '在庫ＥＸＣＥＬデータ取込処理が'
              ?KEKA2 :=  '正常終了しました。'
              ?KEKA3 :=  '（作業２：チェック・更新）'
              ?KEKA4 :=  ''
    END

    IF        ?CNT8  ^=  '0000000'  THEN
              ?KEKA1 :=  'エラーチェックにおいてエラー　　　　　'
              ?KEKA2 :=  '箇所があります。エラーリスト'
              ?KEKA3 :=  'を確認し修正／再取込を行って'
              ?KEKA4 :=  '下さい。'
    END

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    RELEASE FILE-?ZAINSK!@XCL/?ZAISGY!@XCL/?ZAISBR!@XCL/?ZAIWK!@XCL

    ?KEKA1 :=  '在庫ＥＸＣＥＬデータ取込処理が'
    ?KEKA2 :=  '異常終了しました。'
    ?KEKA3 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO    ?MSGX  := ?MSG(?I)
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    END

    RETURN    PGMEC-?PGMEC

ABEND1:

    RELEASE FILE-?ZAINSK!@XCL/?ZAISGY!@XCL/?ZAISBR!@XCL/?ZAIWK!@XCL

    ?KEKA1 :=  '在庫ＥＸＣＥＬデータ取込処理関係ファイ'
    ?KEKA2 :=  'ルが、他で使用されています。'
    ?KEKA3 :=  '他端末で使用中でないか確認して下さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO    ?MSGX  := ?MSG(?I)
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    END

    RETURN    PGMEC-?PGMEC
