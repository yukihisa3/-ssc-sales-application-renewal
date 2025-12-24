/. ***********************************************************  ./
/. *  （株）サカタのタネ                                     *  ./
/. *   SYSTEM-NAME :    社内振替　　　　　　　　　　　       *  ./
/. *   JOB-NAME    :    社内振替発注EXCELチェック・取込      *  ./
/. *   JOB-ID      :    PFU30000                             *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM  (P1-?JIKKOU)

/.##INパラメタ##./
    PARA  ?JIKKOU  ,STRING*1,IN,VALUE-' '
            /.実行区分                               ./
            /.  '1'    :   チェックのみ     ./
            /.  '2'    :   チェック・取込   ./

/.--------------------------------------------------------------./
    VAR  ?WS      ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?WKSTN   ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?PGMEC   ,INTEGER
    VAR  ?PGMES   ,STRING*5,VALUE-'     '
    VAR  ?PGMECX  ,STRING*11
    VAR  ?PGMEM   ,STRING*99
    VAR  ?MSG     ,STRING*99(6)
    VAR  ?MSGX    ,STRING*99
    VAR  ?PGMID   ,STRING*8,VALUE-'PFU30000'
    VAR  ?STEP    ,STRING*8
/.--------------------------------------------------------------./
    VAR  ?BUMON   ,STRING*4,VALUE-'    '     /.部門./
    VAR  ?TANCD8  ,STRING*8,VALUE-'        ' /.部門+担当者./
    VAR  ?TANCD   ,STRING*2,VALUE-'  '       /.担当者     ./
    VAR  ?SOKCD   ,STRING*2,VALUE-'00'       /.実行倉庫   ./
    VAR  ?DSOKCD  ,STRING*2,VALUE-'00'       /.代表倉庫   ./
/.--------------------------------------------------------------./
    VAR  ?CNT1    ,STRING*7,VALUE-'0000000'  /.件数(取込) ./
    VAR  ?CNT2    ,STRING*7,VALUE-'0000000'  /.件数(ＥＲ）./
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
   /.##ファイルＩＤ編集用##./
    VAR  ?FILCSV  ,STRING*8,VALUE-'SFRWKCSV' /.EXCELデータ ./
    VAR  ?FILWK   ,STRING*7,VALUE-'SFRWKPF'  /.EXCEL取込データ ./
    VAR  ?FILHED  ,STRING*7,VALUE-'SFRHEDF'  /.社内振替情報Ｆ./
    VAR  ?FILMEI  ,STRING*7,VALUE-'SFRMEIF'  /.社内振替明細情報Ｆ ./
    VAR  ?LIBNM   ,STRING*8,VALUE-'TOKDTLIB' /.ライブラリ./

    VAR   ?FILNM7  ,STRING*7,VALUE-'       '
    VAR   ?FILNM8  ,STRING*8,VALUE-'        '
    VAR   ?FILID   ,NAME
    VAR   ?LIBID   ,NAME

    VAR   ?WKCSV   ,NAME!MOD
    VAR   ?WKCSVX  ,STRING*17

    VAR   ?WKPF    ,NAME!MOD
    VAR   ?WKPFX   ,STRING*17

    VAR   ?HEDF    ,NAME!MOD
    VAR   ?HEDFX   ,STRING*17

    VAR   ?MEIF    ,NAME!MOD
    VAR   ?MEIFX   ,STRING*17

/.-----------------------------------------------------------./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKDTLIB/TOKDLIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
CLSTART:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    CASE  ?JIKKOU     OF
          #'1'# ?PGNM := 'ＥＸＣＥＬ取込_チェックのみ'
          #'2'# ?PGNM := 'ＥＸＣＥＬ取込_チェック・取込'
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

   /.EXCELデータ./
    ?FILNM8   :=    ?FILCSV
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?WKCSV    :=    %NCAT(?FILID,?LIBID)
    ?WKCSVX   :=    %STRING(?WKCSV)
    ?MSGX     :=    '## 連携ﾌｧｲﾙ名 = ' &&  ?WKCSVX   &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.EXCEL取込ファイル(PF)./
    ?FILNM7   :=    ?FILWK
    ?FILID    :=    %NAME(?FILNM7)
    ?LIBID    :=    %NAME(?LIBNM)
    ?WKPF     :=    %NCAT(?FILID,?LIBID)
    ?WKPFX    :=    %STRING(?WKPF)
    ?MSGX     :=    '## 取込ﾌｧｲﾙ名 = ' &&  ?WKPFX    &&  '  ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.社内振替情報ファイル(PF)./
    ?FILNM7   :=    ?FILHED
    ?FILID    :=    %NAME(?FILNM7)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HEDF     :=    %NCAT(?FILID,?LIBID)
    ?HEDFX    :=    %STRING(?HEDF)
    ?MSGX     :=    '## 振替ﾌｧｲﾙ名 = ' &&  ?HEDFX    &&  '  ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.社内振替明細情報ファイル(PF)./
    ?FILNM7   :=    ?FILMEI
    ?FILID    :=    %NAME(?FILNM7)
    ?LIBID    :=    %NAME(?LIBNM)
    ?MEIF     :=    %NCAT(?FILID,?LIBID)
    ?MEIFX    :=    %STRING(?MEIF)
    ?MSGX     :=    '## 明細ﾌｧｲﾙ名 = ' &&  ?MEIFX   &&  '  ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##使用ファイルロック##./
ASSIGN:

    ?STEP :=  'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    /.在庫EXCELデータ./

    CASE  ?JIKKOU    OF
          #'1'# /.チェックのみ　　   ./

                ASSIGN FILE-?WKCSV!@XCL
                ?PGMEC :=    @PGMEC
                ?PGMEM :=    @PGMEM
                ?PGMES :=    @PGMES
                IF     ?PGMEC ^=   1020 THEN
                   IF  ?PGMEC ^=   0    THEN
                       SNDMSG '他で連携処理が行われています',
                              TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                       ?KEKA4 :=  'ＥＸＣＥＬデータロック'
                       GOTO   ABEND1
                   END
                END

                CLRFILE FILE-?WKCSV

                ASSIGN FILE-?WKPF!@XCL
                ?PGMEC :=    @PGMEC
                ?PGMEM :=    @PGMEM
                ?PGMES :=    @PGMES
                IF     ?PGMEC ^=   1020 THEN
                   IF  ?PGMEC ^=   0    THEN
                       SNDMSG '他で連携処理が行われています',
                              TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                       ?KEKA4 :=  'ＥＸＣＥＬ取込データロック'
                       GOTO   ABEND1
                   END
                END

          #'2'# /.チェック・取込     ./

                ASSIGN FILE-?WKCSV!@XCL
                ?PGMEC :=    @PGMEC
                ?PGMEM :=    @PGMEM
                ?PGMES :=    @PGMES
                IF     ?PGMEC ^=   1020 THEN
                   IF  ?PGMEC ^=   0    THEN
                       SNDMSG '他で連携処理が行われています',
                              TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                       ?KEKA4 :=  'ＥＸＣＥＬデータロック'
                       GOTO   ABEND1
                   END
                END

                CLRFILE FILE-?WKCSV

                ASSIGN FILE-?WKPF!@XCL
                ?PGMEC :=    @PGMEC
                ?PGMEM :=    @PGMEM
                ?PGMES :=    @PGMES
                IF     ?PGMEC ^=   1020 THEN
                   IF  ?PGMEC ^=   0    THEN
                       SNDMSG '他で連携処理が行われています',
                              TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                       ?KEKA4 :=  'ＥＸＣＥＬ取込データロック'
                       GOTO   ABEND1
                   END
                END

                ASSIGN FILE-?HEDF!@XCL
                ?PGMEC :=    @PGMEC
                ?PGMEM :=    @PGMEM
                ?PGMES :=    @PGMES
                IF     ?PGMEC ^=   1020 THEN
                   IF  ?PGMEC ^=   0    THEN
                       SNDMSG '他で連携処理が行われています',
                              TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
                       ?KEKA4 :=  '社内振替データロック'
                       GOTO   ABEND1
                   END
                END
    END

/.##取込確認画面##./
SHOM0900:

    ?STEP :=  'SHOM0900'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE      ?JIKKOU    OF
       #'1'#
           ?OPR1 := '＜社内振替ＥＸＣＥＬデータ取込（チェック）＞'
           ?OPR2 := ''
           ?OPR3 := '　振替ＥＸＣＥＬデータのチェックを行います。'
           ?OPR4 := '　　・連携するデータは出力済ですか？'
           ?OPR5 := '　　・所定フォルダにセット済ですか？'
       #'2'#
           ?OPR1 := '＜社内振替ＥＸＣＥＬデータ取込（　更　新　）＞'
           ?OPR2 := '　　　　　！！確認して下さい！！'
           ?OPR3 := '　振替ＥＸＣＥＬデータの　　　　　　　　　　'
           ?OPR4 := '　　　　　　チェック～更新まで行います。　　'
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
          #'1'# /.チェックのみ　　   ./
                FEXPORT FILE-?WKCSV,
                        MODE-@REP,
                        PARA-SFURIKAE,
                        UNIT-1
                ?PGMEC  := @PGMEC
                ?PGMES  := @PGMES
                IF      ?PGMEC ^= 0    THEN
                        ?KEKA4 := '振替ＥＸＣＥＬ取込異常！！'
                        GOTO   ABEND
                END
          #'2'# /.チェック・取込　   ./
                FEXPORT FILE-?WKCSV,
                        MODE-@REP,
                        PARA-SFURIKAE,
                        UNIT-1
                ?PGMEC  := @PGMEC
                ?PGMES  := @PGMES
                IF      ?PGMEC ^= 0    THEN
                        ?KEKA4 := '振替ＥＸＣＥＬ取込異常！！'
                        GOTO   ABEND
                END
    END

/.##在庫ＥＸＣＥＬ取込ファイル　初期化##./
PCLRFILE:

    ?STEP :=  'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CLRFILE FILE-?WKPF
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
SFU3000B:

    ?STEP :=  'SFU3000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE  ?JIKKOU  OF
          #'1'# /.チェックのみ　　   ./
                CALL   PGM-SFU3000B.TOKSOLIB,
                       PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD,
                             ?CNT1,?CNT2)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF        ?PGMEC ^= 0    THEN
                          ?KEKA4 := '社内振替データチェック'
                          GOTO   ABEND
                END
          #'2'# /.チェック・取込　   ./
                CALL   PGM-SFU3000B.TOKSOLIB,
                       PARA-(?SOKCD,?DSOKCD,?BUMON,?TANCD,
                             ?CNT1,?CNT2)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF        ?PGMEC ^= 0    THEN
                          ?KEKA4 := '社内振替データチェック'
                          GOTO   ABEND
                END
    END

/.##データ件数確認##./
SFU3010I:

    ?STEP :=  'SFU3010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKMDLIB

    CALL      PGM-SFU3010I.TOKSOLIB,
              PARA-(?JIKKOU,?BUMON,?TANCD,
                    ?CNT1,?CNT2)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := 'データ件数確認'
              GOTO   ABEND
    END

    IF  ?CNT2 ^=  '0000000'  THEN
        GOTO  CHECKNG               /.##エラーがある場合##./
    ELSE
        GOTO  CHECKOK               /.##エラーがない場合##./
    END

/.##エラーあり処理##./
CHECKNG:

    ?STEP :=  'CHECKNG'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    /.##チェック結果リスト##./
    CALL      PGM-SFU3021L.TOKSOLIB,
              PARA-(?JIKKOU,?BUMON,?TANCD)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := 'チェック結果リスト'
              GOTO   ABEND
    END

    GOTO      RTN       /.エラーありの場合→終了./

/.##エラーなし処理##./
CHECKOK:

    ?STEP :=  'CHECKOK'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    /.##チェック結果リスト##./
    CALL      PGM-SFU3021L.TOKSOLIB,
              PARA-(?JIKKOU,?BUMON,?TANCD)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := 'チェック結果リスト'
              GOTO   ABEND
    END

    IF  ?CNT1    =  '0000000'  THEN
        GOTO  RTN
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

    IF  ?JIKKOU  =  '2'  THEN   /.##実行区分：チェック・取込##./
        GOTO  SFU3030B
    END

/.##データ更新処理##./
SFU3030B:

    ?STEP :=  'SFU3030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE  ?JIKKOU  OF

          #'2'# /.チェック・更新　   ./
                CALL   PGM-SFU3030B.TOKSOLIB,
                       PARA-(?BUMON,?TANCD)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF     ?PGMEC ^= 0    THEN
                       ?KEKA4 := '社内振替データ取込'
                       GOTO   ABEND
                END

    END

RTN:

    RELEASE FILE-?WKCSV!@XCL/?WKPF!@XCL/?HEDF!@XCL/?MEIF!@XCL

    CASE ?JIKKOU OF
       #'1'#  ?KEKA1 :=  '振替ＥＸＣＥＬデータ処理が'
              ?KEKA2 :=  '正常終了しました。'
              ?KEKA3 :=  '（チェックのみ）'
              ?KEKA4 :=  ''
       #'2'#  ?KEKA1 :=  '振替ＥＸＣＥＬデータ処理が'
              ?KEKA2 :=  '正常終了しました。'
              ?KEKA3 :=  '（チェック・取込）'
              ?KEKA4 :=  ''
    END

    IF        ?CNT2  ^=  '0000000'  THEN
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

    RELEASE FILE-?WKCSV!@XCL/?WKPF!@XCL/?HEDF!@XCL/?MEIF!@XCL

    ?KEKA1 :=  '社内振替ＥＸＣＥＬデータ処理が'
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

    RELEASE FILE-?WKCSV!@XCL/?WKPF!@XCL/?HEDF!@XCL/?MEIF!@XCL

    ?KEKA1 :=  '社内振替ＥＸＣＥＬデータ処理関係ファイ'
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