/. ***********************************************************  ./
/. *  （株）サカタのタネ                                     *  ./
/. *   SYSTEM-NAME :    ナフコ出荷支援　　　　　　　　       *  ./
/. *   JOB-NAME    :    作場・数量一括変更データ取込　　　   *  ./
/. *                    取込チェック・更新                   *  ./
/. *   JOB-ID      :    PSY51400  2019/02/18                 *  ./
/. *   UPDATE      :                                         *  ./
/. *                                                         *  ./
/. ***********************************************************  ./
    PGM  (P1-?JIKKOU)

/.##INパラメタ##./
    PARA  ?JIKKOU  ,STRING*1,IN,VALUE-' '
            /.実行区分                               ./
            /.  '1'    :   チェックのみ　　　　     ./
            /.  '2'    :   チェック・更新　　　　   ./

/.--------------------------------------------------------------./
    VAR  ?WS      ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?WKSTN   ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?PGMEC   ,INTEGER
    VAR  ?PGMES   ,STRING*5,VALUE-'     '
    VAR  ?PGMECX  ,STRING*11
    VAR  ?PGMEM   ,STRING*99
    VAR  ?MSG     ,STRING*99(6)
    VAR  ?MSGX    ,STRING*99
    VAR  ?PGMID   ,STRING*8,VALUE-'PSY51400'
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
   /.---なし-----------
    VAR  ?FILNSK  ,STRING*6,VALUE-'ZAINSK'     EXCELデータ(入出)
    VAR  ?FILSGY  ,STRING*6,VALUE-'ZAISGY'     EXCELデータ(作業)
    VAR  ?FILSBR  ,STRING*6,VALUE-'ZAISBR'     EXCELデータ(作業2)
    VAR  ?FILWK   ,STRING*5,VALUE-'ZAIWK'      EXCEL取込ファイル
    VAR  ?LIBNM   ,STRING*8,VALUE-'ZAIEXCEL'   ライブラリ

    VAR   ?FILNM7  ,STRING*7,VALUE-'       '
    VAR   ?FILNM8  ,STRING*8,VALUE-'        '
    VAR   ?FILID   ,NAME
    VAR   ?LIBID   ,NAME ------------ ./
/.---------------------------------------------------------------./
    /.端末別ファイルＩＤ編集用./
           /.端末番号./
    VAR       ?TANNO    ,STRING*03,VALUE-'   '

           /.一括変更CSVデータ./
    VAR       ?CHG      ,STRING*3,VALUE-'CHG'
    VAR       ?CSVF     ,STRING*1,VALUE-'F'
    VAR       ?CHGXXXF  ,NAME!MOD

           /.一括変更CSVデータ DUET受取用./
    VAR       ?CSVR     ,STRING*1,VALUE-'R'
    VAR       ?CHGXXXR  ,NAME!MOD

           /.SORT用ワーク./
    VAR       ?SORT     ,STRING*3,VALUE-'CHG'
    VAR       ?SORTF    ,STRING*1,VALUE-'S'
    VAR       ?CHGXXXS  ,NAME!MOD

           /.一括変更データ取込ワーク PF./
    VAR       ?CHGPF    ,STRING*3,VALUE-'CHG'
    VAR       ?CHGT     ,STRING*1,VALUE-'T'
    VAR       ?CHGXXXT  ,NAME!MOD

           /.一括変更データ取込ワーク LF./
    VAR       ?CHGLF    ,STRING*3,VALUE-'CHG'
    VAR       ?CHGT1    ,STRING*2,VALUE-'T1'
    VAR       ?CHGXXXT1 ,NAME!MOD

           /.一括変更データ取込ワーク LF./
    VAR       ?CHGT2    ,STRING*2,VALUE-'T2'
    VAR       ?CHGXXXT2 ,NAME!MOD

           /.ファイルＩＤ表示用./
    VAR       ?FILENAME ,STRING*17,VALUE-'                 '

    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*8,VALUE-'TOKDTLIB'
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME

/.-----------------------------------------------------------./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKSOLIB/TOKDLIB/TOKFLIB/TOKKLIB/TOKDTLIB

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
CLSTART:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    CASE  ?JIKKOU     OF
          #'1'# ?PGNM := '一括変更データ_チェックのみ'
          #'2'# ?PGNM := '一括変更データ_チェック・更新'
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

/.----------------------------------------------------------------./
/.##端末番号を取得##./
SBT0620B:

    ?STEP :=   'SBT0620B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SBT0620B.TOKELIBO,PARA-(?WS,?TANNO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX := '## 端末NO = ' && ?TANNO
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##端末別ファイルＩＤ取得##./
   /.一括変更CSVデータ./
    ?FILNM    :=    ?CHG  && ?TANNO && ?CSVF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CHGXXXF  :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?CHGXXXF)
    ?MSGX     :=    '##一括変更CSVデータ =' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.一括変更CSVデータ DUET受取用./
    ?FILNM    :=    ?CHG  && ?TANNO && ?CSVR
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CHGXXXR  :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?CHGXXXR)
    ?MSGX     :=    '##一括変更CSV受取用 =' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.SORT用ワーク  なし
    ?FILNM    :=    ?SORT && ?TANNO && ?SORTF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CHGXXXS  :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?CHGXXXS)
    ?MSGX     :=    '##ＳＯＲＴ用ワーク　=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES   ./

   /.一括変更データ出力ワーク PF./
    ?FILNM    :=    ?CHGPF && ?TANNO && ?CHGT
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CHGXXXT  :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?CHGXXXT)
    ?MSGX     :=    '##一括変更データＰＦ=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.一括変更データ出力ワーク LF1./
    ?FILNM    :=    ?CHGLF && ?TANNO && ?CHGT1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CHGXXXT1 :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?CHGXXXT1)
    ?MSGX     :=    '##一括変更データＬ１=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.一括変更データ出力ワーク LF2./
    ?FILNM    :=    ?CHGLF && ?TANNO && ?CHGT2
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CHGXXXT2 :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?CHGXXXT2)
    ?MSGX     :=    '##一括変更データＬ２=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

FILECHK:

    ASSIGN FILE-?CHGXXXF!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := 'ファイル排他獲得１'
           SNDMSG 'ファイル排他獲得１',TOWS-@ORGWS
           GOTO   ABEND1
    END

    ASSIGN FILE-?CHGXXXT!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := 'ファイル排他獲得２'
           SNDMSG 'ファイル排他獲得２',TOWS-@ORGWS
           GOTO   ABEND1
    END

    CLRFILE FILE-?CHGXXXT
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := 'ファイルクリア２'
           SNDMSG 'ファイルクリア２',TOWS-@ORGWS
           GOTO   ABEND1
    END

  /.なし
    DLTFILE FILE-?CHGXXXS
    CPYFILE FILE-?CHGXXXF,TOFILE-?CHGXXXS,CRTFILE-@YES
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := 'ファイル排他獲得２－１'
           SNDMSG 'ファイル排他獲得２－１',TOWS-@ORGWS
           GOTO   ABEND1
    END
    ASSIGN FILE-?CHGXXXS!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := 'ファイル排他獲得２－２'
           SNDMSG 'ファイル排他獲得２－２',TOWS-@ORGWS
           GOTO   ABEND1
    END ./


/.##取込確認画面##./
SHOM0900:

    ?STEP :=  'SHOM0900'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE      ?JIKKOU    OF
       #'1'#
           ?OPR1 := '＜作場・数量一括変更　　データチェック＞'
           ?OPR2 := ''
           ?OPR3 := '　一括変更データ　の取り込みを行います。'
           ?OPR4 := '　　・連携するデータは出力済ですか？'
           ?OPR5 := '　　・所定フォルダにセット済ですか？'
       #'2'#
           ?OPR1 := '＜作場・数量一括変更　【データ更新】　＞'
           ?OPR2 := '　　　　　！！確認して下さい！！'
           ?OPR3 := '　 一括変更データ　の取込＆更新を行います'
           ?OPR4 := '　・更新まで実施されるため注意願います。'
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

                FEXPORT FILE-?CHGXXXR,
                        MODE-@REP,
                        PARA-NFHACCSK,
                        UNIT-8
                ?PGMEC  := @PGMEC
                ?PGMES  := @PGMES
                IF      ?PGMEC ^= 0    THEN
                       ?KEKA4 := 'ＥＸＣＥＬ（一括変更）取込異常！！'
                        GOTO   ABEND
                END
CNVFILE:

    ?STEP :=  'CNVFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

                CNVFILE FILE-?CHGXXXR,
                        TOFILE-?CHGXXXF,
                        ADD-@NO
                ?PGMEC  := @PGMEC
                ?PGMES  := @PGMES
                IF      ?PGMEC ^= 0    THEN
                       ?KEKA4 := 'ＥＸＣＥＬ（一括変更）複写異常！！'
                        GOTO   ABEND
                END

/.##一括変更ＥＸＣＥＬデータチェック##./
SSY5140B:

    ?STEP :=  'SSY5140B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

                OVRF   FILE-CHGXXXF,TOFILE-?CHGXXXF
                OVRF   FILE-CHGXXXT1,TOFILE-?CHGXXXT1
                CALL   PGM-SSY5140B.TOKSOLIB,
                       PARA-(?BUMON,?TANCD,
                             ?CNT1,?CNT2)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF        ?PGMEC ^= 0    THEN
                          ?KEKA4 := 'データチェック'
                          GOTO   ABEND
                END


    IF  ?CNT2 ^=  '0000000'  THEN   /.##エラーがあった場合##./
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

        GOTO  SSY5150L

    END
    IF  ?CNT1  =  '0000000'  THEN        /.CSVゼロ件時は終了./
        IF  ?CNT2  =  '0000000'  THEN
            SNDMSG 'ＥＸＣＥＬデータ０件です！',
                   ,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
            RETURN
        END
    END

/.##ＥＸＣＥＬ取込チェックリスト##./
SSY5150L:

    ?STEP :=  'SSY5150L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF   FILE-CHGXXXT2,TOFILE-?CHGXXXT2
    CALL      PGM-SSY5150L.TOKSOLIB,
              PARA-(?BUMON,?TANCD)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '取込チェックリスト'
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


    IF  ?JIKKOU  =  '2'  THEN   /.##実行区分：チェック・更新##./
        IF  ?CNT2  =  '0000000'  THEN
             GOTO  SSY5160B
        ELSE
             GOTO  RTN
        END
    END

/.##データ更新処理##./
SSY5160B:

    ?STEP :=  'SSY5160B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    CASE  ?JIKKOU  OF

          #'2'# /.チェック・更新　   ./
                OVRF   FILE-CHGXXXT1,TOFILE-?CHGXXXT1
                CALL   PGM-SSY5160B.TOKSOLIB,
                       PARA-(?BUMON,?TANCD)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF     ?PGMEC ^= 0    THEN
                       ?KEKA4 := 'データ更新'
                       GOTO   ABEND
                END

    END

RTN:

    RELEASE FILE-?CHGXXXF!@XCL/?CHGXXXT!@XCL

    CASE ?JIKKOU OF
       #'1'#  ?KEKA1 :=  '※作場・数量一括変更'
              ?KEKA2 :=  '　ＥＸＣＥＬデータ取込処理が'
              ?KEKA3 :=  '　正常終了しました。'
              ?KEKA4 :=  '　（チェックのみ）'
       #'2'#  ?KEKA1 :=  '※作場・数量一括変更'
              ?KEKA2 :=  '　ＥＸＣＥＬデータ取込処理が'
              ?KEKA3 :=  '　正常終了しました。'
              ?KEKA4 :=  '　（チェック・更新）'
    END

    IF        ?CNT2  ^=  '0000000'  THEN
              ?KEKA1 :=  '作場・数量一括変更　　データチェック　'
              ?KEKA2 :=  '　エラー箇所があります　　　　　　　　'
              ?KEKA3 :=  '　チェックリストを確認し、修正／再取込'
              ?KEKA4 :=  '　を行って下さい。'
    END

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    RELEASE FILE-?CHGXXXF!@XCL/?CHGXXXT!@XCL

    ?KEKA1 :=  '作場・数量一括変更データ取込処理が'
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

    RELEASE FILE-?CHGXXXF!@XCL/?CHGXXXT!@XCL

    ?KEKA1 :=  'ＥＸＣＥＬデータ取込処理関係ファイ'
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