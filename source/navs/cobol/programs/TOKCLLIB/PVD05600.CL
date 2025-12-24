/. ***********************************************************  ./
/. *  （株）サカタのタネ                                     *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹　　　　　　　　　　　       *  ./
/. *   JOB-NAME    :    新倉庫間移動指示取込チェック・更新   *  ./
/. *   JOB-ID      :    PVD05600  2022/08/31                 *  ./
/. *   UPDATE      :                                         *  ./
/. *                                                         *  ./
/. ***********************************************************  ./

    VAR  ?WS      ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?WKSTN   ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?PGMEC   ,INTEGER
    VAR  ?PGMES   ,STRING*5,VALUE-'     '
    VAR  ?PGMECX  ,STRING*11
    VAR  ?PGMEM   ,STRING*99
    VAR  ?MSG     ,STRING*99(6)
    VAR  ?MSGX    ,STRING*99
    VAR  ?PGMID   ,STRING*8,VALUE-'PVD05600'
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
    VAR  ?OUTKBN  ,STRING*1,VALUE-' '        /.出力種別   ./
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

  /.##端末別ファイルＩＤ編集用##./

    /.共通_ライブラリ名./
    VAR  ?LIBNM    ,STRING*8,VALUE-'TOKDTLIB'
    /.共通_ファイルＩＤワーク./
    VAR  ?FILID    ,NAME
    /.共通_ＬＩＢＩＤワーク./
    VAR  ?LIBID    ,NAME
    /.共通_端末番号./
    VAR  ?TANNO    ,STRING*03,VALUE-'   '

    /.データ_共通部_ファイルＩＤ先頭./
    VAR  ?IDO      ,STRING*3,VALUE-'IDO'
    /.データ_個別部_ファイルＩＤ末尾./
    VAR  ?CS       ,STRING*2,VALUE-'CS'
    VAR  ?WK       ,STRING*2,VALUE-'WK'
    VAR  ?W1       ,STRING*2,VALUE-'W1'
    VAR  ?W2       ,STRING*2,VALUE-'W2'
    VAR  ?W3       ,STRING*2,VALUE-'W3'

    /.データ_OVRF_ファイルＩＤ./
    VAR  ?IDOXXXCS ,NAME!MOD
    VAR  ?IDOXXXWK ,NAME!MOD
    VAR  ?IDOXXXW1 ,NAME!MOD
    VAR  ?IDOXXXW2 ,NAME!MOD
    VAR  ?IDOXXXW3 ,NAME!MOD

    /.ファイルＩＤ表示用ワーク./
    VAR  ?DSPFILID,STRING*17


    VAR  ?FILNM7  ,STRING*7,VALUE-'       '
    VAR  ?FILNM8  ,STRING*8,VALUE-'        '

/.-----------------------------------------------------------./


/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   D365DLIB/TOKELIB/TOKELIBO/TOKSOLIB/TOKDLIB/TOKFLIB
             /TOKKLIB/TOKDTLIB

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
CLSTART:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM := '倉庫間移動指示取込チェック＆更新'

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

/.##端末番号を取得##./
SBT0620B:

    ?STEP :=   'SBT0620B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SBT0620B.TOKELIBO,PARA-(?WS,?TANNO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND END
  /.?MSGX := '## 端末NO = ' && ?TANNO          ./
  /.SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES./

/.##端末別ファイルＩＤ編集##./
FILNMCHG:

    ?STEP :=  'FILNMCHG'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

   /.倉庫間移動指示ＣＳＶ_ＳＦ ./
    ?FILNM8   :=    ?IDO  && ?TANNO && ?CS
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?IDOXXXCS :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?IDOXXXCS)
    ?MSGX     :=    '##ﾒﾝﾃﾅﾝｽCSVﾃﾞｰﾀ =' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.倉庫間移動指示取込Ｆ_ＰＦ ./
    ?FILNM8   :=    ?IDO  && ?TANNO && ?WK
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?IDOXXXWK :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?IDOXXXWK)
    ?MSGX     :=    '##ﾒﾝﾃﾅﾝｽ取込F PF=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.倉庫間移動指示取込Ｆ_Ｌ１ ./
    ?FILNM8   :=    ?IDO  && ?TANNO && ?W1
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?IDOXXXW1 :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?IDOXXXW1)
    ?MSGX     :=    '##ﾒﾝﾃﾅﾝｽ取込F L1=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.倉庫間移動指示取込Ｆ_Ｌ２ ./
    ?FILNM8   :=    ?IDO  && ?TANNO && ?W2
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?IDOXXXW2 :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?IDOXXXW2)
    ?MSGX     :=    '##ﾒﾝﾃﾅﾝｽ取込F L2=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.倉庫間移動指示取込Ｆ_Ｌ３ ./
    ?FILNM8   :=    ?IDO  && ?TANNO && ?W3
    ?FILID    :=    %NAME(?FILNM8)
    ?LIBID    :=    %NAME(?LIBNM)
    ?IDOXXXW3 :=    %NCAT(?FILID,?LIBID)
    ?DSPFILID :=    %STRING(?IDOXXXW3)
    ?MSGX     :=    '##ﾒﾝﾃﾅﾝｽ取込F L3=' && ?DSPFILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##使用ファイルロック・クリア##./
ASSIGN:

    ?STEP :=  'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    ASSIGN FILE-?IDOXXXCS!@XCL
    ?PGMEC :=    @PGMEC
    ?PGMEM :=    @PGMEM
    ?PGMES :=    @PGMES
    IF     ?PGMEC ^=   1020 THEN
       IF  ?PGMEC ^=   0    THEN
           SNDMSG '連携処理が行えません',
                   TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
           ?KEKA4 :=  'ＣＳＶデータロック'
           GOTO   ABEND1
       END
    END

    ASSIGN FILE-?IDOXXXWK!@XCL
    ?PGMEC :=    @PGMEC
    ?PGMEM :=    @PGMEM
    ?PGMES :=    @PGMES
    IF     ?PGMEC ^=   0    THEN
           SNDMSG '連携処理が行えません',
                   TOWS-@ORGWS,JLOG-@YES,SLOG-@YES
           ?KEKA4 :=  '取込ファイルロック'
           GOTO   ABEND1
    END
    CLRFILE FILE-?IDOXXXWK

/.##取込確認画面##./
SHOM0900:

    ?STEP :=  'SHOM0900'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    ?OPR1 := '　＜　倉庫間移動指示　取込・更新　＞'
    ?OPR2 := '　　　　！！確認して下さい！！'
    ?OPR3 := '　 ＥＸＣＥＬデータの取込＆更新を行います'
    ?OPR4 := '　　　・連携するデータは出力済ですか？'
    ?OPR5 := '　　　・所定フォルダにセット済ですか？'

    CALL   OHOM0900.TOKELIB,
           PARA-(?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '処理確認画面'
              GOTO   ABEND
    END

/.##ＣＳＶデータ転送処理##./
PFEXPORT:

    ?STEP :=  'PFEXPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

                FEXPORT FILE-?IDOXXXCS,
                        MODE-@REP,
                        PARA-IDOXXXCS,
                        UNIT-1
                ?PGMEC  := @PGMEC
                ?PGMES  := @PGMES
                IF      ?PGMEC ^= 0    THEN
                        ?KEKA4 := '移動指示ＣＳＶ転送異常！！'
                        GOTO   ABEND
                END

SORT1:

    SORT      INFILE-?IDOXXXCS,INRL-134,INBF-30,
              OUTFILE-?IDOXXXCS,OUTBF-30,
              KEY-1!2!CA,
              KEY1-13!2!CA,
              KEY2-16!13!CA,
              KEY3-30!6!CA,
              KEY4-37!6!CA,
              RCDL-@DSP
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＳＯＲＴ処理'
              GOTO   ABEND
    END

/.##倉庫間移動指示取込チェック##./
NVD0560B:

    ?STEP :=  'NVD0560B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

                OVRF   FILE-IDOXXXCS,TOFILE-?IDOXXXCS
                OVRF   FILE-IDOXXXW1,TOFILE-?IDOXXXW1
                CALL   PGM-NVD0560B.TOKSOLIB,
                       PARA-(?BUMON,?TANCD,
                             ?CNT1,?CNT2)
                ?PGMEC := @PGMEC
                ?PGMES := @PGMES
                IF        ?PGMEC ^= 0    THEN
                          ?KEKA4 := 'データチェック'
                          GOTO   ABEND
                END

    IF  ?CNT1  =  '0000000'  THEN   /.CSVゼロ件時は終了./
        IF  ?CNT2  =  '0000000'  THEN
            SNDMSG 'ＥＸＣＥＬデータ０件です！',
                   ,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
            RETURN
        END
    END

    IF  ?CNT2 ^=  '0000000'  THEN   /.##エラーがあった場合##./
        GOTO  NVD0570L
    ELSE
        GOTO  NVD0580B
    END

/.##倉庫間移動指示取込チェックリスト(エラー分)##./
NVD0570L:

    ?STEP :=  'NVD0570L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    ?OUTKBN   :=  '2'
/.  IF  ?BUMON  =  '2920'  THEN
        OVRPRTF FILE-XU04LP,TOFILE-LBPPRT01.TOKELIBO
    ELSE
        OVRPRTF FILE-XU04LP,TOFILE-LBPPRTE1.TOKELIBO
    END
./
    OVRF      FILE-IDOXXXW1,TOFILE-?IDOXXXW1
    OVRF      FILE-IDOXXXW2,TOFILE-?IDOXXXW2
    CALL      PGM-NVD0570L.TOKSOLIB,
              PARA-(?BUMON,?TANCD,?OUTKBN)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := '取込エラーリスト'
              GOTO   ABEND
    END

    GOTO  RTN

/.##Ｄ３６５入出庫ファイル更新（倉庫間移動）##./
NVD0580B:

    ?STEP :=  'NVD0580B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF   FILE-IDOXXXW3,TOFILE-?IDOXXXW3
    CALL   PGM-NVD0580B.TOKSOLIB,
           PARA-(?BUMON,?TANCD)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC ^= 0    THEN
           ?KEKA4 := '入出庫ファイル更新'
           GOTO   ABEND
    END

RTN:

    RELEASE   FILE-?IDOXXXCS!@XCL/?IDOXXXWK!@XCL

    IF        ?CNT2   =  '0000000'  THEN
              ?KEKA1 :=  '※倉庫間移動指示'
              ?KEKA2 :=  '　ＥＸＣＥＬデータ取込処理が'
              ?KEKA3 :=  '　正常終了しました。'
              ?KEKA4 :=  '　（チェック・更新）'
    ELSE
              ?KEKA1 :=  '倉庫間移動指示　データチェック'
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

    RELEASE   FILE-?IDOXXXCS!@XCL/?IDOXXXWK!@XCL

    ?KEKA1 :=  '倉庫間移動指示取込・更新処理が'
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

    RELEASE   FILE-?IDOXXXCS!@XCL/?IDOXXXWK!@XCL

    ?KEKA1 :=  'ＥＸＣＥＬデータ取込処理関係ファイルが'
    ?KEKA2 :=  '他で使用されています。'
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