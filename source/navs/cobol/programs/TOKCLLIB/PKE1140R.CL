/. ***********************************************************  ./
/. *   サカタのタネ　　　　　　　　　　　　　　　　  　      *  ./
/. *   SYSTEM-NAME :    流通ＢＭＳ　　　　　　　　　　　　   *  ./
/. *   JOB-ID      :    PKE11400                             *  ./
/. *   JOB-NAME    :    出荷梱包送信データ作成　　　　　　   *  ./
/. *               :    　（カインズ店直）　　　　　　　　   *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PKE11400'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,NAME!MOD
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK3   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?OK1    ,STRING*2,VALUE-'  '        /.実行確認1./
    VAR       ?OK2    ,STRING*2,VALUE-'  '        /.実行確認2./
    VAR       ?OK3    ,STRING*2,VALUE-'  '        /.実行確認3./
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
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門名./
    VAR       ?TANCD8   ,STRING*8,VALUE-'        ' /.担当者CD./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者CD./
    VAR       ?WS ,STRING*8,VALUE-'        '       /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?SOKCD  ,STRING*2,VALUE-'  '         /.自倉庫ｺｰﾄﾞ./
    VAR       ?DSOKCD ,STRING*2,VALUE-'  '         /.代表倉庫ｺｰﾄﾞ./
    VAR       ?JDATE  ,STRING*8,VALUE-'20230605'   /.受信日付./
    VAR       ?JTIME  ,STRING*4,VALUE-'1125'       /.受信時間./
    VAR       ?TORICD ,STRING*8,VALUE-'00921084'   /.取引先./
    VAR       ?SOKO   ,STRING*2,VALUE-'94'         /.出荷場所./
    VAR       ?NOUDT  ,STRING*8,VALUE-'20230615'   /.納品日./
    VAR       ?BMNCD  ,STRING*4,VALUE-'0000'       /.部門./
    VAR       ?TNCD   ,STRING*2,VALUE-'00'         /.担当者　./
    VAR       ?NKEITAI,STRING*1,VALUE-'2'          /.納品形態./
                                                   /.1:TC 2:店直./
    VAR       ?DKIND  ,STRING*1,VALUE-'2'          /.データ種類./
                                                   /.1:出荷ＤＴ作成./
                                                   /.2:送信ＤＴ作成./
    VAR       ?MAISU  ,STRING*7,VALUE-'0000396'    /.伝票枚数./
    VAR       ?KENSU  ,STRING*7,VALUE-'0000000'    /.出力件数./
/.-----------------------------------------------------------./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMH2  ,STRING*6,VALUE-'WKONCZ'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM2   ,STRING*7,VALUE-'BMSFLIB'     /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?CZKONF   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?CZKONFN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?CZKON1   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?CZKON1N  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?CZKON2   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?CZKON2N  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?WKONCZ   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?WKONCZN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./

    DEFLIBL BMSFLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO/TOKKPLIB/
            TOKDLIB/TOKDTLIB/ONLBLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＢＭＳ送信ＤＴ作成（カインズ店直分）'
    ?SYORINM1 :=  'ＢＭＳ送信ＤＴ作成（カインズ店直分）'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##確認画面##./
STEP010:

    ?OPR1  :=  '　　　　　【流通ＢＭＳ送信データ作成】'
    ?OPR2  :=  '　　　　流通ＢＭＳ送信データを作成します。'
    ?OPR3  :=  '　　　　　（カインズ　店舗直接納品分）　　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL BMSFLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKDTLIB/TOKSOLIB/
            TOKDLIB/ONLBLIB

/.##ログインユーザー情報取得##./
STEP030:

    ?STEP :=   'STEP030'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END
    ?TANCD := ?TANCD8


/.##倉庫ｺｰﾄﾞ取得##./
STEP040:

    ?STEP :=   'STEP040'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?SOKCD && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    /.出荷梱包送信データSF./
    ?FILNMH   :=    ?FILNMH2
    ?FILNM    :=    ?FILNMH && ?SOKCD    /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM2)        /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?WKONCZ   :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?WKONCZN  :=    %STRING(?WKONCZ)
    ?MSGX     :=    '## 送信ＤＴ = ' && ?WKONCZN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌｧｲﾙ排他##./

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN    FILE-?WKONCZ!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=  1020  THEN
       IF     ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              SNDMSG '出荷梱包送信ファイルロック　',TOWS-@ORGWS
              ?KEKA4 := '【送信ファイルロック】'
              GOTO ABEND
       END
    END

    ASSIGN    FILE-CZKEPTNF.BMSFLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=  1020  THEN
       IF     ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              SNDMSG '欠品データファイルロック　　',TOWS-@ORGWS
              ?KEKA4 := '【欠品ファイルロック】'
              GOTO ABEND
       END
    END


/.##出荷梱包送信データ作成##./
STEP080:

    ?STEP :=   'STEP080 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-CZSYKTN6,TOFILE-CZSYKTN6.BMSFLIB
    OVRF FILE-CZKEPTN1,TOFILE-CZKEPTN1.BMSFLIB
    OVRF FILE-BMSHACL3,TOFILE-BMSHACL3.BMSFLIB
    OVRF FILE-CZKONGK1,TOFILE-CZKONGK1.BMSFLIB
    OVRF FILE-WKONCZXX,TOFILE-?WKONCZ
    CALL  NKE1140B.TOKSOLIB,PARA-(?JDATE,?JTIME,?TORICD,?SOKO,?NOUDT,
                                  ?MAISU,?KENSU)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【出荷梱包送信データ作成（店直）】'
                    GOTO ABEND
    END

/. 出荷送信データ複写 ./
STEP090:

    ?STEP :=   'STEP090 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE   FILE-?WKONCZ,
              TOFILE-SYKSNDCZ.ONLBLIB,
              ADD-@YES,BF-3
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '出荷梱包送信データ複写処理'
              GOTO ABEND
    END

/. 欠品データクリア　重複作成防止 ./
STEP100:

    ?STEP :=   'STEP100 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE   FILE-CZKEPTNF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '欠品データクリア処理'
              GOTO ABEND
    END

/.##プログラム正常終了##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '送信データ作成処理が正常終了しました。'
    ?KEKA2 :=  '件数リスト等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB/BMSFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORIKN1 := ?KENSU
    ?SYORINM2 := '出荷送信データ作成処理が正常終了しました。　　'
    ?SYORINM3 := '件数１：出荷件数（カインズ　店舗直接納品分）　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了##./
ABEND:

 /. ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM ./

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '送信データ作成処理が異常終了しました。'
    ?KEKA2 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  ''
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

    RETURN    PGMEC-?PGMEC
