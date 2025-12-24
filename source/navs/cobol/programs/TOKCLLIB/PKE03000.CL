/. ***********************************************************  ./
/. *     サカタのタネ　本社システム                          *  ./
/. *   SYSTEM-NAME :    検品システム　　                     *  ./
/. *   JOB-ID      :    PKE03000                             *  ./
/. *   JOB-NAME    :    返品検品データ取込　　　　　         *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?FILHENK1 ,STRING*6,VALUE-'RCVHHD'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILHENK2 ,STRING*6,VALUE-'RCVHHM'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILHENK3 ,STRING*6,VALUE-'RCVHJH'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILHENK4 ,STRING*6,VALUE-'RCVHJM'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKKPLIB'    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?RCVHEP1  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?RCVHEPN1 ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?RCVHEP2  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?RCVHEPN2 ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?RCVHEP3  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?RCVHEPN3 ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?RCVHEP4  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?RCVHEPN4 ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PKE03000'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門CD./
    VAR ?TANCD8   ,STRING*8,VALUE-'        '    /.担当者CD./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?JISOKO   ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?DSOKO    ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?NO       ,STRING*1,VALUE-'5'           /.処理種別./
    VAR ?KENSU    ,STRING*7,VALUE-'0000000'     /.抽出件数./
    VAR ?HAKKBN   ,STRING*1,VALUE-' '           /.発注区分./
    VAR ?PRTKBN   ,STRING*1,VALUE-' '           /.ﾌﾟﾘﾝﾄ区分./
    VAR ?JISKBN   ,STRING*1,VALUE-' '           /.入荷実績区分./
    VAR ?TDATE    ,STRING*8,VALUE-'00000000'    /.取込日付./
    VAR ?TTIME    ,STRING*6,VALUE-'000000'      /.取込時刻./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '（検品）入荷検品データ取込'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKKPLIB/TOKKLIB/TOKDLIB/TOKDTLIB

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ログインユーザー情報取得】'
              GOTO ABEND
    END
    ?TANCD := ?TANCD8

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?JISOKO,?DSOKO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【倉庫コード取得】'
              GOTO ABEND
    END

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX     :=   '## 実行倉庫ｺｰﾄﾞ = ' && ?JISOKO && ' ##'
    SNDMSG    MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=   ?FILHENK1 && ?JISOKO   /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=   %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=   %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVHEP1  :=   %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVHEPN1 :=   %STRING(?RCVHEP1)
    ?MSGX     :=   '## 返品管理名 = ' && ?RCVHEPN1 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=   ?FILHENK2 && ?JISOKO   /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=   %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=   %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVHEP2  :=   %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVHEPN2 :=   %STRING(?RCVHEP2)
    ?MSGX     :=   '## 返品管理Ｍ = ' && ?RCVHEPN2 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=   ?FILHENK3 && ?JISOKO   /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=   %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=   %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVHEP3  :=   %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVHEPN3 :=   %STRING(?RCVHEP3)
    ?MSGX     :=   '## 返品実績Ｈ = ' && ?RCVHEPN3 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=   ?FILHENK4 && ?JISOKO   /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=   %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=   %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?RCVHEP4  :=   %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?RCVHEPN4 :=   %STRING(?RCVHEP4)
    ?MSGX     :=   '## 返品実績Ｍ = ' && ?RCVHEPN4 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##資源獲得##./
PASSIGN:

    ?STEP :=   'PASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN FILE-?RCVHEP1!@XCL/?RCVHEP2!@XCL/?RCVHEP3!@XCL
               /?RCVHEP4!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【資源獲得】'
              GOTO ABEND
    END

/.##ファイル初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-?RCVHEP1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【初期化：.ホスト返品管理ファイル】'
              GOTO ABEND
    END

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-?RCVHEP2
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【初期化：.ホスト返品管理明細Ｆ　】'
              GOTO ABEND
    END

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-?RCVHEP3
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【初期化：.ホスト返品実績ヘッダＦ】'
              GOTO ABEND
    END

    CLRFILE FILE-?RCVHEP4
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【初期化：.ホスト返品実績明細Ｆ　】'
              GOTO ABEND
    END

/.##返品検品データ取込##./
PFEXPORT:

    ?STEP :=   'PFEXPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-?RCVHEP1,MODE-@ADD,PARA-KENPIN,UNIT-13
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【返品管理データ取込】'
              GOTO ABEND
    END

    FEXPORT FILE-?RCVHEP2,MODE-@ADD,PARA-KENPIN,UNIT-14
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【返品管理明細データ取込】'
              GOTO ABEND
    END

    FEXPORT FILE-?RCVHEP3,MODE-@ADD,PARA-KENPIN,UNIT-15
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【返品実績データ取込】'
              GOTO ABEND
    END

    FEXPORT FILE-?RCVHEP4,MODE-@ADD,PARA-KENPIN,UNIT-16
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【返品実績明細データ取込】'
              GOTO ABEND
    END

/.##返品検品データ取込指示入力##./
NKE0300I:

    ?STEP :=   'NKE0300I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-RCVHHDXX,TOFILE-?RCVHEP1
    OVRF FILE-RCVHHMXX,TOFILE-?RCVHEP2
    OVRF FILE-RCVHJHXX,TOFILE-?RCVHEP3
    OVRF FILE-RCVHJMXX,TOFILE-?RCVHEP4
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    CALL PGM-NKE0300I.TOKSOLIB,PARA-(?BUMON,?TANCD,?JISOKO,?HAKKBN,
                                     ?PRTKBN,?TDATE,?TTIME,?KENSU)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 :=  '【返品検品データ取込指示入力】'
              GOTO ABEND
         END
    END

    ?MSGX  :=  '?BUMON   =' && ?BUMON
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?TANCD   =' && ?TANCD
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?JISOKO  =' && ?JISOKO
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?HAKKBN  =' && ?HAKKBN
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?PRTKBN  =' && ?PRTKBN
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?TDATE   =' && ?TDATE
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?TTIME   =' && ?TTIME
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?KENSU   =' && ?KENSU
     SNDMSG ?MSGX,TO-XCTL

/.##返品検品データ取込##./
NKE0310B:

    ?STEP :=   'NKE0310B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-NKE0310B.TOKSOLIB,PARA-(?BUMON,?TANCD,?JISOKO,
                                          ?TDATE,?TTIME)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【返品検品データ取込】'
              GOTO ABEND END

/.##（返品確定）連携件数リスト##./
NKE0900L:

    ?STEP :=   'NKE0900L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-NKE0900L.TOKSOLIB,PARA-(?BUMON,?TANCD,?NO,?KENSU)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【（返品確定）連携件数リスト】'
              GOTO ABEND END

/.##返品検品結果確認リスト発行##./
NKE0330L:

    IF  ?HAKKBN ^= '1'  THEN
         GOTO   RTN
    END

    ?STEP :=   'NKE0330L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-NKE0330L.TOKSOLIB,PARA-(?BUMON,?TANCD,?JISOKO,
                                          ?TDATE,?TDATE)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【倉庫入荷検品リスト発行】'
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '返品検品データ取込が正常終了しました'
    ?KEKA2 :=  '本社発注担当者様に取り込んだ旨、連絡'
    ?KEKA3 :=  'をお願い致します。'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC

ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '返品検品データ取込が異常終了しました'
    ?KEKA2 :=  'ログを採取してＮＡＶへ連絡した下さい'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC
