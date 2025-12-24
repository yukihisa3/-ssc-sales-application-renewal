/. ***********************************************************  ./
/. *     サカタのタネ　本社システム                          *  ./
/. *   SYSTEM-NAME :    検品システム　　                     *  ./
/. *   JOB-ID      :    PKE02000                             *  ./
/. *   JOB-NAME    :    入荷予定データ抽出　　　　　         *  ./
/. *   UPDATE      :    2019.02.04              　　         *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMK   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SNDFIN   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMH1  ,STRING*6,VALUE-'SNDNYK'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMH2  ,STRING*6,VALUE-'SNDNKK'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMH3  ,STRING*6,VALUE-'WRKNYK'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMH4  ,STRING*6,VALUE-'SRTNYK'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKKPLIB'    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?SNDNYK   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDNYKN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDNKK   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDNKKN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SRTNYK   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SRTNYKN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?WRKNYK   ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?WRKNYKN  ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PKE02000'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門CD./
    VAR ?TANCD8   ,STRING*8,VALUE-'        '    /.担当者CD./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?JISOKO   ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?DSOKO    ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?SIJISOKO ,STRING*2,VALUE-'00'          /.指示倉庫  ./
    VAR ?NO       ,STRING*1,VALUE-'3'           /.処理種別./
    VAR ?KENSU    ,STRING*7,VALUE-'0000000'     /.抽出件数./
    VAR ?KENSU1   ,STRING*7,VALUE-'0000000'     /.抽出件数./
    VAR ?ERRSU    ,STRING*7,VALUE-'0000000'     /.重複件数./
    VAR ?SYORINM1 ,STRING*50
    VAR ?SYORINM2 ,STRING*40
    VAR ?HACKBN   ,STRING*1,VALUE-' '           /.発注区分./
    VAR ?FDATE    ,STRING*8,VALUE-'00000000'    /.開始日付./
    VAR ?TDATE    ,STRING*8,VALUE-'00000000'    /.終了日付./
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
    DEFLIBL   TOKELIB/TOKFLIB/TOKKLIB/TOKDLIB/TOKDTLIB/TOKKPLIB

/.##ログインユーザー情報取得##./
STEP030:

    ?STEP :=   'STEP030'
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
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【倉庫コード取得】'
              GOTO ABEND
    END

/.##入荷予定データ抽出指示入力##./
NKE0200I:

    ?STEP :=   'NKE0200I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-NKE0200I.TOKSOLIB,PARA-(?BUMON,?TANCD,?JISOKO,?DSOKO,
                                          ?SIJISOKO,?HACKBN,
                                          ?FDATE,?TDATE)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 :=  '【入荷予定データ抽出指示入力】'
              GOTO ABEND
         END
    END

    ?MSGX  :=  '?BUMON   =' && ?BUMON
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?TANCD   =' && ?TANCD
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?JISOKO  =' && ?JISOKO
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?DSOKO   =' && ?DSOKO
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?SIJISOKO=' && ?SIJISOKO
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?HACKBN  =' && ?HACKBN
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?FDATE   =' && ?FDATE
     SNDMSG ?MSGX,TO-XCTL
    ?MSGX  :=  '?TDATE   =' && ?TDATE
     SNDMSG ?MSGX,TO-XCTL

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?FILNMH  :=   ?FILNMH1
    ?MSGX    :=   '## 実行倉庫ｺｰﾄﾞ = ' && ?SIJISOKO && ' ##'
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM   :=   ?FILNMH && ?SIJISOKO  /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID   :=   %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID   :=   %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?SNDNYK  :=   %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?SNDNYKN :=   %STRING(?SNDNYK)
    ?MSGX    :=   '## 倉庫ﾌｧｲﾙ名 = ' && ?SNDNYKN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNMK  :=   ?FILNMH2
    ?FILNM   :=   ?FILNMK  && ?SIJISOKO  /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID   :=   %NAME(?FILNM)          /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID   :=   %NAME(?LIBNM)          /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?SNDNKK  :=   %NCAT(?FILID,?LIBID)   /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?SNDNKKN :=   %STRING(?SNDNKK)
    ?MSGX    :=   '## 倉庫ﾌｧｲﾙ名 = ' && ?SNDNKKN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNMK  :=   ?FILNMH3
    ?FILNM   :=   ?FILNMK  && ?SIJISOKO  /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID   :=   %NAME(?FILNM)          /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID   :=   %NAME(?LIBNM)          /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?WRKNYK  :=   %NCAT(?FILID,?LIBID)   /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?WRKNYKN :=   %STRING(?WRKNYK)
    ?MSGX    :=   '## 倉庫ﾌｧｲﾙ名 = ' && ?WRKNYKN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNMK  :=   ?FILNMH4
    ?FILNM   :=   ?FILNMK  && ?SIJISOKO  /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID   :=   %NAME(?FILNM)          /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID   :=   %NAME(?LIBNM)          /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?SRTNYK  :=   %NCAT(?FILID,?LIBID)   /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?SRTNYKN :=   %STRING(?SRTNYK)
    ?MSGX    :=   '## 倉庫ﾌｧｲﾙ名 = ' && ?SRTNYKN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌｧｲﾙ排他 & クリア##./

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN    FILE-?SNDNYK!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              SNDMSG '入荷指示データファイルロック',TOWS-@ORGWS
              ?KEKA4 :=  '【入荷指示データ排他制御異常】'
              GOTO ABEND
    END

    ASSIGN    FILE-?SNDNKK!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              SNDMSG '入荷指示件数　ファイルロック',TOWS-@ORGWS
              ?KEKA4 :=  '【入荷指示件数　排他制御異常】'
              GOTO ABEND
    END

    CLRFILE   FILE-?SNDNYK
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG 'ファイルクリアに失敗！',TOWS-@ORGWS
              SNDMSG '　入荷指示件数ファイル',TOWS-@ORGWS
              ?KEKA4 :=  '【入荷指示件数Ｆ初期化　異常】'
              GOTO ABEND
    END

    DLTFILE   FILE-?WRKNYK
    CPYFILE   FILE-?SNDNYK,TOFILE-?WRKNYK,CRTFILE-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG 'ワークファイル確保失敗！',TOWS-@ORGWS
              SNDMSG '　　入荷予定ワーク　　　',TOWS-@ORGWS
              ?KEKA4 :=  '【ワーク確保異常　入荷予定Ｗ】'
              GOTO ABEND
    END

    DLTFILE   FILE-?SRTNYK
    CPYFILE   FILE-?SNDNYK,TOFILE-?SRTNYK,CRTFILE-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG 'ワークファイル確保失敗！',TOWS-@ORGWS
              SNDMSG '　　ソート用ワーク　　　',TOWS-@ORGWS
              ?KEKA4 :=  '【ワーク確保異常　ソートＷＫ】'
              GOTO ABEND
    END

/.##入荷指示データ抽出(メーカー発注)##./
NKE0210B:

    /. 発注区分＝'2'(社内発注)の場合はスキップ./
    IF        ?HACKBN    =   '2'  THEN
              GOTO   NKE0220B
    END

    ?STEP :=   'NKE0210B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HACHEDL2,TOFILE-HACHEDL2.TOKFLIB
    OVRF      FILE-HACMEIL1,TOFILE-HACMEIL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SNDNYKXX,TOFILE-?SNDNYK
    OVRF      FILE-SNDNKKXX,TOFILE-?SNDNKK
    CALL      PGM-NKE0210B.TOKSOLIB,PARA-(?KENSU,?BUMON,?TANCD,
                                          ?SIJISOKO,?FDATE,?TDATE,
                                          ?KENSU1)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC     =   4010 THEN
              GOTO RTN   END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【入荷指示データ抽出（メーカー発注）】'
              GOTO ABEND END

    ?KENSU    :=   ?KENSU1
    ?MSGX     :=   '?KENSU=' && ?KENSU

/.##入荷指示データ抽出(社内発注)##./
NKE0220B:

    /. 発注区分＝'1'(メーカー発注)の場合はスキップ./
    IF        ?HACKBN    =   '1'  THEN
              GOTO   SORT
    END

    ?STEP :=   'NKE0220B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SFRHEDL2,TOFILE-SFRHEDL2.TOKDLIB
    OVRF      FILE-SNDNYKXX,TOFILE-?SNDNYK
    OVRF      FILE-SNDNKKXX,TOFILE-?SNDNKK
    CALL      PGM-NKE0220B.TOKSOLIB,PARA-(?KENSU,?BUMON,?TANCD,
                                          ?SIJISOKO,?FDATE,?TDATE,
                                          ?KENSU1)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC     =   4010 THEN
              GOTO RTN   END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【入荷指示データ抽出（社内発注）】'
              GOTO ABEND END

    ?KENSU    :=   ?KENSU1
    ?MSGX     :=   '?KENSU=' && ?KENSU
    SNDMSG    ?MSGX,TO-XCTL

SORT:

    ?STEP :=   'SORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTD   INFILE-?SNDNYK,
            INRL-85,
            INBF-48,
            OUTFILE-?WRKNYK,
            OUTRL-85,
            OUTBF-48,
            WORKFILE-?SRTNYK,
            KEY-1!7!DA,      /. _発注伝票_ ./
            KEY1-33!13!CA,   /. _ＪＡＮＣＤ ./
            KEY2-9!3!DA,     /. _行_       ./
         /. RCD-50, ./
            SEQ-@YES,
            RCDL-@DSP
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ＳＯＲＴ処理】'
              GOTO ABEND
    END

/.##入荷予定データ重複排除##./
NKE0225B:

    ?STEP :=   'NKE0225B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-WRKNYKXX,TOFILE-?WRKNYK
    OVRF      FILE-SNDNYKXX,TOFILE-?SNDNYK
    OVRF      FILE-SNDNKKXX,TOFILE-?SNDNKK
    CALL      PGM-NKE0225B.TOKSOLIB,PARA-(?KENSU,?ERRSU)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC     =   4010 THEN
              GOTO RTN   END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【入荷予定データ重複排除】'
              GOTO ABEND END

    ?KENSU    :=   ?KENSU1
    ?MSGX     :=   '?KENSU=' && ?KENSU
    SNDMSG    ?MSGX,TO-XCTL

/.##データ転送##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX     :=    '入荷指示　転送'
    SNDMSG    ?MSGX,TO-XCTL
    FIMPORT FILE-?SNDNYK,PARA-KENPIN,UNIT-6
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
          IF  ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【入荷指示データ転送】'
              GOTO ABEND END

    ?MSGX     :=    '指示件数　転送'
    SNDMSG    ?MSGX,TO-XCTL
    FIMPORT FILE-?SNDNKK,PARA-KENPIN,UNIT-7
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
          IF  ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【入荷指示指示件数転送】'
              GOTO ABEND END

/.##連携件数リスト##./
NKE0900L:

    ?STEP :=   'NKE0900L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-NKE0900L.TOKSOLIB,PARA-(?BUMON,?TANCD,?NO,?KENSU)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【連携件数リスト】'
              GOTO ABEND END

RTN:

    IF        ?ERRSU    ^=   '0000000'    THEN
              SNDMSG    ']]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]',
                                                      TO-XCTL
              SNDMSG    '　入荷予定データに重複があります　',
                                                      TO-XCTL
              SNDMSG    '　確認してください　　　　　　　　',
                                                      TO-XCTL
              SNDMSG    ']]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]',
                                                      TO-XCTL
    END

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
 /. ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM  ./
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