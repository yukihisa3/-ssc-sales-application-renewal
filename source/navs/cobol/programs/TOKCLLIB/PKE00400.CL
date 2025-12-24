/. ***********************************************************  ./
/. *     サカタのタネ　本社システム                          *  ./
/. *   SYSTEM-NAME :    出荷検品システム                     *  ./
/. *   JOB-ID      :    PKE00400                             *  ./
/. *   JOB-NAME    :    出荷データ抽出（手書）　　　         *  ./
/. *                    　　　　　　                         *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMK   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SNDFIN   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMH1  ,STRING*6,VALUE-'SNDTED'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMK1  ,STRING*6,VALUE-'SNDSKK'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKKPLIB'    /.ﾗｲﾌﾞﾗﾘ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILID    ,NAME                         /.ﾌｧｲﾙ名名前型./
    VAR ?LIBID    ,NAME                         /.ﾗｲﾌﾞﾗﾘ名名前型./
    VAR ?SNDDENF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDDENFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDDEKF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDDEKFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?SNDFINF  ,NAME!MOD                     /.ﾌｧｲﾙ.ﾗｲﾌﾞﾗﾘ./
    VAR ?SNDFINFN ,STRING*17                    /.ﾌｧｲﾙ名表示用./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMID    ,STRING*8,VALUE-'PKE00400'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8,VALUE-'        '    /.STEP-ID./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門./
    VAR ?TANCD8   ,STRING*8,VALUE-'        '    /.担当者CD./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?P1       ,STRING*2,VALUE-'00'          /.倉庫(自)./
    VAR ?P2       ,STRING*8,VALUE-'00000000'    /.取引先./
    VAR ?P3       ,STRING*9,VALUE-'000000000'   /.開始伝票./
    VAR ?P4       ,STRING*9,VALUE-'000000000'   /.終了伝票./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?NO       ,STRING*1,VALUE-'1'           /.処理種別./
    VAR ?KENSU    ,STRING*7,VALUE-'0000000'     /.抽出件数./
    VAR ?SYORINM1 ,STRING*50
    VAR ?SYORINM2 ,STRING*40
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
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
              GOTO ABEND
    END
    ?TANCD := ?TANCD8

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P1,?P5)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##手書きデータ抽出指示入力##./
NKE0040I:

    ?STEP :=   'NKE0040I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-SHTDENL6,TOFILE-SHTDENL6.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-SOKKENL1,TOFILE-SOKKENL1.TOKFLIB
    CALL      PGM-NKE0040I.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P5)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              GOTO ABEND
         END
    END
    SNDMSG ?P1,TO-XCTL
    SNDMSG ?P2,TO-XCTL
    SNDMSG ?P3,TO-XCTL
    SNDMSG ?P4,TO-XCTL
    SNDMSG ?P5,TO-XCTL

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?FILNMH := ?FILNMH1
    ?FILNMK := ?FILNMK1
    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?P1 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMH && ?P1        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?SNDDENF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?SNDDENFN :=    %STRING(?SNDDENF)
    ?MSGX     :=    '## 倉庫ﾌｧｲﾙ名 = ' && ?SNDDENFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMK && ?P1        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?SNDDEKF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?SNDDEKFN :=    %STRING(?SNDDEKF)
    ?MSGX     :=    '## 件数ﾌｧｲﾙ名 = ' && ?SNDDEKFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌｧｲﾙ排他##./

    ?STEP :=   'ASSIGN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN    FILE-?SNDDENF!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              SNDMSG '出荷指示データファイルロック',TOWS-@ORGWS
              GOTO ABEND
    END

    ASSIGN    FILE-?SNDDEKF!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              SNDMSG '出荷指示件数　ファイルロック',TOWS-@ORGWS
              GOTO ABEND
    END

/.##手書きデータ抽出##./
NKE0050B:

    ?STEP :=   'NKE0050B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-SNDTEDF,TOFILE-?SNDDENF
    OVRF      FILE-SNDSKKF,TOFILE-?SNDDEKF
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-NKE0050B.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,
                                          ?KENSU)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              GOTO RTN
         ELSE
              GOTO ABEND
         END
    END

/.##データ転送##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-?SNDDENF,PARA-KENPIN,UNIT-3
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
          IF  ?PGMEC    ^=   0    THEN
              GOTO ABEND END
    FIMPORT FILE-?SNDDEKF,PARA-KENPIN,UNIT-4
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
          IF  ?PGMEC    ^=   0    THEN
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
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC

ABEND:

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