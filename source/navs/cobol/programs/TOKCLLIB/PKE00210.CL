/. ***********************************************************  ./
/. *     サカタのタネ　本社システム                          *  ./
/. *   SYSTEM-NAME :    出荷検品システム                     *  ./
/. *   JOB-ID      :    PKE00210                             *  ./
/. *   JOB-NAME    :    出荷データ抽出（オンライン）         *  ./
/. *                    アレンザＨＤ                         *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '    /.ﾌｧｲﾙ名合併用./
    VAR ?FILNMH   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMK   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?SNDFIN   ,STRING*6,VALUE-'      '      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
    VAR ?FILNMH1  ,STRING*6,VALUE-'SNDSKD'      /.ﾌｧｲﾙ名ﾃﾞﾌｫﾙﾄ./
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
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PKE00210'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門CD./
    VAR ?TANCD8   ,STRING*8,VALUE-'        '    /.担当者CD./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?P6       ,STRING*8,VALUE-'00000000'    /.納品日　　./
/.  VAR ?P7       ,STRING*1,VALUE-'0'   ./      /.出力区分./
    VAR ?P8       ,STRING*1,VALUE-'0'           /.部門付加./
    VAR ?NO       ,STRING*1,VALUE-'1'           /.処理種別./
    VAR ?KENSU    ,STRING*7,VALUE-'0000000'     /.抽出件数./
    VAR ?SYORINM1 ,STRING*50
    VAR ?SYORINM2 ,STRING*40
    VAR ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN5 ,STRING*7,VALUE-'0000000'
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKKLIB/TOKDLIB/TOKDTLIB/TOKKPLIB/
              TOKWLIB

    ?PGNM    :=  '出荷データ抽出（オンライン）'

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
              ?KEKA4 :=  'ログインユーザー情報取得'
              GOTO ABEND
    END
    ?TANCD := ?TANCD8

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P4,?P5)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '倉庫ｺｰﾄﾞ取得'
              GOTO ABEND
    END

/.##オンラインデータ抽出指示入力##./
NKE0010I:

    ?STEP :=   'NKE0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-NKE0010I.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P5,?P6,
                                          ?P8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 :=  'オンラインデータ抽出指示入力'
              GOTO ABEND
         END
    END
    SNDMSG ?P1,TO-XCTL
    SNDMSG ?P2,TO-XCTL
    SNDMSG ?P3,TO-XCTL
    SNDMSG ?P4,TO-XCTL
    SNDMSG ?P5,TO-XCTL
    SNDMSG ?P6,TO-XCTL
    SNDMSG ?P8,TO-XCTL

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?FILNMH := ?FILNMH1
    ?FILNMK := ?FILNMK1
    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?P4 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMH && ?P4        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
    ?FILID    :=    %NAME(?FILNM)         /.ﾌｧｲﾙ名名前型変換   ./
    ?LIBID    :=    %NAME(?LIBNM)         /.ﾗｲﾌﾞﾗﾘ名名前型変換 ./
    ?SNDDENF  :=    %NCAT(?FILID,?LIBID)  /.ﾌｧｲﾙ名.ﾗｲﾌﾞﾗﾘ名    ./
    ?SNDDENFN :=    %STRING(?SNDDENF)
    ?MSGX     :=    '## 倉庫ﾌｧｲﾙ名 = ' && ?SNDDENFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMK && ?P4        /.ﾃﾞﾌｫﾙﾄﾌｧｲﾙ名+倉庫CD./
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
              ?KEKA4 :=  'ファイル排他１'
              GOTO ABEND
    END

    ASSIGN    FILE-?SNDDEKF!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              SNDMSG '他で連携処理が行われています',TOWS-@ORGWS
              SNDMSG '出荷指示件数　ファイルロック',TOWS-@ORGWS
              ?KEKA4 :=  'ファイル排他２'
              GOTO ABEND
    END

/.##売上伝票データ抽出（アレンザ）##./
SKE5013B:

    ?STEP :=   'SKE5013B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-ARZDENWK,TOFILE-ARZDENWK.TOKWLIB
    CALL      PGM-SKE5013B.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P6)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '売上伝票データ抽出'
              GOTO ABEND END

/.##売上伝票データ集計（アレンザ）##./
SKE5014B:

    ?STEP :=   'SKE5014B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ARZDENW1,TOFILE-ARZDENW1.TOKWLIB
    OVRF      FILE-ARZSYUWK,TOFILE-ARZSYUWK.TOKWLIB
    CALL      PGM-SKE5014B.TOKSOLIB

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '売上伝票データ抽出'
              GOTO ABEND END

/.##オンラインデータ抽出（アレンザ）##./
NKE0020B:

    ?STEP :=   'NKE0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-ARZSYUW1.TOKWLIB
    OVRF      FILE-SNDSKDF,TOFILE-?SNDDENF
    OVRF      FILE-SNDSKKF,TOFILE-?SNDDEKF
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-NKE0020B.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P6,?P8,
                                          ?KENSU)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '売上伝票データ抽出'
              GOTO ABEND END

/.##データ転送##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-?SNDDENF,PARA-KENPIN,UNIT-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
          IF  ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'データ転送１'
              GOTO ABEND END
    FIMPORT FILE-?SNDDEKF,PARA-KENPIN,UNIT-2
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
          IF  ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'データ転送２'
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
              ?KEKA4 :=  '連携件数リスト'
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC

ABEND:

    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
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