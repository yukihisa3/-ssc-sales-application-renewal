/. ***********************************************************  ./
/. *     サカタのタネ　営業第２部　ＮＡＶＳ　　　　          *  ./
/. *   SYSTEM-NAME :    ＨＨＴ_卸機能　　　　　　　　　　   *  ./
/. *   JOB-ID      :    PTN00800                             *  ./
/. *   JOB-NAME    :    _卸基幹連携データ取込　　　　　　   *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PTN00800'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*2,VALUE-'  '          /.倉庫ＣＤ  ./
    VAR ?P2       ,STRING*2,VALUE-'  '          /.倉庫(代表)./
    VAR ?P3       ,STRING*1,VALUE-' '           /.出力区分./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(開始)./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(終了)./
    VAR ?P6       ,STRING*1,VALUE-'1'           /.帳票巣別./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                    /.      2    ./
    VAR ?OPR3     ,STRING*50                    /.      3    ./
    VAR ?OPR4     ,STRING*50                    /.      4    ./
    VAR ?OPR5     ,STRING*50                    /.      5    ./
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?FILNM1   ,STRING*6,VALUE-'SKTNSF'
    VAR ?FILNM2   ,STRING*6,VALUE-'SKTANA'
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKDTLIB'
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?SKTNSFF  ,NAME!MOD
    VAR ?SKTNSFFN ,STRING*17
    VAR ?SKTANAF  ,NAME!MOD
    VAR ?SKTANAFN ,STRING*17
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '_卸基幹連携データ取込'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKMDLIB/ZAIEXCEL/TOKFLIB/TOKKLIB
           /TOKDTLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P1,?P2)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行倉庫ＣＤ = ' && ?P1 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNM1 && ?P1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SKTNSFF  :=    %NCAT(?FILID,?LIBID)
    ?SKTNSFFN :=    %STRING(?SKTNSFF)
    ?MSGX     :=    '## 倉庫側_卸Ｆ  = ' && ?SKTNSFFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?FILNM2 && ?P1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SKTANAF  :=    %NCAT(?FILID,?LIBID)
    ?SKTANAFN :=    %STRING(?SKTANAF)
    ?MSGX     :=    '## 倉庫_卸取込F = '  && ?SKTANAFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##_卸関係データ取得確認##./
PPAUSE:

    ?STEP :=   'PPAUSE  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＜_卸基幹連携データ取込＞＃＃'
    ?OPR2  :=  '　倉庫にて確定された_卸基幹連携データ'
    ?OPR3  :=  '　取込を行ないます。'
    ?OPR4  :=  '　確認後、実行して下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##HHT_卸基幹連携データ転送##./
PFEXPORT:

    ?STEP :=   'PFEXPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-?SKTNSFF,MODE-@REP,PARA-HHTTANA,UNIT-4
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸基幹連携データ転送'
              GOTO ABEND END

/.##取込データセット##./
PSETPF:

    ?STEP :=   'PSETUP  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-?SKTNSFF,TOFILE-?SKTANAF,ADD-@NO,ACTCHK-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '取込データセット'
              GOTO ABEND END

/.##倉庫_卸結果データ取込##./
STN0080B:

    ?STEP :=   'STN0080B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-SKTANAXX,TOFILE-?SKTANAF
    CALL      PGM-STN0080B.TOKSOLIB,PARA-(?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '_卸予定ＣＳＶデータ転送'
              GOTO ABEND END

/.##倉庫側データ初期化##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##FIMPORT FILE-HHTZEROF.TOKDTLIB,PARA-HHTTANA,UNIT-5
##./?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '倉庫側データ初期化'
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '＜_卸基幹連携データ取込＞'
    ?KEKA2 :=  '_卸基幹連携データ取込が正常終了'
    ?KEKA3 :=  'しました。作成結果の確認をお願い致し'
    ?KEKA4 :=  'ます。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '＜_卸基幹連携データ取込＞'
    ?KEKA2 :=  '処理が異常終了しました。ＬＯＧを採取し'
    ?KEKA3 :=  'ＮＡＶへ連絡して下さい'
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

    RETURN    PGMEC-@PGMEC
