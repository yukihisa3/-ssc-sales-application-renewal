# PJU2020T

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PJU2020T.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :  受配信管理システム                     *  ./
/. *   JOB-ID      :  PJU20200                               *  ./
/. *   JOB-NAME    :  ２５６バイトファイル　バックアップ     *  ./
/. ***********************************************************  ./
/.  PGM  (P1-?JDATE,P2-?JTIME,P3-?TORICD,P4-?JHFNM,P5-?LIBNMP,
          P6-?PARAKEKA)
./  PGM
/.##パラメタ定義##./
    VAR  ?JDATE   ,STRING*8,VALUE-'20101102' /.受信日./
    VAR  ?JTIME   ,STRING*4,VALUE-'1830'     /.受信時間./
    VAR  ?TORICD  ,STRING*8,VALUE-'00926061' /.取引先CD./
    VAR  ?JHFNM   ,STRING*8,VALUE-'ONLSEKIH' /.受配信ファイル名./
    VAR  ?LIBNMP  ,STRING*8,VALUE-'ONLBLIB ' /.ﾗｲﾌﾞﾗﾘ名./
    VAR  ?PARAKEKA,STRING*4,VALUE-'0000'    /.ｴﾗｰ結果./
/.##ワーク定義##./
    VAR  ?PGMEC    ,INTEGER                     /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX   ,STRING*11                   /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM    ,STRING*99                   /.ﾘﾀｰﾝ名称./
    VAR  ?MSG      ,STRING*99(6)                /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSGX     ,STRING*99                   /.ﾒｯｾｰｼﾞ表示用./
    VAR  ?PGMID    ,STRING*8,VALUE-'PJU20200'   /.PROGRAM-ID./
    VAR  ?STEP     ,STRING*8                    /.STEP-ID./
    VAR  ?SNDMSGR  ,STRING*20                   /.ﾒｯｾｰｼﾞ表示用./
    VAR  ?REBAK    ,INTEGER                     /.ﾊﾞｯｸｱｯﾌﾟﾏﾁ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR  ?FILN     ,NAME                        /.ﾌｧｲﾙ名前型./
    VAR  ?LIBNM    ,NAME                        /.ﾗｲﾌﾞﾗﾘ名./
    VAR  ?JFILNM   ,NAME!MOD                    /.受信ﾌｧｲﾙ名./
    VAR  ?FILID    ,STRING*17                   /.ﾌｧｲﾙ名表示用./

    ACTMSGQ MAXRMSG-50

/.##プログラム開始メッセージ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊　受信データバックアップ開始　＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKJLIB

/.##ｴﾗｰ判断##./
FILENAME:

    ?STEP := 'FILENAME'
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG  ?MSGX,TO-XCTL

/.## 受配信ファイル名編集 ##./
    ?FILN   := %NAME(?JHFNM)
    ?LIBNM  := %NAME(?LIBNMP)
    ?JFILNM := %NCAT(?FILN,?LIBNM)
    ?FILID  := %STRING(?JFILNM)
    ?MSGX   := '***受信ﾌｧｲﾙ名 = ' && ?FILID && ' ##'
    SNDMSG  ?MSGX,TO-XCTL

/.##資源獲得##./
PASSIGN:

    ?STEP :=   'PASSIGN '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'**資源獲得',TO-XCTL

    ASSIGN FILE-BACK0256.ONLBLIB!@XCL
    IF   @PGMEC    ^=   0    THEN
         IF ?PGMEC = 4095 THEN
            IF ?REBAK < 5  THEN
               ?REBAK := ?REBAK + 1
               ?MSGX := '## 他JOBで資源使用中です BACK0256 ##'
               SNDMSG   ?MSGX,TO-XCTL
               /.### ﾊﾞｯｸｱｯﾌﾟ ﾏﾁｱﾜｾ ###./
               CALL TWAIT30.XUCL
               GOTO   PASSIGN
            ELSE
               ?PARAKEKA  :=  'K532'
               GOTO   ABEND
            END
         ELSE
           ?PARAKEKA  :=  'K549'
           GOTO   ABEND
         END
    END

/.##受信ファイルバックアップ##./
DATABACK:

    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｭｼﾝF BAK 256 START')
    ?STEP := 'DATABACK'
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSGX,TO-XCTL

    OVRF FILE-CVCSG002,TOFILE-?JFILNM
    OVRF FILE-BKCVCS02,TOFILE-BACK0256.ONLBLIB
    CALL SJU2020B.TOKELIB,PARA-(?JDATE,?JTIME,?TORICD)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0 THEN
        ?PARAKEKA  :=  'K550'
        GOTO        ABEND
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    RELEASE LIB-ONLBLIB!@XCL
    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｭｼﾝF BAK 256 END  ')
    ?SNDMSGR := ' ﾌｧｲﾙ名 = ' && ?JHFNM
    CALL SCVMSG.TOKCLIBO,PARA-(?SNDMSGR)
    ?MSGX := '***   ' && ?PGMID && ' END    ***'
    SNDMSG ?MSGX,TO-XCTL
    RETURN PGMEC-@PGMEC

/.##ﾌﾟﾛｸﾞﾗﾑ異常終了##./
ABEND:

    RELEASE LIB-ONLBLIB!@XCL
    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｭｼﾝF BAK 256 ABEND')
    ?PGMECX := %STRING(?PGMEC)
    ?MSG(1) := '### ' && ?PGMID && ' ABEND ' && ' ###'
    ?MSG(2) := '### PGMEC = ' &&
               %SBSTR(?PGMECX,8,4) && '   ###'
    ?MSG(3) := '### LINE  = ' && %LAST(LINE) && '   ###'
    FOR ?I := 1 TO 3
      DO ?MSGX := ?MSG(?I)
         SNDMSG ?MSGX,TO-XCTL
    END

    SNDMSG ?PGMEM,TO-XCTL
    RETURN PGMEC-@PGMEC

```
