# CVCS11H2

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/CVCS11H2.CL`

## ソースコード

```jcl
/. ************************************************************ ./
/. *     サカタのタネ　特販システム                           * ./
/. *     オンライン手動受信システム　                         * ./
/. *     取引先：ホーマック関東送信                          * ./
/. *     作成者：NAV T.TAKAHASHI                              * ./
/. *     作成日：02/12/18                     ID:CVCS11H2      * ./
/. ************************************************************ ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMECY   ,STRING*4
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*13
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'CVCS11H2'
    VAR       ?STEP     ,STRING*8
/.ライブラリリストの登録                                        ./
    DEFLIBL CVCSLBT/CVCSOBJ
    SNDMSG MSG-'## 送信開始 ##',TO-XCTL.@ORGPROF
/.##ＶＬＤの活性化～データ受信##./
KENJYUSN:
           OVRVLDF FILE-LD901,TOFILE-LD801.CVCSLBT
           CALL CVCS1001.CVCSOBJ,
           PARA-('01BB39840017E')
           ?PGMEC            :=        @PGMEC
           ?PGMECX           :=        %STRING(?PGMEC)
           ?PGMECY           :=        %SBSTR(?PGMECX,8,4)
           IF  ?PGMECY  ^=  '0000'  THEN
               GOTO   ABENDK
           END

OWARI:

    SNDMSG MSG-'## 送信終了 ##',TO-XCTL.@ORGPROF
    RETURN    PGMEC-@PGMEC

ABENDK:

    SNDMSG MSG-'## 送信異常 ##',TO-XCTL.@ORGPROF
    DSPLOG    OUTPUT-@LIST,EDT-@JEF
    RETURN    PGMEC-@PGMEC

```
