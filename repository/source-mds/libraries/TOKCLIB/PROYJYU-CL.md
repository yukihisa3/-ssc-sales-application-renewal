# PROYJYU

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PROYJYU.CL`

## ソースコード

```jcl
/. ************************************************************ ./
/. *     サカタのタネ　特販システム                           * ./
/. *     オンライン手動受信システム　                         * ./
/. *     取引先：ロイヤルホームセンター（発注）               * ./
/. *     作成者：NAV T.TAKAHASHI                              * ./
/. *     作成日：02/12/18                     ID:PROYJYU      * ./
/. ************************************************************ ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMECY   ,STRING*4
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*13
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PROYJYU '
    VAR       ?STEP     ,STRING*8
/.ライブラリリストの登録                                        ./
    DEFLIBL CVCSLBI/CVCSOBJ
    SNDMSG MSG-'## 受信開始 ##',TO-XCTL.@ORGPROF
/.##ＶＬＤの活性化～データ受信##./
KENJYUSN:
           OVRVLDF FILE-LD901,TOFILE-LD901.CVCSLBI
           CALL CVCS1001.CVCSOBJ,
           PARA-('01AA8F100203G')
           ?PGMEC            :=        @PGMEC
           ?PGMECX           :=        %STRING(?PGMEC)
           ?PGMECY           :=        %SBSTR(?PGMECX,8,4)
           /.## 4095の時、ﾘﾀﾞｲﾔﾙ待機中                  ##./
           /.## 4020の時、送信ﾌｧｲﾙ無し                  ##./
           /.## 4095,4020以外は、受信ｴﾗｰとする          ##./
           IF  ?PGMECY  =  '4095'  THEN
               GOTO   KENJYUSN
           ELSE
               IF  ?PGMECY  =  '4020'  THEN
                   SNDMSG MSG-'## 受領送信F無 ##',TO-XCTL.@ORGPROF
               ELSE
                   IF  ?PGMECY  ^=  '0000'  THEN
                       GOTO   ABENDK
                   END
               END
           END
/.## GP6000へﾃﾞｰﾀ転送 ##./
GP6000CP:
    ?STEP :=   'GP6000CP'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CPYFILE FILE-CVCSG001.CVCSLBI,TOFILE-ROYAL.ONLBLIB,
            CRTFILE-@YES

/.##時間待ち合わせ##./
KWAIT:
    CALL   TWAIT3
/.##集信データ累積ファイルから集信データを抽出する##./
KENTYUSY:
    ?STEP :=   'KENTYUSY'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   CVCSOBJ/CVCSLBI
    OVRF      FILE-CVCSS005,TOFILE-CVCSS005.CVCSLBI
    OVRF      FILE-CVCSL004,TOFILE-CVCSL004.CVCSLBI
    OVRF      FILE-CVCSCARD,TOFILE-ROYJYU.CVCSLBI
    CALL      PGM-CVCS1003.CVCSOBJ
    IF        @PGMEC    ^=   0    THEN GOTO ABENDK END
/.##運用状況リスト出力##./
LSTOUT:
    OVRF      FILE-CVCSCARD,TOFILE-CARD0010.CVCSLBI
    CALL      CLI1010.CVCSLBI
OWARI:

    SNDMSG MSG-'## 受信終了 ##',TO-XCTL.@ORGPROF
    RETURN    PGMEC-@PGMEC
ABENDK:

    SNDMSG MSG-'## 受信異常 ##',TO-XCTL.@ORGPROF
    DSPLOG    OUTPUT-@LIST,EDT-@JEF
    RETURN    PGMEC-@PGMEC

```
