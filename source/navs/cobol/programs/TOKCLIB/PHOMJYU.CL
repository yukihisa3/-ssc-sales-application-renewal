/. ************************************************************ ./
/. *     サカタのタネ　特販システム                           * ./
/. *     オンライン手動受信システム　                         * ./
/. *     取引先：ホーマック関東（受領データ）                 * ./
/. *     作成者：NAV T.TAKAHASHI                              * ./
/. *     作成日：00/08/21                     ID:PHOMJYU      * ./
/. ************************************************************ ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMECY   ,STRING*4
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*13
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PHOMJYU '
    VAR       ?STEP     ,STRING*8
/.ライブラリリストの登録                                        ./
    DEFLIBL CVCSLBT/CVCSOBJ
    SNDMSG MSG-'## 受信開始 ##',TO-XCTL.@PRGPROF
/.ＶＬＤの活性化～データ受信                                    ./
KENJYUSN:
           OVRVLDF FILE-LD901,TOFILE-LD801.CVCSLBT
           CALL CVCS1001.CVCSOBJ,
           PARA-('01AA39840002F')
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
    SNDFILE COM-KGPATH01.KGNODE01,SFILE-CVCSG001.CVCSLBT,
            DFILE-'HOMJYU',RL-128,BF-1
/.## GP6000-JOB起動   ##./
    SNDRSJJ COM-KGPATH01.KGNODE01,DFILE-'HOMACJYU'
/.  時間待ち合わせ                                              ./
KWAIT:
    CALL   TWAIT3
/.  集信データ累積ファイルから集信データを抽出する              ./
KENTYUSY:
    ?STEP :=   'KENTYUSY'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   CVCSOBJ/CVCSLBT
    OVRF      FILE-CVCSS005,TOFILE-CVCSS005.CVCSLBT
    OVRF      FILE-CVCSL004,TOFILE-CVCSL004.CVCSLBT
    OVRF      FILE-CVCSCARD,TOFILE-HOMJYU.CVCSLBT
    CALL      PGM-CVCS1003.CVCSOBJ
    IF        @PGMEC    ^=   0    THEN GOTO ABENDK END
/.  運用状況リスト出力                                         ./
LSTOUT:
    OVRF      FILE-CVCSCARD,TOFILE-CARD0010.CVCSLBT
    CALL      CLT1010.CVCSLBT
OWARI:

    SNDMSG MSG-'## 受信終了 ##',TO-XCTL.@PRGPROF
    RETURN    PGMEC-@PGMEC
ABENDK:

    SNDMSG MSG-'## 受信異常 ##',TO-XCTL.@PRGPROF
    DSPLOG    OUTPUT-@LIST,EDT-@JEF
    RETURN    PGMEC-@PGMEC
