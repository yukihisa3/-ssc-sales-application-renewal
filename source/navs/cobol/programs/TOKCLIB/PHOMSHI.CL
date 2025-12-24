/. ************************************************************ ./
/. *     サカタのタネ　特販システム                           * ./
/. *     オンライン手動受信システム　                         * ./
/. *     取引先：ホーマック関東                               * ./
/. *     作成者：NAV T.TAKAHASHI                              * ./
/. *     作成日：00/07/28                     ID:PHOMSHI      * ./
/. ************************************************************ ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMECY   ,STRING*4
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*13
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PHOMSHI '
    VAR       ?STEP     ,STRING*8
/.ライブラリリストの登録                                        ./
    DEFLIBL CVCSLBT/CVCSOBJ
    SNDMSG MSG-'## 受信開始 ##',TO-XCTL.@ORGPROF
/.ＶＬＤの活性化～データ受信                                    ./
SIJYUSN:
           OVRVLDF FILE-LD901,TOFILE-LD801.CVCSLBT
           CALL CVCS1001.CVCSOBJ,
           PARA-('01AA39840018C')
           ?PGMEC            :=        @PGMEC
           ?PGMECX           :=        %STRING(?PGMEC)
           ?PGMECY           :=        %SBSTR(?PGMECX,8,4)
           IF    ?PGMECY     =         '4095'
                 THEN                  GOTO   SIJYUSN
           ELSE
                 IF ?PGMECY  ^=        '0000'
                     THEN              GOTO   ABENDK
                 END
           END

/.ホーマック支払データＧＰ送信                                  ./
PSNDFILE:
    ?STEP :=   'PSNDFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDFILE COM-PCPATH02.PCNODE02,SFILE-CVCSG001.CVCSLBT,
            DFILE-'HOMSHI',RL-128,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABENDK END

/.ホーマック支払データ編集ジョブ起動                            ./
PSNDRSJJ:
    ?STEP :=   'PSNDRSJJ'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDRSJJ COM-PCPATH02.PCNODE02,DFILE-'HORMAC'
    IF        @PGMEC    ^=   0    THEN
              GOTO ABENDK END

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
    OVRF      FILE-CVCSCARD,TOFILE-HOMSHI.CVCSLBT
    CALL      PGM-CVCS1003.CVCSOBJ
    IF        @PGMEC    ^=   0    THEN GOTO ABENDK END
/.  運用状況リスト出力                                         ./
LSTOUT:
    OVRF      FILE-CVCSCARD,TOFILE-CARD0010.CVCSLBT
    CALL      CLT1010.CVCSLBT
OWARI:

    SNDMSG MSG-'## 受信終了 ##',TO-XCTL.@ORGPROF
    RETURN    PGMEC-@PGMEC
ABENDK:

    SNDMSG MSG-'## 受信異常 ##',TO-XCTL.@ORGPROF
    DSPLOG    OUTPUT-@LIST,EDT-@JEF
    RETURN    PGMEC-@PGMEC
