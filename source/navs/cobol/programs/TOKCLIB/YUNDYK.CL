/. ************************************************************ ./
/. *     サカタのタネ　特販システム                           * ./
/. *     オンライン自動集配信システム                         * ./
/. *     ユニディー　（６３２１）検収　                       * ./
/. *     PROGRAM-ID     :    JYUNDY                           * ./
/. *                    :    1999.06.02 （新基幹システム用）  * ./
/. ************************************************************ ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMECY   ,STRING*4
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*13
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'JYUNDY  '
    VAR       ?STEP     ,STRING*8

/.##ユニディー検収データコピー##./
    CNVFILE FILE-YUNDYK.ONLBLIB,TOFILE-YUNDYKEN.ONLBLIB,ADD-@NO,
            BF-1

/.  データ編集（２５６～１２８変換）                            ./
KENHEN:

    ?STEP :=   'KENHEN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CVCSF,TOFILE-YUNDYK.ONLBLIB
    OVRF      FILE-WORK01,TOFILE-WORKF.ONLBLIB
    CALL      PGM-VDA1100B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABENDK END
/.検収データファイルにデータコピー                              ./
DATACPY:

    OVRF      FILE-WORKF,TOFILE-WORKF.ONLBLIB
    OVRF      FILE-YNKENSYU,TOFILE-YNKENSYU.TOKFLIB
    CALL      PGM-VDA1191B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABENDK END

/.  検収データ受信件数リスト出力                                ./
KENLST:

    OVRF      FILE-YNKENSYU,TOFILE-YNKENSYU.TOKFLIB
    CALL      PGM-VDA1180B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABENDK END

OWARI:
    DEFLIBL   TOKELIB
    CALL      CVCSLST,PARA-(' 6321')
    DEFLIBL   TOKFLIB
/.  CALL      SIMCNT.TOKELIB   ./
    RETURN    PGMEC-@PGMEC
ABENDK:
    DEFLIBL   TOKELIB
    CALL      CVCSLST,PARA-('16322')
    DSPLOG    OUTPUT-@LIST,EDT-@JEF
    RETURN    PGMEC-@PGMEC

